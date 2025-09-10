open Symex
open Soteria_std.Syntaxes.FunctionWrap

(** A [Split_tree] is a simplified representation of a tree, that has no offset.
    It however indicates, on [Node]s, at what offset the split occurs, relative
    to that node's start. *)
module Split_tree = struct
  type ('a, 'sint) t =
    | Leaf of 'a
    | Node of ('a, 'sint) t * 'sint * ('a, 'sint) t
  [@@deriving show]

  let rec map f = function
    | Leaf x -> Leaf (f x)
    | Node (l, at, r) -> Node (map f l, at, map f r)
end

type ('a, 'sint) tree = {
  node : 'a node;
  range : 'sint * 'sint;
  children : (('a, 'sint) tree * ('a, 'sint) tree) option;
}

and 'a node = NotOwned of node_qty | Owned of 'a

and node_qty = Partially | Totally
[@@deriving show { with_path = false }, make]

(** The input module of [Tree_block]. A memory value [t] represents an owned
    part of the tree block, with the property that it can be split or merged as
    needed.

    The merge doesn't need to preserve all information. For instance consider
    the memory values [Value | Uninit | PartlyUninit]; merging nodes [Value] and
    [Uninit] may yield a [PartlyUninit] node, and that node will contain both
    children, ensuring no information is lost.

    Furthermore, to enable bi-abduction, a memory value can potentially be
    serialized into one or more predicates (named [serialized]). Consuming
    should extract that predicate from the given tree; producing adds it onto
    it.

    [serialized] mustn't store information about the offset or length it applies
    to, as [Tree_block] wraps it into a structure containing this information.
*)
module type MemVal = sig
  module Symex : Symex.S

  module SInt : sig
    type +'a t = 'a Symex.Value.t
    type sbool
    type sint

    val v_false : sbool t
    val zero : unit -> sint t
    val ( +@ ) : sint t -> sint t -> sint t
    val ( -@ ) : sint t -> sint t -> sint t
    val ( <@ ) : sint t -> sint t -> sbool t
    val ( <=@ ) : sint t -> sint t -> sbool t
    val ( ==@ ) : sint t -> sint t -> sbool t
    val ( &&@ ) : sbool t -> sbool t -> sbool t
  end

  type t
  type sint := SInt.sint SInt.t

  val pp : Format.formatter -> t -> unit

  (** Merges two children node into a single node; returns the merged node. Note
      children of the node are always preserved too, for further accesses. *)
  val merge : left:t -> right:t -> t

  (** Splits the given node at [at], which is the relative offset within the
      node. Returns the left and right split trees, which themselves may contain
      further splits. *)
  val split :
    at:sint -> t -> ((t, sint) Split_tree.t * (t, sint) Split_tree.t) Symex.t

  type serialized

  val pp_serialized : Format.formatter -> serialized -> unit
  val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

  val iter_vars_serialized :
    serialized -> (Var.t * 'b Symex.Value.ty -> unit) -> unit

  (** Serialize this memory value; either returns [Some serialized], or [None]
      to signal the children must instead be serialized. *)
  val serialize : t -> serialized Seq.t option

  (** Extract the given [serialized] predicate from the tree; this may result in
      an empty ([NotOwned Totally]) tree, or may only modify part of the tree if
      the predicate only represents part of this tree's state. A [Missing] may
      be raised if part of the state is missing for the consumption to succeed.

      The input tree corresponds to the subtree relevant to the predicate's
      offset and length, meaning [t.node] is the node covering the whole
      predicate's range. *)
  val consume :
    serialized ->
    (t, sint) tree ->
    ((t, sint) tree, 'err, serialized) Symex.Result.t

  (** Add the given [serialized] predicate onto the given tree; the input tree
      is not necessarily empty ([NotOwned Totally]), and if the predicate
      overlaps the production may [vanish].

      The input tree corresponds to the subtree relevant to the predicate's
      offset and length, meaning [t.node] is the node covering the whole
      predicate's range. *)
  val produce : serialized -> (t, sint) tree -> (t, sint) tree Symex.t

  (** Returns [ok] if this memory value is exclusively owned, ie no additional
      state can be composed with it; in other words, calling [produce] on a tree
      with this node must always vanish. Otherwise this should raise a [miss]
      with the fixes needed for this to become exclusively owned. *)
  val assert_exclusively_owned : t -> (unit, 'err, serialized) Symex.Result.t
end

module Make
    (Symex : Symex.S)
    (MemVal :
      MemVal
        with module Symex = Symex
         and type 'a SInt.t = 'a Symex.Value.t
         and type SInt.sbool = Symex.Value.S_bool.t) =
struct
  open Compo_res
  open Symex.Syntax
  open Symex
  open MemVal.SInt

  module Sym_int_syntax = struct
    let zero = MemVal.SInt.zero
  end

  type nonrec sint = sint Symex.Value.t

  (* re-export the types to be able to use them easily *)
  type nonrec ('a, 'sint) tree = ('a, 'sint) tree = {
    node : 'a node;
    range : 'sint * 'sint;
    children : (('a, 'sint) tree * ('a, 'sint) tree) option;
  }

  module Range = struct
    type t = sint * sint

    let pp ft (l, u) = Fmt.pf ft "[%a, %a[" Symex.Value.ppa l Symex.Value.ppa u
    let sem_eq (a, c) (b, d) = a ==@ b &&@ (c ==@ d)
    let is_inside (l1, u1) (l2, u2) = l2 <=@ l1 &&@ (u1 <=@ u2)
    let strictly_inside x (l, u) = l <@ x &&@ (x <@ u)
    let size (l, u) = u -@ l
    let split_at (l, h) x = ((l, x), (x, h))
    let offset (l, u) off = (l +@ off, u +@ off)
    let of_low_and_size low size = (low, low +@ size)
  end

  module Split_tree = Split_tree

  module Node = struct
    type qty = node_qty = Partially | Totally

    let pp_qty ft = function
      | Partially -> Fmt.pf ft "Part."
      | Totally -> Fmt.pf ft "Tot."

    let merge_qty left right =
      match (left, right) with Totally, Totally -> Totally | _, _ -> Partially

    type t = MemVal.t node

    let pp ft = function
      | NotOwned qty -> Fmt.pf ft "NotOwned %a" pp_qty qty
      | Owned v -> MemVal.pp ft v

    let is_empty = function NotOwned Totally -> true | _ -> false
    let is_fully_owned = function NotOwned _ -> false | Owned _ -> true

    let assert_exclusively_owned = function
      | NotOwned _ ->
          Result.miss_no_fix
            ~reason:"assert_exclusively_owned - tree not fully owned" ()
      | Owned n -> MemVal.assert_exclusively_owned n

    let merge ~left ~right =
      match (left, right) with
      | NotOwned Totally, NotOwned Totally -> (NotOwned Totally, false)
      | NotOwned _, _ | _, NotOwned _ -> (NotOwned Partially, true)
      | Owned left, Owned right ->
          let v = MemVal.merge ~left ~right in
          (Owned v, true)

    let split ~at node =
      match node with
      | Owned v ->
          let lift = Split_tree.map (fun v -> Owned v) in
          let+ left, right = MemVal.split ~at v in
          (lift left, lift right)
      | NotOwned Totally ->
          return
            ( Split_tree.Leaf (NotOwned Totally),
              Split_tree.Leaf (NotOwned Totally) )
      | NotOwned Partially -> failwith "Should never split an intermediate node"
  end

  module Tree = struct
    type t = (MemVal.t, sint) tree

    let rec pp ft { node; range; children } =
      let open Fmt in
      pf ft "@[<v 2>{%a %a%a}@]" Node.pp node Range.pp range
        (option ~none:nop (fun fmt (l, r) ->
             pf fmt " -->@,@[%a@,%a@]" pp l pp r))
        children

    let make = make_tree
    let is_empty t = Node.is_empty t.node
    let not_owned range = make ~node:(NotOwned Totally) ~range ?children:None ()

    let rec map_leaves t f =
      match t.children with
      | None -> f t
      | Some (l, r) ->
          let** l = map_leaves l f in
          let++ r = map_leaves r f in
          { t with children = Some (l, r) }

    let rec iter_leaves_rev t f =
      match t.children with
      | None -> f t
      | Some (l, r) ->
          iter_leaves_rev r f;
          iter_leaves_rev l f

    let of_children_s ~left ~right =
      let range = (fst left.range, snd right.range) in
      let node, keep_children = Node.merge ~left:left.node ~right:right.node in
      let children = if keep_children then Some (left, right) else None in
      return { range; children; node }

    let of_children _ ~left ~right = of_children_s ~left ~right

    let with_children t ~left ~right =
      return { t with children = Some (left, right) }

    let rec offset ~by tree =
      let low, high = tree.range in
      let range = (low +@ by, high +@ by) in
      let children =
        Option.map (fun (l, r) -> (offset ~by l, offset ~by r)) tree.children
      in
      make ~node:tree.node ~range ?children ()

    (** Converts a [Split_tree] of [Node]s (ie. a tree with no base) into a
        [Tree], reconstructing each node's range and constructing intermediary
        nodes. *)
    let rec of_split_tree range = function
      | Split_tree.Leaf node -> return @@ make ~node ~range ()
      | Node (left, at, right) ->
          let left_span, right_span = Range.split_at range (fst range +@ at) in
          let* left = of_split_tree left_span left in
          let* right = of_split_tree right_span right in
          of_children_s ~left ~right

    (** Converts the given [Tree] into a [Split_tree], ignoring intermediary
        nodes and erasing offset information. *)
    let rec to_split_tree t : (Node.t, sint) Split_tree.t =
      let low, _ = t.range in
      let aux t : (Node.t, sint) Split_tree.t =
        match t.children with
        | None -> Leaf t.node
        | Some (left, right) ->
            let _, split = left.range in
            Node (to_split_tree left, split -@ low, to_split_tree right)
      in
      aux t

    (** [split ~range t] isolates [range] from [t]. Precondition: [range] is a
        strict subrange of [t.range] (neither empty nor equal to [t.range]).
        Returns [(node, left, right)] where:
        - [node] is the node covering exactly [range]
        - [left] and [right] can be safely set as [t]'s children, and [range]
          lies within either [left] or [right].

        If [range] touches the left or right edge of [t], a single split
        suffices. Otherwise, we first split at [fst range] to peel off the left
        part, then carve [range] out of the resulting right part. This makes the
        procedure right-biased (it prefers introducing structure on the right)
    *)
    let rec split ~range t : (Node.t * t * t) Symex.t =
      let old_span = t.range in
      let ol, oh = old_span in
      let nl, nh = range in
      if%sat ol ==@ nl then
        let* left_node, right_node = Node.split ~at:(nh -@ ol) t.node in
        let left_span, right_span = Range.split_at old_span nh in
        let* left = of_split_tree left_span left_node in
        let+ right = of_split_tree right_span right_node in
        (left.node, left, right)
      else
        if%sat oh ==@ nh then
          let* left_node, right_node = Node.split ~at:(nl -@ ol) t.node in
          let left_span, right_span = Range.split_at old_span nl in
          let* left = of_split_tree left_span left_node in
          let+ right = of_split_tree right_span right_node in
          (right.node, left, right)
        else
          (* We're first splitting on the left then splitting again on the right *)
          let* left_node, right_node = Node.split ~at:(nl -@ ol) t.node in
          let left_span, right_span = Range.split_at old_span nl in
          let* left = of_split_tree left_span left_node in
          let* full_right = of_split_tree right_span right_node in
          (* we need to first extract the relevant part of the right subtree; as
             constructing it may have yielded a complex tree *)
          let* sub_right, right_extra = extract full_right range in
          let* node, right_left, right_right = split ~range sub_right in
          let* right =
            with_children sub_right ~left:right_left ~right:right_right
          in
          let+ right =
            match right_extra with
            | None -> return right
            | Some right_extra ->
                with_children full_right ~left:right ~right:right_extra
          in
          (node, left, right)

    and extract (t : t) (range : Range.t) : (t * t option) Symex.t =
      (* First result is the extracted tree, second is the remain *)
      if%sat Range.sem_eq range t.range then return (t, None)
      else if Option.is_none t.children then return (t, None)
      else
        let left, right = Option.get t.children in
        if%sat Range.is_inside range left.range then
          let* extracted, new_left = extract left range in
          let+ new_self =
            match new_left with
            | Some left -> of_children_s ~right ~left
            | None -> return right
          in
          (extracted, Some new_self)
        else
          let* extracted, new_right = extract right range in
          let+ new_self =
            match new_right with
            | Some right -> of_children_s ~right ~left
            | None -> return left
          in
          (extracted, Some new_self)

    let extend_if_needed t range =
      let rl, rh = range in
      let sl, sh = t.range in
      let* t_with_left =
        if%sat rl <@ sl then
          let new_left_tree =
            make ~node:(NotOwned Totally) ~range:(rl, sl) ()
          in
          let children = (new_left_tree, t) in
          let qty = if is_empty t then Node.Totally else Partially in
          return (make ~node:(NotOwned qty) ~range:(rl, sh) ~children ())
        else return t
      in
      let sl, _ = t_with_left.range in
      let* result =
        if%sat sh <@ rh then
          let new_right_tree =
            make ~node:(NotOwned Totally) ~range:(sh, rh) ()
          in
          let children = (t_with_left, new_right_tree) in
          let qty = if is_empty t_with_left then Node.Totally else Partially in
          return (make ~node:(NotOwned qty) ~range:(sl, rh) ~children ())
        else return t_with_left
      in
      return result

    let rec add_to_the_right t addition : t Symex.t =
      match t.children with
      | None -> of_children_s ~left:t ~right:addition
      | Some (left, right) ->
          let* new_right = add_to_the_right right addition in
          of_children_s ~left ~right:new_right

    let rec add_to_the_left t addition : t Symex.t =
      match t.children with
      | None -> of_children_s ~left:addition ~right:t
      | Some (left, right) ->
          let* new_left = add_to_the_left left addition in
          of_children_s ~left:new_left ~right

    (** [frame_range t ~replace_node ~rebuild_parent range] Extracts from [t]
        the subtree that exactly spans [range]. The [range] must be non-empty.
        If [t] does not already cover [range], it is first extended with
        [NotOwned] nodes so that it does.

        Once the target subtree is found, [replace_node] is applied to it and
        must return the replacement subtree.

        The path back to the root is then rebuilt by calling [rebuild_parent]
        with the (possibly modified) children at each step. Use [of_children_s]
        for [rebuild_parent] when the parent node needs to be recomputed from
        its children. Use [with_children] when only the tree structure changed
        (e.g. after a load) and no recomputation of the parent’s semantic
        content is required. In doubt, [of_children_s] is usually a safe bet.

        [frame_range] returns a pair of the extracted subtree (before
        modification with [replace_node]) and the new root of the whole tree. *)
    let frame_range (t : t) ~(replace_node : t -> (t, 'e, 'm) Result.t)
        ~rebuild_parent (range : Range.t) : (t * t, 'e, 'm) Result.t =
      let rec frame_inside ~(replace_node : t -> (t, 'e, 'm) Result.t)
          ~rebuild_parent (t : t) (range : Range.t) : (t * t, 'e, 'm) Result.t =
        if%sat Range.sem_eq range t.range then
          let++ new_tree = replace_node t in
          (t, new_tree)
        else
          match t.children with
          | Some (left, right) ->
              let _, mid = left.range in
              if%sat Range.strictly_inside mid range then
                let l, h = range in
                let upper_range = (mid, h) in
                let dont_replace_node = Result.ok in
                if%sat
                  (* High-range already good *)
                  Range.sem_eq upper_range right.range
                then
                  (* Rearrange left*)
                  let lower_range = (l, mid) in
                  let** _, left =
                    frame_inside ~replace_node:dont_replace_node
                      ~rebuild_parent:with_children left lower_range
                  in
                  let* extracted, left_opt = extract left lower_range in
                  let* right = add_to_the_left right extracted in
                  let* new_self =
                    of_children_s ~left:(Option.get left_opt) ~right
                  in
                  frame_inside ~replace_node ~rebuild_parent new_self range
                else
                  let** _, right =
                    frame_inside ~replace_node:dont_replace_node
                      ~rebuild_parent:with_children right upper_range
                  in
                  let* extracted, right_opt = extract right upper_range in
                  let* left = add_to_the_right left extracted in
                  let* new_self =
                    of_children_s ~left ~right:(Option.get right_opt)
                  in
                  frame_inside ~replace_node ~rebuild_parent new_self range
              else
                if%sat Range.is_inside range left.range then
                  let** node, left =
                    frame_inside ~replace_node ~rebuild_parent left range
                  in
                  let+ new_parent = rebuild_parent t ~left ~right in
                  Ok (node, new_parent)
                else
                  (* Range is necessarily inside of right *)
                  let** node, right =
                    frame_inside ~replace_node ~rebuild_parent right range
                  in
                  let+ new_parent = rebuild_parent t ~left ~right in
                  Ok (node, new_parent)
          | None ->
              let* _, left, right = split ~range t in
              let* new_self = with_children t ~left ~right in
              frame_inside ~replace_node ~rebuild_parent new_self range
      in
      let* root = extend_if_needed t range in
      frame_inside ~replace_node ~rebuild_parent root range

    (* Exposed helpers *)

    let get_raw ofs size t =
      let range = Range.of_low_and_size ofs size in
      let replace_node node = Result.ok node in
      let rebuild_parent = with_children in
      frame_range t ~replace_node ~rebuild_parent range

    let put_raw tree t =
      let rebuild_parent = of_children in
      let replace_node _ = Result.ok tree in
      let++ _, new_tree =
        frame_range t ~replace_node ~rebuild_parent tree.range
      in
      ((), new_tree)

    (** Cons/prod *)

    let consume (serialized : MemVal.serialized) (range : Range.t) (st : t) :
        (t, 'err, MemVal.serialized) Symex.Result.t =
      let replace_node = MemVal.consume serialized in
      let rebuild_parent = of_children in
      let++ _, tree = frame_range st ~replace_node ~rebuild_parent range in
      tree

    let produce (serialized : MemVal.serialized) (range : Range.t) (st : t) :
        t Symex.t =
      let replace_node t =
        let+ t = MemVal.produce serialized t in
        Ok t
      in
      let rebuild_parent = of_children in
      let* res = frame_range st ~replace_node ~rebuild_parent range in
      match res with Ok (_, tree) -> return tree | _ -> Symex.vanish ()
  end

  type t = {
    root : Tree.t;
    bound : sint option; [@printer Fmt.(option ~none:(any "⊥") Symex.Value.ppa)]
  }
  [@@deriving show { with_path = false }]

  let is_empty t = Option.is_none t.bound && Tree.is_empty t.root

  let pp_pretty ft t =
    let open PrintBox in
    let r = ref [] in
    let () =
      Option.iter
        (fun b ->
          let range_str = Fmt.str "[%a; ∞[" Symex.Value.ppa b in
          r := [ (range_str, text "OOB") ])
        t.bound
    in
    let () =
      Tree.iter_leaves_rev t.root (fun leaf ->
          let range_str = (Fmt.to_to_string Range.pp) leaf.range in
          let node_str = Fmt.to_to_string Node.pp leaf.node |> text in
          r := (range_str, node_str) :: !r)
    in
    PrintBox_text.pp ft (frame @@ record !r)

  (** Logic *)

  type serialized_atom =
    | MemVal of {
        offset : sint; [@printer Symex.Value.ppa]
        len : sint; [@printer Symex.Value.ppa]
        v : MemVal.serialized;
      }
    | Bound of sint [@printer fun f v -> Fmt.pf f "Bound(%a)" Symex.Value.ppa v]
  [@@deriving show { with_path = false }]

  type serialized = serialized_atom list

  let lift_miss ~offset ~len symex =
    let+? fix = symex in
    [ MemVal { v = fix; offset; len } ]

  let iter_values_serialized serialized f =
    List.iter (function MemVal { v; _ } -> f v | _ -> ()) serialized

  let of_opt ?(mk_fixes = fun () -> Symex.return []) = function
    | None ->
        let+ fixes = mk_fixes () in
        Missing fixes
    | Some t -> Result.ok t

  let to_opt t = if is_empty t then None else Some t

  let with_bound_check (t : t) (ofs : sint) f =
    let** () =
      match t.bound with
      | None -> Result.ok ()
      | Some bound ->
          if%sat bound <@ ofs then Result.error `OutOfBounds else Result.ok ()
    in
    let++ res, root = f t.root in
    (res, to_opt { t with root })

  let with_bound_and_owned_check ?mk_fixes t ofs f =
    let** t = of_opt ?mk_fixes t in
    with_bound_check t ofs f

  let assert_exclusively_owned t =
    let** t = of_opt t in
    match t.bound with
    | None ->
        Result.miss_no_fix ~reason:"assert_exclusively_owned - no bound" ()
    | Some bound ->
        let { range = low, high; node; _ } = t.root in
        if%sat low ==@ 0s &&@ (high ==@ bound) then
          lift_miss ~offset:0s ~len:bound @@ Node.assert_exclusively_owned node
        else
          Result.miss_no_fix
            ~reason:"assert_exclusively_owned - tree does not span [0; bound["
            ()

  let get_raw_tree_owned ofs size t =
    let@ t = with_bound_and_owned_check t (ofs +@ size) in
    let** tree, t = Tree.get_raw ofs size t in
    if Node.is_fully_owned tree.node then
      let tree = Tree.offset ~by:(0s -@ ofs) tree in
      Result.ok (tree, t)
    else Result.miss_no_fix ~reason:"get_raw_tree_owned" ()

  (* This is used for copy_nonoverapping.
   It is an action on the destination block, and assumes the received tree is at offset 0 *)
  let put_raw_tree ofs (tree : Tree.t) t :
      (unit * t option, 'err, 'fix list) Result.t =
    let size = Range.size tree.range in
    let@ t = with_bound_and_owned_check t (ofs +@ size) in
    let tree = Tree.offset ~by:ofs tree in
    Tree.put_raw tree t

  let alloc v size =
    {
      root = { node = Owned v; range = (0s, size); children = None };
      bound = Some size;
    }

  (** Logic *)

  let subst_serialized subst_var (serialized : serialized) =
    let v_subst v = Symex.Value.subst subst_var v in
    let subst_atom = function
      | MemVal { offset; len; v } ->
          let v = MemVal.subst_serialized subst_var v in
          MemVal { offset = v_subst offset; len = v_subst len; v }
      | Bound v -> Bound (v_subst v)
    in
    List.map subst_atom serialized

  let iter_vars_serialized serialized f =
    List.iter
      (function
        | MemVal { offset; len; v } ->
            Symex.Value.iter_vars offset f;
            Symex.Value.iter_vars len f;
            MemVal.iter_vars_serialized v f
        | Bound v -> Symex.Value.iter_vars v f)
      serialized

  let pp_serialized = Fmt.Dump.list pp_serialized_atom

  let serialize t =
    let bound =
      match t.bound with
      | None -> Seq.empty
      | Some bound -> Seq.return (Bound bound)
    in
    let rec serialize_tree (tree : Tree.t) =
      match tree.node with
      | NotOwned Totally -> Seq.empty
      | Owned v -> (
          let offset = fst tree.range in
          let len = Range.size tree.range in
          match MemVal.serialize v with
          | None ->
              let left, right = Option.get tree.children in
              Seq.append (serialize_tree left) (serialize_tree right)
          | Some seq -> Seq.map (fun v -> MemVal { offset; len; v }) seq)
      | NotOwned Partially ->
          let left, right = Option.get tree.children in
          Seq.append (serialize_tree left) (serialize_tree right)
    in
    Seq.append (serialize_tree t.root) bound |> List.of_seq

  let consume_bound bound t =
    match t with
    | None | Some { bound = None; _ } ->
        Result.miss_no_fix ~reason:"consume_bound" ()
    | Some { bound = Some v; root } ->
        let+ () = Symex.assume [ v ==@ bound ] in
        Ok (to_opt { bound = None; root })

  let produce_bound bound t =
    match t with
    | None ->
        Symex.return
          (Some { bound = Some bound; root = Tree.not_owned (0s, bound) })
    | Some { bound = None; root } ->
        Symex.return (Some { bound = Some bound; root })
    | Some { bound = Some _; _ } -> Symex.vanish ()

  let produce_mem_val offset len v t =
    let ((_, high) as range) = Range.of_low_and_size offset len in
    let t =
      match t with
      | Some t -> t
      | None -> { bound = None; root = Tree.not_owned range }
    in
    let* () =
      match t.bound with
      | None -> return ()
      | Some bound -> Symex.assume [ high <=@ bound ]
    in
    let+ root = Tree.produce v range t.root in
    to_opt { t with root }

  let consume_mem_val offset len v t =
    let ((_, high) as range) = Range.of_low_and_size offset len in
    let** t = of_opt t in
    let* () =
      match t.bound with
      | None -> Symex.return ()
      | Some bound -> Symex.assume [ high <=@ bound ]
    in
    let++ root = lift_miss ~offset ~len @@ Tree.consume v range t.root in
    to_opt { t with root }

  let consume (list : serialized) (t : t option) =
    Symex.Result.fold_list
      ~f:(fun acc -> function
        | Bound bound -> consume_bound bound acc
        | MemVal { offset; len; v } -> consume_mem_val offset len v acc)
      ~init:t list

  let produce (list : serialized) (t : t option) =
    Symex.fold_list
      ~f:(fun acc -> function
        | Bound bound -> produce_bound bound acc
        | MemVal { offset; len; v } -> produce_mem_val offset len v acc)
      ~init:t list
end
