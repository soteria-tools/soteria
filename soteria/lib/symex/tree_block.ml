open Syntaxes.FunctionWrap

type ('a, 'sint) split_tree =
  | Leaf of 'a
  | Node of ('a, 'sint) split_tree * 'sint * ('a, 'sint) split_tree

let rec map_tree f = function
  | Leaf x -> Leaf (f x)
  | Node (l, at, r) -> Node (map_tree f l, at, map_tree f r)

type ('a, 'sint) tree = {
  node : 'a node;
  range : 'sint * 'sint;
  children : (('a, 'sint) tree * ('a, 'sint) tree) option;
}

and 'a node = NotOwned of node_qty | Owned of 'a

and node_qty = Partially | Totally
[@@deriving show { with_path = false }, make]

module type MemVal = sig
  module Symex : Symex.S

  module SInt : sig
    type +'a t = 'a Symex.Value.t
    type sbool
    type sint

    val zero : sint t
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

  (** Merges two children node into a single node; returns the merged node,
      along with a flag indicating whether the children must be preserved
      ([`KeepChildren]), or if they can be discarded because the merged node
      contains all the required information ([`Complete]).

      For instance, two totally undefined nodes can be merged into a single
      totally undefined node, and the children can be discarded. However, an
      owned node and an unowned node are merged into a partly owned node where
      children must be preserved. *)
  val merge : left:t -> right:t -> t * [ `KeepChildren | `Complete ]

  (** Splits the given node at [at], which is the relative offset within the
      node. Returns the left and right split trees, which themselves may contain
      further splits. *)
  val split :
    at:sint -> t -> ((t, sint) split_tree * (t, sint) split_tree) Symex.t

  type serialized

  val pp_serialized : Format.formatter -> serialized -> unit
  val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

  val iter_vars_serialized :
    serialized -> (Var.t * 'b Symex.Value.ty -> unit) -> unit

  (** Serialize this memory value; either returns [Some serialized], or [None]
      to signal the children must instead be serialized. *)
  val serialize : t -> serialized option

  val consume :
    serialized -> (t, sint) tree -> (unit, 'err, serialized list) Symex.Result.t

  val produce : serialized -> t Symex.t
end

module Make
    (Symex : Symex.S)
    (MemVal :
      MemVal
        with module Symex = Symex
         and type 'a SInt.t = 'a Symex.Value.t
         and type SInt.sbool = Symex.Value.sbool) =
struct
  open Compo_res
  open Symex.Syntax
  open Symex
  open MemVal.SInt

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

  let miss_no_fix_immediate ?(msg = "") () =
    Soteria_logs.Logs.L.info (fun m -> m "MISSING WITH NO FIX %s" msg);
    Missing []

  let miss_no_fix ?msg () = return (miss_no_fix_immediate ?msg ())

  module Node = struct
    type qty = node_qty = Partially | Totally

    let pp_qty ft = function
      | Partially -> Fmt.pf ft "Part."
      | Totally -> Fmt.pf ft "Tot."

    let merge_qty left right =
      match (left, right) with Totally, Totally -> Totally | _, _ -> Partially

    (* Later we could add arbitrary annotations to the mem_val variant.
       For example, we can keep track of read-only or tainted bytes.
       i.e. MemVal { v: mem_val; annot: 'a }.
       Quite probably, the Node module can be parametrized by a module
       capturing the annotations behavior in analysis. *)
    type t = MemVal.t node

    let pp ft = function
      | NotOwned qty -> Fmt.pf ft "NotOwned %a" pp_qty qty
      | Owned v -> MemVal.pp ft v

    let is_empty = function NotOwned Totally -> true | _ -> false
    let is_fully_owned = function NotOwned _ -> false | Owned _ -> true

    let merge ~left ~right =
      match (left, right) with
      | NotOwned Totally, NotOwned Totally -> (NotOwned Totally, `Complete)
      | NotOwned _, _ | _, NotOwned _ -> (NotOwned Partially, `KeepChildren)
      | Owned left, Owned right ->
          let v, children = MemVal.merge ~left ~right in
          (Owned v, children)

    let split ~at node =
      match node with
      | Owned v ->
          let lift = map_tree (fun v -> Owned v) in
          let+ left, right = MemVal.split ~at v in
          (lift left, lift right)
      | NotOwned Totally ->
          return (Leaf (NotOwned Totally), Leaf (NotOwned Totally))
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

    let iter_leaves_offsets t =
      Iter.map
        (fun leaf ->
          let offset, _ = leaf.range in
          let offset = offset -@ fst t.range in
          (leaf.node, offset))
        (iter_leaves_rev t)

    let of_children_s ~left ~right =
      let range = (fst left.range, snd right.range) in
      let node, children = Node.merge ~left:left.node ~right:right.node in
      let children =
        match children with
        | `Complete -> None
        | `KeepChildren -> Some (left, right)
      in
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

    let rec tree_of_rec_node range = function
      | Leaf node -> return @@ make ~node ~range ()
      | Node (left, at, right) ->
          let left_span, right_span = Range.split_at range (fst range +@ at) in
          let* left = tree_of_rec_node left_span left in
          let* right = tree_of_rec_node right_span right in
          of_children_s ~left ~right

    let rec split ~range t : (Node.t * t * t) Symex.t =
      (* this function splits a tree and returns the node in the given range *)
      (* We're assuming that range is inside old_span *)
      let old_span = t.range in
      let ol, oh = old_span in
      let nl, nh = range in
      if%sat ol ==@ nl then
        let at = nh in
        let* left_node, right_node = Node.split ~at:(at -@ ol) t.node in
        let left_span, right_span = Range.split_at old_span at in
        let* left = tree_of_rec_node left_span left_node in
        let+ right = tree_of_rec_node right_span right_node in
        (left.node, left, right)
      else
        if%sat oh ==@ nh then
          let* left_node, right_node = Node.split ~at:(nl -@ ol) t.node in
          let left_span, right_span = Range.split_at old_span nl in
          let* left = tree_of_rec_node left_span left_node in
          let+ right = tree_of_rec_node right_span right_node in
          (right.node, left, right)
        else
          (* We're first splitting on the left then splitting again on the right *)
          let* left_node, right_node = Node.split ~at:(nl -@ ol) t.node in
          let left_span, right_span = Range.split_at old_span nl in
          let* left = tree_of_rec_node left_span left_node in
          let* full_right = tree_of_rec_node right_span right_node in
          let* sub_right, right_extra = extract full_right range in
          let* node, right_left, right_right = split ~range sub_right in
          let* right =
            with_children sub_right ~left:right_left ~right:right_right
          in
          match right_extra with
          | None -> return (node, left, right)
          | Some right_extra ->
              let+ right =
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
        (t, 'err, MemVal.serialized list) Symex.Result.t =
      let replace_node _ = Result.ok @@ not_owned range in
      let rebuild_parent = of_children in
      let** framed, tree = frame_range st ~replace_node ~rebuild_parent range in
      let++ () = MemVal.consume serialized framed in
      tree

    let produce (serialized : MemVal.serialized) (range : Range.t) (st : t) :
        t Symex.t =
      let replace_node t =
        match t.node with
        | NotOwned Totally ->
            let+ node = MemVal.produce serialized in
            Ok { node = Owned node; range; children = None }
        | _ -> vanish ()
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
    | Bound of sint [@printer Symex.Value.ppa]
  [@@deriving show { with_path = false }]

  type serialized = serialized_atom list

  let iter_values_serialized serialized f =
    List.iter (function MemVal { v; _ } -> f v | _ -> ()) serialized

  let with_bound_check (t : t) (ofs : sint) f =
    let** () =
      match t.bound with
      | None -> Result.ok ()
      | Some bound ->
          if%sat bound <@ ofs then Result.error `OutOfBounds else Result.ok ()
    in
    let++ res, root = f t.root in
    (res, { t with root })

  let of_opt ?(mk_fixes = fun () -> Symex.return []) = function
    | None ->
        let+ fixes = mk_fixes () in
        Missing fixes
    | Some t -> Result.ok t

  let with_bound_and_owned_check ?mk_fixes t ofs f =
    let** t = of_opt ?mk_fixes t in
    let++ res, root = with_bound_check t ofs f in
    (res, Some root)

  let to_opt t = if is_empty t then None else Some t

  let assert_exclusively_owned t =
    let** t = of_opt t in
    match t.bound with
    | None -> miss_no_fix ~msg:"assert_exclusively_owned - no bound" ()
    | Some bound ->
        let { range = low, high; node; _ } = t.root in
        if Node.is_fully_owned node then
          if%sat low ==@ zero &&@ (high ==@ bound) then Result.ok ()
          else
            miss_no_fix
              ~msg:"assert_exclusively_owned - tree does not span [0; bound[" ()
        else
          miss_no_fix ~msg:"assert_exclusively_owned - tree not fully owned" ()

  let get_raw_tree_owned ofs size t =
    let@ t = with_bound_and_owned_check t (ofs +@ size) in
    let** tree, t = Tree.get_raw ofs size t in
    if Node.is_fully_owned tree.node then
      let tree = Tree.offset ~by:(zero -@ ofs) tree in
      Result.ok (tree, t)
    else miss_no_fix ~msg:"get_raw_tree_owned" ()

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
      root = { node = Owned v; range = (zero, size); children = None };
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
          | Some v -> Seq.return (MemVal { offset; len; v }))
      | NotOwned Partially ->
          let left, right = Option.get tree.children in
          Seq.append (serialize_tree left) (serialize_tree right)
    in
    Seq.append (serialize_tree t.root) bound |> List.of_seq

  let assume_bound_check_res (t : t) (ofs : sint) f =
    let* () =
      match t.bound with
      | None -> Symex.return ()
      | Some bound -> Symex.assume [ ofs <=@ bound ]
    in
    let++ root = f () in
    { t with root }

  let assume_bound_check (t : t) (ofs : sint) f =
    let* () =
      match t.bound with
      | None -> Symex.return ()
      | Some bound -> Symex.assume [ ofs <=@ bound ]
    in
    let+ root = f () in
    { t with root }

  let abstract_cons bound cons_tree t =
    let** t = of_opt t in
    let++ tree =
      let@ () = assume_bound_check_res t bound in
      cons_tree t.root
    in
    to_opt tree

  let abstract_prod bound prod_tree t =
    let t =
      match t with
      | Some t -> t
      | None -> { bound = None; root = Tree.not_owned (zero, bound) }
    in
    let+ tree =
      let@ () = assume_bound_check t bound in
      prod_tree t.root
    in
    to_opt tree

  let consume_bound bound t =
    match t with
    | None | Some { bound = None; _ } -> miss_no_fix ~msg:"consume_bound" ()
    | Some { bound = Some v; root } ->
        let+ () = Symex.assume [ v ==@ bound ] in
        Ok (to_opt { bound = None; root })

  let produce_bound bound t =
    match t with
    | None ->
        Symex.return
          (Some { bound = Some bound; root = Tree.not_owned (zero, bound) })
    | Some { bound = None; root } ->
        Symex.return (Some { bound = Some bound; root })
    | Some { bound = Some _; _ } -> Symex.vanish ()

  let consume_atom atom t =
    match atom with
    | Bound bound -> consume_bound bound t
    | MemVal { offset; len; v } ->
        let ((_, bound) as range) = Range.of_low_and_size offset len in
        abstract_cons bound (Tree.consume v range) t

  let produce_atom atom t =
    match atom with
    | Bound bound -> produce_bound bound t
    | MemVal { offset; len; v } ->
        let ((_, bound) as range) = Range.of_low_and_size offset len in
        abstract_prod bound (Tree.produce v range) t

  let consume (list : serialized) (t : t option) =
    Symex.Result.fold_list ~f:(fun acc st -> consume_atom st acc) ~init:t list

  let produce (list : serialized) (t : t option) =
    Symex.fold_list ~f:(fun acc st -> produce_atom st acc) ~init:t list
end
