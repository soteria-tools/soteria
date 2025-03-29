module type Impl = sig
  module Symex : Symex.S

  type 'a v := 'a Symex.Value.t
  type sbool := Symex.Value.sbool
  type sint

  val zero : sint v

  module Syntax : sig
    val ( <@ ) : sint v -> sint v -> sbool v
    val ( <=@ ) : sint v -> sint v -> sbool v
    val ( &&@ ) : sbool v -> sbool v -> sbool v
    val ( -@ ) : sint v -> sint v -> sint v
    val ( +@ ) : sint v -> sint v -> sint v
    val ( ~- ) : sint v -> sint v
  end

  module Leaf : sig
    type t
    type out_ty
    type ty

    val pp : t Fmt.t
    val pp_ty : ty Fmt.t
    val split_at : sint v -> t -> (t * t) Symex.MONAD.t
    val init : out_ty v -> ty -> t
    val decode : ty -> t -> (out_ty v, [> `UBTransmute ], 'miss) Symex.Result.t
    val to_zeros : ty -> out_ty v
    val is_zeros : t -> sbool v list
    val constrs : ty -> out_ty v -> sbool v list
    val size_of : ty -> sint v

    type serialized

    val pp_serialized : serialized Fmt.t
    val serialize : t -> serialized
    val deserialize : serialized -> out_ty v Symex.MONAD.t
    val ty_of : serialized -> ty
    val iter_vars : serialized -> 'b Symex.Value.ty Var.iter_vars
    val nondet_serialize : ty -> serialized Symex.MONAD.t
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized
  end
end

module Make
    (L : Logs.LOG)
    (Symex : Symex.S)
    (Impl : Impl with module Symex = Symex) =
struct
  module Leaf = Impl.Leaf
  (* This could be parametric on an integer type and Node type. But is there realy a reason ? *)

  open Compo_res
  open Symex.Syntax
  open Symex
  open Impl.Syntax

  let ( ==@ ) = Symex.Value.sem_eq

  type +'a v = 'a Symex.Value.t

  let pp_v _ = Symex.Value.ppa
  let pp_sint = Fmt.nop
  let ( let@ ) = ( @@ )

  let miss_no_fix_immediate ?(msg = "") () =
    let msg = "MISSING WITH NO FIX " ^ msg in
    L.info (fun m -> m "%s" msg);
    (* Symex.push_give_up (msg, get_loc ()); *)
    Compo_res.Missing []

  let miss_no_fix msg = Symex.return (miss_no_fix_immediate ~msg ())

  type serialized_atom =
    | TypedVal of { offset : Impl.sint v; v : Leaf.serialized }
    | Bound of Impl.sint v
    | Uninit of { offset : Impl.sint v; len : Impl.sint v }
    | Zeros of { offset : Impl.sint v; len : Impl.sint v }
    | Any of { offset : Impl.sint v; len : Impl.sint v }

  let pp_serialized_atom ft = function
    | TypedVal { offset; v } ->
        Fmt.pf ft "TypedVal {offset = %a; v = %a}" Symex.Value.ppa offset
          Leaf.pp_serialized v
    | Bound bound -> Fmt.pf ft "Bound %a" Symex.Value.ppa bound
    | Uninit { offset; len } ->
        Fmt.pf ft "Uninit {offset = %a; len = %a}" Symex.Value.ppa offset
          Symex.Value.ppa len
    | Zeros { offset; len } ->
        Fmt.pf ft "Zeros {offset = %a; len = %a}" Symex.Value.ppa offset
          Symex.Value.ppa len
    | Any { offset; len } ->
        Fmt.pf ft "Any {offset = %a; len = %a}" Symex.Value.ppa offset
          Symex.Value.ppa len

  type serialized = serialized_atom list

  let pp_serialized = Fmt.Dump.list pp_serialized_atom

  let mk_fix_typed offset ty () =
    let+ v = Leaf.nondet_serialize ty in
    [ [ TypedVal { offset; v } ] ]

  let mk_fix_any ~ofs ~len () = [ [ Any { offset = ofs; len } ] ]

  module Range = struct
    type t = Impl.sint v * Impl.sint v

    let pp ft (l, u) = Fmt.pf ft "[%a, %a[" Symex.Value.ppa l Symex.Value.ppa u
    let sem_eq (a, c) (b, d) = a ==@ b &&@ (c ==@ d)
    let is_inside (l1, u1) (l2, u2) = l2 <=@ l1 &&@ (u1 <=@ u2)
    let strictly_inside x (l, u) = l <@ x &&@ (x <@ u)
    let size (l, u) = u -@ l
    let split_at (l, h) x = ((l, x), (x, h))
    let of_low_and_size low size : t = (low, low +@ size)
    let offset (l, u) by = (l +@ by, u +@ by)
  end

  module Node = struct
    type qty = Partially | Totally

    let pp_qty ft = function
      | Partially -> Fmt.pf ft "Part."
      | Totally -> Fmt.pf ft "Tot."

    let merge_qty left right =
      match (left, right) with Totally, Totally -> Totally | _, _ -> Partially

    type mem_val = Zeros | Uninit of qty | Lazy | Init of Leaf.t | Any

    let pp_mem_val ft =
      let open Fmt in
      function
      | Zeros -> pf ft "Zeros"
      | Uninit qty -> pf ft "Uninit %a" pp_qty qty
      | Lazy -> pf ft "Lazy"
      | Init mv -> Leaf.pp ft mv
      | Any -> pf ft "Any"

    (* Later we could add arbitrary annotations to the mem_val variant.
       For example, we can keep track of read-only or tainted bytes.
       i.e. MemVal { v: mem_val; annot: 'a }.
       Quite probably, the Node module can be parametrized by a module
       capturing the annotations behavior in analysis. *)
    type t = NotOwned of qty | Owned of mem_val

    let pp ft = function
      | NotOwned qty -> Fmt.pf ft "NotOwned %a" pp_qty qty
      | Owned mv -> pp_mem_val ft mv

    let is_fully_owned = function NotOwned _ -> false | Owned _ -> true

    let merge ~left ~right =
      match (left, right) with
      | NotOwned Totally, NotOwned Totally -> return (NotOwned Totally)
      | NotOwned _, _ | _, NotOwned _ -> return (NotOwned Partially)
      | Owned Zeros, Owned Zeros -> return (Owned Zeros)
      | Owned (Uninit lq), Owned (Uninit rq) ->
          let qty = merge_qty lq rq in
          return (Owned (Uninit qty))
      | Owned _, Owned _ -> return (Owned Lazy)

    let split ~range:(_, _) ~at node =
      match node with
      | NotOwned Totally -> return (NotOwned Totally, NotOwned Totally)
      | Owned (Uninit Totally) ->
          return (Owned (Uninit Totally), Owned (Uninit Totally))
      | Owned Zeros -> return (Owned Zeros, Owned Zeros)
      | Owned Any -> return (Owned Any, Owned Any)
      | Owned (Init v) ->
          let+ l, r = Leaf.split_at at v in
          (Owned (Init l), Owned (Init r))
      | NotOwned Partially | Owned (Uninit Partially) | Owned Lazy ->
          failwith "Should never split an intermediate node"

    let uninit = Owned (Uninit Totally)

    let decode ~ofs ~ty t : (Leaf.out_ty v, 'err, 'fix) Symex.Result.t =
      match t with
      | NotOwned _ ->
          let+ fixes = mk_fix_typed ofs ty () in
          miss fixes
      | Owned (Uninit _) -> Result.error `UninitializedMemoryAccess
      | Owned Zeros -> Result.ok @@ Leaf.to_zeros ty
      | Owned Lazy ->
          Fmt.kstr miss_no_fix "Lazy memory access, cannot decode %a" pp t
      | Owned (Init value) -> Leaf.decode ty value
      | Owned Any ->
          (* We don't know if this read is valid, as memory could be uninitialised.
             We have to approximate and vanish. *)
          L.info (fun m -> m "Reading from Any memory, vanishing.");
          Symex.vanish ()
  end

  module Tree = struct
    type t = { node : Node.t; range : Range.t; children : (t * t) option }
    [@@deriving show { with_path = false }, make]

    let rec pp ft { node; range; children } =
      let open Fmt in
      pf ft "@[<v 2>{%a %a%a}@]" Node.pp node Range.pp range
        (option ~none:nop (fun fmt (l, r) ->
             pf fmt " -->@,@[%a@,%a@]" pp l pp r))
        children

    let is_empty { node; _ } =
      match node with NotOwned Totally -> true | _ -> false

    let uninit range = make ~node:Node.uninit ~range ?children:None ()

    let zeros range =
      make ~node:(Node.Owned Node.Zeros) ~range ?children:None ()

    let not_owned range = make ~node:(NotOwned Totally) ~range ?children:None ()

    let sval_leaf ~range ~value ~ty =
      make ~node:(Owned (Init (Leaf.init value ty))) ~range ?children:None ()

    let any range = make ~node:(Owned Node.Any) ~range ?children:None ()

    let rec iter_leaves_rev t f =
      match t.children with
      | None -> f t
      | Some (l, r) ->
          iter_leaves_rev r f;
          iter_leaves_rev l f

    let of_children_s ~left ~right =
      let range = (fst left.range, snd right.range) in
      let+ node = Node.merge ~left:left.node ~right:right.node in
      let children =
        match node with
        | Node.NotOwned Totally | Owned (Zeros | Uninit Totally) -> None
        | _ -> Some (left, right)
      in
      { range; children; node }

    let of_children _ ~left ~right = of_children_s ~left ~right

    let with_children t ~left ~right =
      return { t with children = Some (left, right) }

    let rec offset ~by tree =
      let range = Range.offset tree.range by in
      let children =
        Option.map (fun (l, r) -> (offset ~by l, offset ~by r)) tree.children
      in
      make ~node:tree.node ~range ?children ()

    let rec split ~range t : (Node.t * t * t) Symex.t =
      (* this function splits a tree and returns the node in the given range *)
      (* We're assuming that range is inside old_span *)
      let old_span = t.range in
      let ol, oh = old_span in
      let nl, nh = range in
      if%sat ol ==@ nl then
        let at = nh in
        let+ left_node, right_node = Node.split ~range:old_span ~at t.node in
        let left_span, right_span = Range.split_at old_span at in
        let left = make ~node:left_node ~range:left_span () in
        let right = make ~node:right_node ~range:right_span () in
        (left_node, left, right)
      else
        if%sat oh ==@ nh then
          let+ left_node, right_node =
            Node.split ~range:old_span ~at:nl t.node
          in
          let left_span, right_span = Range.split_at old_span nl in
          let left = make ~node:left_node ~range:left_span () in
          let right = make ~node:right_node ~range:right_span () in
          (right_node, left, right)
        else
          (* We're first splitting on the left then splitting again on the right *)
          let* left_node, right_node =
            Node.split ~range:old_span ~at:nl t.node
          in
          let left_span, right_span = Range.split_at old_span nl in
          let left = make ~node:left_node ~range:left_span () in
          let full_right = make ~node:right_node ~range:right_span () in
          let* node, right_left, right_right = split ~range full_right in
          let+ right =
            with_children full_right ~left:right_left ~right:right_right
          in
          (node, left, right)

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

    let rec extract (t : t) (range : Range.t) : (t * t option) Symex.t =
      (* First result is the extracted tree, second is the remain *)
      if%sat Range.sem_eq range t.range then return (t, None)
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

    let frame_range (t : t) ~(replace_node : t -> t) ~rebuild_parent
        (range : Range.t) : (t * t) Symex.t =
      let rec frame_inside ~(replace_node : t -> t) ~rebuild_parent (t : t)
          (range : Range.t) : (t * t) Symex.t =
        if%sat Range.sem_eq range t.range then
          let new_tree = replace_node t in
          return (t, new_tree)
        else
          match t.children with
          | Some (left, right) ->
              let _, mid = left.range in
              if%sat Range.strictly_inside mid range then
                let l, h = range in
                let upper_range = (mid, h) in
                let dont_replace_node t = t in
                if%sat
                  (* High-range already good *)
                  Range.sem_eq upper_range right.range
                then
                  (* Rearrange left*)
                  let lower_range = (l, mid) in
                  let* _, left =
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
                  let* _, right =
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
                  let* node, left =
                    frame_inside ~replace_node ~rebuild_parent left range
                  in
                  let+ new_parent = rebuild_parent t ~left ~right in
                  (node, new_parent)
                else
                  (* Range is necessarily inside of right *)
                  let* node, right =
                    frame_inside ~replace_node ~rebuild_parent right range
                  in
                  let+ new_parent = rebuild_parent t ~left ~right in
                  (node, new_parent)
          | None ->
              let* _, left, right = split ~range t in
              let* new_self = with_children t ~left ~right in
              frame_inside ~replace_node ~rebuild_parent new_self range
      in
      let* root = extend_if_needed t range in
      frame_inside ~replace_node ~rebuild_parent root range

    let load ?(is_move = false) ofs size ty t =
      let range = Range.of_low_and_size ofs size in
      let replace_node node = if is_move then uninit range else node in
      let rebuild_parent = with_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      let++ sval = Node.decode ~ofs ~ty framed.node in
      (sval, tree)

    let store ofs ty sval t =
      let size = Leaf.size_of ty in
      let range = Range.of_low_and_size ofs size in
      let replace_node _ = sval_leaf ~range ~value:sval ~ty in
      let rebuild_parent = of_children in
      let* node, tree = frame_range t ~replace_node ~rebuild_parent range in
      let++ () =
        match node.node with
        | NotOwned Totally ->
            let len = Leaf.size_of ty in
            let fixes = mk_fix_any ~ofs ~len () in
            Result.miss fixes
        | NotOwned Partially -> miss_no_fix "partially missing store"
        | _ -> Result.ok ()
      in
      ((), tree)

    let uninit_range ofs len t =
      let range = Range.of_low_and_size ofs len in
      let replace_node _ = uninit range in
      let rebuild_parent = of_children in
      let* node, tree = frame_range t ~replace_node ~rebuild_parent range in
      let++ () =
        match node.node with
        | NotOwned Totally ->
            let fixes = mk_fix_any ~ofs ~len () in
            Result.miss fixes
        | NotOwned Partially -> miss_no_fix "partially missing uninit"
        | _ -> Result.ok ()
      in
      ((), tree)

    let get_raw ofs size t =
      let range = Range.of_low_and_size ofs size in
      let replace_node node = node in
      let rebuild_parent = with_children in
      frame_range t ~replace_node ~rebuild_parent range

    let put_raw tree t =
      let replace_node _ = tree in
      let rebuild_parent = of_children in
      let* old_node, new_tree =
        frame_range t ~replace_node ~rebuild_parent tree.range
      in
      let++ () =
        match old_node.node with
        | NotOwned _ -> miss_no_fix "put_raw"
        | _ -> Result.ok ()
      in
      ((), new_tree)

    (** Cons/prod *)

    let consume_typed_val ofs size ty v t =
      let range = Range.of_low_and_size ofs size in
      let replace_node _ = not_owned range in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      let* sval_res = Node.decode ~ofs ~ty framed.node in
      match sval_res with
      | Ok sv ->
          let+ () = Symex.assume [ sv ==@ v ] in
          Ok tree
      | Missing fixes -> Symex.Result.miss fixes
      | Error _ -> Symex.vanish ()

    let produce_typed_val low size ty value t =
      let range = Range.of_low_and_size low size in
      let* () = Symex.assume (Leaf.constrs ty value) in
      let replace_node _ = sval_leaf ~range ~value ~ty in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      match framed.node with
      | NotOwned Totally -> return tree
      | _ -> Symex.vanish ()

    let consume_any low len t =
      let range = Range.of_low_and_size low len in
      let replace_node _ = not_owned range in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      let++ () =
        match framed.node with
        | NotOwned _ -> miss_no_fix "consume_any"
        | _ -> Result.ok ()
      in
      tree

    let produce_any low len t =
      let range = Range.of_low_and_size low len in
      let replace_node _ = any range in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      match framed.node with
      | NotOwned Totally -> return tree
      | _ -> Symex.vanish ()

    let consume_uninit low len t =
      let range = Range.of_low_and_size low len in
      let replace_node _ = not_owned range in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      let++ () =
        match framed.node with
        | NotOwned _ -> miss_no_fix "consume_uninit"
        | Owned (Uninit Totally) -> Result.ok ()
        | _ ->
            L.info (fun m -> m "Consuming uninit but no uninit, vanishing");
            Symex.vanish ()
      in
      tree

    let produce_uninit low len t =
      let range = Range.of_low_and_size low len in
      let replace_node _ = uninit range in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      match framed.node with
      | NotOwned Totally -> return tree
      | _ -> Symex.vanish ()

    let consume_zeros low len t =
      let range = Range.of_low_and_size low len in
      let replace_node _ = not_owned range in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      match framed.node with
      | NotOwned _ -> miss_no_fix "consume_zeros"
      | Owned Zeros -> Result.ok tree
      | Owned (Init value) ->
          let+ () = Symex.assume @@ Leaf.is_zeros value in
          Ok tree
      | _ ->
          L.info (fun m -> m "Consuming zero but not zero, vanishing");
          Symex.vanish ()

    let produce_zeros low len t =
      let range = Range.of_low_and_size low len in
      let replace_node _ = zeros range in
      let rebuild_parent = of_children in
      let* framed, tree = frame_range t ~replace_node ~rebuild_parent range in
      match framed.node with
      | NotOwned Totally -> return tree
      | _ -> Symex.vanish ()
  end

  type t = { root : Tree.t; bound : Impl.sint v option }

  let pp fmt t =
    let open Format in
    fprintf fmt "{ root = %a; bound = %a }" Tree.pp t.root
      Fmt.(option ~none:(any "None") Symex.Value.ppa)
      t.bound

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
          let node_str = Fmt.kstr text "%a" Node.pp leaf.node in
          r := (range_str, node_str) :: !r)
    in
    PrintBox_text.pp ft (frame @@ record !r)

  (** Logic *)

  let iter_values_serialized serialized f =
    List.iter (function TypedVal { v; _ } -> f v | _ -> ()) serialized

  let with_bound_check t ofs f =
    let** () =
      match t.bound with
      | None -> Result.ok ()
      | Some bound ->
          if%sat bound <@ ofs then (
            L.debug (fun m ->
                m "Out of bounds access: %a > %a" Symex.Value.ppa ofs
                  Symex.Value.ppa bound);
            Result.error `OutOfBounds)
          else Result.ok ()
    in
    let++ res, root = f () in
    (res, { t with root })

  let of_opt ?(mk_fixes = fun () -> Symex.return []) = function
    | None ->
        let+ fixes = mk_fixes () in
        Compo_res.miss fixes
    | Some t -> Result.ok t

  let to_opt t = if is_empty t then None else Some t

  let assert_exclusively_owned t =
    let** t = of_opt t in
    match t.bound with
    | None -> miss_no_fix "assert_exclusively_owned - no bound"
    | Some bound ->
        let Tree.{ range = low, high; node; _ } = t.root in
        if Node.is_fully_owned node then
          if%sat low ==@ Impl.zero &&@ (high ==@ bound) then Result.ok ()
          else
            miss_no_fix
              "assert_exclusively_owned - tree does not span [0; bound["
        else miss_no_fix "assert_exclusively_owned - tree not fully owned"

  let load ?is_move ofs ty t =
    let** t = of_opt ~mk_fixes:(mk_fix_typed ofs ty) t in
    let size = Leaf.size_of ty in
    let++ res, tree =
      let@ () = with_bound_check t (ofs +@ size) in
      Tree.load ?is_move ofs size ty t.root
    in
    (res, to_opt tree)

  let store ofs ty sval t =
    let size = Leaf.size_of ty in
    match t with
    | None ->
        let fixes = mk_fix_any ~ofs ~len:size () in
        Result.miss fixes
    | Some t ->
        let++ (), tree =
          let@ () = with_bound_check t (ofs +@ size) in
          Tree.store ofs ty sval t.root
        in
        ((), to_opt tree)

  let get_raw_tree_owned ofs size t =
    let** t = of_opt t in
    let++ res, tree =
      let@ () = with_bound_check t (ofs +@ size) in
      let+ tree, t = Tree.get_raw ofs size t.root in
      if Node.is_fully_owned tree.node then
        let tree = Tree.offset ~by:~-ofs tree in
        Ok (tree, t)
      else miss_no_fix_immediate ~msg:"get_raw_tree_owned" ()
    in
    (res, to_opt tree)

  (* This is used for copy_nonoverapping.
     It is an action on the destination block, and assumes the received tree is at offset 0 *)
  let put_raw_tree ofs (tree : Tree.t) t :
      (unit * t option, 'err, 'fix list) Result.t =
    let** t = of_opt t in
    let size = Range.size tree.range in
    let tree = Tree.offset ~by:ofs tree in
    let++ res, t =
      let@ () = with_bound_check t (ofs +@ size) in
      Tree.put_raw tree t.root
    in
    (res, to_opt t)

  let alloc size = { root = Tree.uninit (Impl.zero, size); bound = Some size }

  let uninit_range ofs size t =
    match t with
    | None -> miss_no_fix "uninit on none"
    | Some t ->
        let++ (), tree =
          let@ () = with_bound_check t (ofs +@ size) in
          Tree.uninit_range ofs size t.root
        in
        ((), to_opt tree)

  (** Logic *)

  let subst_serialized subst_var (serialized : serialized) =
    let v_subst v = Symex.Value.subst subst_var v in
    let subst_atom = function
      | TypedVal { offset; v } ->
          TypedVal
            { offset = v_subst offset; v = Leaf.subst_serialized subst_var v }
      | Bound v -> Bound (v_subst v)
      | Uninit { offset; len } ->
          Uninit { offset = v_subst offset; len = v_subst len }
      | Zeros { offset; len } ->
          Zeros { offset = v_subst offset; len = v_subst len }
      | Any { offset; len } ->
          Any { offset = v_subst offset; len = v_subst len }
    in
    List.map subst_atom serialized

  let iter_vars_serialized serialized
      (f : Var.t * Leaf.out_ty Symex.Value.ty -> unit) =
    List.iter
      (function
        | Bound v -> Symex.Value.iter_vars v f
        | TypedVal { offset; v; _ } ->
            Symex.Value.iter_vars offset f;
            Leaf.iter_vars v f
        | Uninit { offset; len } | Zeros { offset; len } | Any { offset; len }
          ->
            Symex.Value.iter_vars offset f;
            Symex.Value.iter_vars len f)
      serialized

  let serialize t =
    let bound =
      match t.bound with
      | None -> Seq.empty
      | Some bound -> Seq.return (Bound bound)
    in
    let rec serialize_tree (tree : Tree.t) =
      match tree.node with
      | Node.NotOwned _ -> Seq.empty
      | Owned Zeros ->
          Seq.return
            (Zeros { offset = fst tree.range; len = Range.size tree.range })
      | Owned (Uninit Totally) ->
          Seq.return
            (Uninit { offset = fst tree.range; len = Range.size tree.range })
      | Owned (Init value) ->
          Seq.return
            (TypedVal { offset = fst tree.range; v = Leaf.serialize value })
      | Owned Any ->
          Seq.return
            (Any { offset = fst tree.range; len = Range.size tree.range })
      | Owned (Lazy | Uninit Partially) ->
          let children = Option.get tree.children in
          Seq.append
            (serialize_tree (fst children))
            (serialize_tree (snd children))
    in
    Seq.append (serialize_tree t.root) bound |> List.of_seq

  let assume_bound_check_res t ofs f =
    let* () =
      match t.bound with
      | None -> Symex.return ()
      | Some bound -> Symex.assume [ ofs <=@ bound ]
    in
    let++ root = f () in
    { t with root }

  let assume_bound_check t ofs f =
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

  let abstract_prod bound mk_root_from_empty prod_tree t =
    match t with
    | None ->
        let+ root = mk_root_from_empty () in
        Some { bound = None; root }
    | Some t ->
        let+ tree =
          let@ () = assume_bound_check t bound in
          prod_tree t.root
        in
        to_opt tree

  let consume_typed_val ofs ty v t =
    let size = Leaf.size_of ty in
    let cons_tree root = Tree.consume_typed_val ofs size ty v root in
    abstract_cons (ofs +@ size) cons_tree t

  let produce_typed_val ofs ty v t =
    let size = Leaf.size_of ty in
    let prod_tree root = Tree.produce_typed_val ofs size ty v root in
    let mk_root_from_empty () =
      let range = Range.of_low_and_size ofs size in
      return @@ Tree.sval_leaf ~range ~value:v ~ty
    in
    abstract_prod (ofs +@ size) mk_root_from_empty prod_tree t

  let consume_any ofs len t =
    let cons_tree root = Tree.consume_any ofs len root in
    abstract_cons (ofs +@ len) cons_tree t

  let produce_any ofs len t =
    let prod_tree root = Tree.produce_any ofs len root in
    let mk_root_from_empty () =
      let range = (ofs, ofs +@ len) in
      Symex.return (Tree.any range)
    in
    abstract_prod (ofs +@ len) mk_root_from_empty prod_tree t

  let consume_uninit ofs len t =
    let cons_tree root = Tree.consume_uninit ofs len root in
    abstract_cons (ofs +@ len) cons_tree t

  let produce_uninit ofs len t =
    let bound = ofs +@ len in
    let prod_tree root = Tree.produce_uninit ofs len root in
    let mk_root_from_empty () =
      let range = (ofs, bound) in
      Symex.return (Tree.uninit range)
    in
    abstract_prod bound mk_root_from_empty prod_tree t

  let consume_zeros ofs len t =
    let cons_tree root = Tree.consume_zeros ofs len root in
    abstract_cons (ofs +@ len) cons_tree t

  let produce_zeros ofs len t =
    let bound = ofs +@ len in
    let prod_tree root = Tree.produce_zeros ofs len root in
    let mk_root_from_empty () =
      let range = (ofs, bound) in
      Symex.return (Tree.zeros range)
    in
    abstract_prod bound mk_root_from_empty prod_tree t

  let consume_bound bound t =
    match t with
    | None | Some { bound = None; _ } -> miss_no_fix "consume_bound"
    | Some { bound = Some v; root } ->
        let+ () = Symex.assume [ v ==@ bound ] in
        Ok (to_opt { bound = None; root })

  let produce_bound bound t =
    match t with
    | None ->
        Symex.return
          (Some { bound = Some bound; root = Tree.not_owned (Impl.zero, bound) })
    | Some { bound = None; root } ->
        Symex.return (Some { bound = Some bound; root })
    | Some { bound = Some _; _ } -> Symex.vanish ()

  let consume_atom atom t =
    match atom with
    | Bound bound -> consume_bound bound t
    | TypedVal { offset; v } ->
        let ty = Leaf.ty_of v in
        let* v = Leaf.deserialize v in
        consume_typed_val offset ty v t
    | Uninit { offset; len } -> consume_uninit offset len t
    | Any { offset; len } -> consume_any offset len t
    | Zeros { offset; len } -> consume_zeros offset len t

  let produce_atom atom t =
    match atom with
    | Bound bound -> produce_bound bound t
    | TypedVal { offset; v } ->
        let ty = Leaf.ty_of v in
        let* v = Leaf.deserialize v in
        produce_typed_val offset ty v t
    | Uninit { offset; len } -> produce_uninit offset len t
    | Any { offset; len } -> produce_any offset len t
    | Zeros { offset; len } -> produce_zeros offset len t

  let consume (list : serialized) (t : t option) =
    Symex.Result.fold_list ~f:(fun acc st -> consume_atom st acc) ~init:t list

  let produce (list : serialized) (t : t option) =
    Symex.fold_list ~f:(fun acc st -> produce_atom st acc) ~init:t list
end
