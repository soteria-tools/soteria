open Soteria.Symex.Compo_res
open Typed
open Typed.Infix
open Charon
open Syntaxes.FunctionWrap
module DecayMapMonad = Sptr.DecayMapMonad
open DecayMapMonad
open DecayMapMonad.Result
open DecayMapMonad.Syntax

module Make (Sptr : Sptr.S) = struct
  module Encoder = Encoder.Make (Sptr)

  type rust_val = Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp Sptr.pp

  module MemVal = struct
    module TB = Soteria.Sym_states.Tree_block
    module Symex = DecayMapMonad

    module SInt = struct
      include Typed
      include Typed.Infix

      type sint = T.sint
      type sbool = T.sbool

      let zero () = Typed.BitVec.usize Z.zero
      let ( <@ ) = Typed.Infix.( <$@ )
      let ( <=@ ) = Typed.Infix.( <=$@ )
      let ( +@ ) = Typed.Infix.( +!@ )
      let ( -@ ) = Typed.Infix.( -!@ )
    end

    let pp_init ft (v, ty) =
      let open Fmt in
      pf ft "%a : %a" pp_rust_val v Charon_util.pp_ty ty

    type qty = Totally | Partially [@@deriving show { with_path = false }]

    type value =
      | Init of (rust_val * Types.ty) [@printer pp_init]
      | Zeros
      | Uninit of qty
      | Any
      | Lazy

    type t = value * Tree_borrow.tb_state

    let pp_value ft =
      let open Fmt in
      function
      | Zeros -> pf ft "Zeros"
      | Uninit qty -> pf ft "Uninit %a" pp_qty qty
      | Lazy -> pf ft "Lazy"
      | Init mv -> pp_init ft mv
      | Any -> pf ft "Any"

    let pp ft (v, _) = pp_value ft v

    let merge ~left:(v1, tb1) ~right:(v2, tb2) =
      let tb = Tree_borrow.merge tb1 tb2 in
      match (v1, v2) with
      | Zeros, Zeros -> (Zeros, tb)
      | Uninit Totally, Uninit Totally -> (Uninit Totally, tb)
      | Uninit _, Uninit _ -> (Uninit Partially, tb)
      | _, _ -> (Lazy, tb)

    let split ~(at : T.sint Typed.t) (node, tb) =
      let open TB.Split_tree in
      match node with
      | (Uninit Totally | Zeros | Any) as v ->
          return (Leaf (v, tb), Leaf (v, tb))
      | Init (value, ty) ->
          let rec aux = function
            | `Leaf (value, ty) -> Leaf (Init (value, ty), tb)
            | `Node (at, l, r) -> Node (aux l, at, aux r)
          in
          let+ ltree, rtree = Encoder.split value ty at in
          (aux ltree, aux rtree)
      | Lazy | Uninit Partially ->
          failwith "Should never split an intermediate node"

    type serialized =
      | SInit of (rust_val * Types.ty)
          [@printer Fmt.(pair ~sep:comma pp_rust_val Charon_util.pp_ty)]
      | SUninit
      | SZeros
      | SAny
    [@@deriving show { with_path = false }]

    let subst_serialized f = function
      | SInit (v, ty) -> SInit (Rust_val.subst Sptr.subst f v, ty)
      | v -> v

    let iter_vars_serialized v f =
      match v with
      | SInit (v, _) -> Rust_val.iter_vars Sptr.iter_vars v f
      | _ -> ()

    (* TODO: serialize tree borrow information! *)
    let serialize ((t, _) : t) : serialized Seq.t option =
      match t with
      | Init v -> Some (Seq.return (SInit v))
      | Uninit Totally -> Some (Seq.return SUninit)
      | Zeros -> Some (Seq.return SZeros)
      | Any -> Some (Seq.return SAny)
      | Lazy | Uninit Partially -> None

    type tree = (t, T.sint Typed.t) TB.tree

    let not_owned (t : tree) : tree =
      { t with node = NotOwned Totally; children = None }

    let owned (t : tree) (v : value) : tree =
      let tb =
        match t.node with
        | NotOwned _ -> Tree_borrow.empty_state
        | Owned (_, tb) -> tb
      in
      { t with node = Owned (v, tb); children = None }

    let consume (s : serialized) (t : tree) : (tree, 'e, 'f) Result.t =
      match (s, t.node) with
      | _, NotOwned _ -> miss []
      (* init *)
      | SInit _, _ -> not_impl "Consume typed value on rust_val equality."
      (* any *)
      | SAny, Owned _ -> ok (not_owned t)
      (* uninit *)
      | SUninit, Owned (Uninit Totally, _) -> ok (not_owned t)
      | SUninit, _ -> vanish ()
      (* zeros *)
      | SZeros, Owned (Zeros, _) -> ok (not_owned t)
      | SZeros, Owned (Init _, _) -> not_impl "Assume rust_val == 0s"
      | SZeros, _ -> vanish ()

    let produce (s : serialized) (t : tree) : tree Symex.t =
      match (s, t.node) with
      | _, (Owned _ | NotOwned Partially) -> vanish ()
      | SInit v, NotOwned Totally -> return (owned t (Init v))
      | SZeros, NotOwned Totally -> return (owned t Zeros)
      | SUninit, NotOwned Totally -> return (owned t (Uninit Totally))
      | SAny, NotOwned Totally -> return (owned t Any)

    let assert_exclusively_owned _ = Result.ok ()
  end

  open MemVal
  include Soteria.Sym_states.Tree_block.Make (DecayMapMonad) (MemVal)

  let decode_mem_val ~ty = function
    | Uninit _ -> Result.error `UninitializedMemoryAccess
    | Zeros ->
        let+ v =
          of_opt_not_impl "Don't know how to zero this type"
          @@ Layout.zeroed ~null_ptr:(Sptr.null_ptr ()) ty
        in
        Ok v
    | Lazy -> not_impl "Lazy memory access, cannot decode"
    | Init (value, tyw) ->
        if Types.equal_ty ty tyw then Result.ok value
        else (
          L.debug (fun m ->
              m "Transmuting %a to %a" Charon_util.pp_ty tyw Charon_util.pp_ty
                ty);
          Encoder.transmute ~from_ty:tyw ~to_ty:ty value)
    | Any ->
        (* We don't know if this read is valid, as memory could be uninitialised.
         We have to approximate and vanish. *)
        L.info (fun m -> m "Reading from Any memory, vanishing.");
        vanish ()

  let collect_leaves (t : Tree.t) =
    Result.fold_iter (Tree.iter_leaves_rev t) ~init:[] ~f:(fun vs leaf ->
        let offset, _ = leaf.range in
        let offset = offset -!@ fst t.range in
        match leaf.node with
        | NotOwned Totally -> miss_no_fix ~reason:"decode" ()
        | Owned (Uninit Totally, _) -> Result.error `UninitializedMemoryAccess
        | Owned (Zeros, _) ->
            let* ty =
              match Typed.kind (Range.size leaf.range) with
              | BitVec size -> return (Layout.size_to_uint (Z.to_int size))
              | _ -> not_impl "Don't know how to read this size"
            in
            let+ value =
              of_opt_not_impl "Don't know how to zero this type"
              @@ Layout.zeroed ~null_ptr:(Sptr.null_ptr ()) ty
            in
            Ok (Encoder.{ value; ty; offset } :: vs)
        | Owned (Init (value, ty), _) ->
            Result.ok (Encoder.{ value; ty; offset } :: vs)
        | Owned (Any, _) ->
            L.info (fun m -> m "Reading from Any memory, vanishing.");
            vanish ()
        | NotOwned Partially | Owned ((Lazy | Uninit Partially), _) ->
            L.debug (fun m -> m "Iterating over an intermediate node?");
            vanish ())

  let decode_tree ~ty (t : Tree.t) =
    match t.node with
    | NotOwned _ -> miss []
    | Owned (Lazy, _) ->
        let** leaves = collect_leaves t in
        Encoder.transmute_many ~to_ty:ty leaves
    | Owned (node, _) -> decode_mem_val ~ty node

  let uninit range tb : Tree.t =
    Tree.make ~node:(Owned (Uninit Totally, tb)) ~range ()

  let zeros range tb : Tree.t = Tree.make ~node:(Owned (Zeros, tb)) ~range ()

  let as_owned t f =
    match t.node with
    | NotOwned _ -> miss_no_fix ~reason:"as_owned" ()
    | Owned (v, tb) -> f (v, tb)

  let load ~(ignore_borrow : bool) (ofs : [< T.sint ] Typed.t) (ty : Types.ty)
      (tag : Tree_borrow.tag) (tb : Tree_borrow.t) (t : t option) :
      (rust_val * t option, 'err, 'fix) Result.t =
    let*^ size = Layout.size_of_s ty in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes () =
      let+^ v = Layout.nondet ty in
      [ [ MemVal { offset = ofs; len = size; v = SInit (v, ty) } ] ]
    in
    let@ t = with_bound_and_owned_check ~mk_fixes t bound in
    let replace_node t =
      let@ v, tb_st = as_owned t in
      let++^ tb_st' =
        if ignore_borrow then Rustsymex.Result.ok tb_st
        else Tree_borrow.access tb tag Read tb_st
      in
      { t with node = Owned (v, tb_st') }
    in
    let rebuild_parent = Tree.with_children in
    let** framed, tree =
      Tree.frame_range t ~replace_node ~rebuild_parent range
    in
    let++ sval = decode_tree ~ty framed in
    (sval, tree)

  let store (ofs : [< T.sint ] Typed.t) (ty : Types.ty) (value : rust_val)
      (tag : Tree_borrow.tag) (tb : Tree_borrow.t) (t : t option) :
      (unit * t option, 'err, 'fix) Result.t =
    let*^ size = Layout.size_of_s ty in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node t =
      let@ _, tb_st = as_owned t in
      let++^ tb_st' = Tree_borrow.access tb tag Write tb_st in
      { node = Owned (Init (value, ty), tb_st'); range; children = None }
    in
    let rebuild_parent = Tree.of_children in
    let** node, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
    let++ () =
      match node.node with
      | NotOwned _ -> miss_no_fix ~reason:"store" ()
      | _ -> Result.ok ()
    in
    ((), tree)

  let uninit_range (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (t : t option) : (unit * t option, 'err, 'fix) Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node t =
      let@ _ = as_owned t in
      Tree.map_leaves t @@ fun tt ->
      match tt.node with
      | Owned (_, tb) -> Result.ok (uninit tt.range tb)
      | _ -> assert false
    in
    let rebuild_parent = Tree.of_children in
    let++ _, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
    ((), tree)

  let zero_range (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (t : t option) : (unit * t option, 'err, 'fix) Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node t =
      let@ _, tb = as_owned t in
      (* Is there something to do with the tree borrow here? *)
      Result.ok @@ zeros range tb
    in
    let rebuild_parent = Tree.of_children in
    let++ _, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
    ((), tree)

  (* Tree borrow updates *)

  let protect ofs size tag tb t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node t =
      let@ v, tb_st = as_owned t in
      (* We need to do two things: protect this tag for the block, and perform a read, as
         all function calls perform one on the parameters. *)
      let tb_st' = Tree_borrow.set_protector ~protected:true tb tag tb_st in
      let++^ tb_st' = Tree_borrow.access tb tag Tree_borrow.Read tb_st' in
      { t with node = Owned (v, tb_st') }
    in
    let rebuild_parent = Tree.of_children in
    let++ _, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
    ((), tree)

  let unprotect ofs size tag tb t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node t =
      let@ v, tb_st = as_owned t in
      (* We need to do two things: protect this tag for the block, and perform a read, as
         all function calls perform one on the parameters. *)
      let tb_st' = Tree_borrow.set_protector ~protected:false tb tag tb_st in
      Result.ok { t with node = Owned (v, tb_st') }
    in
    let rebuild_parent = Tree.of_children in
    let++ _, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
    ((), tree)

  let tb_access (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (tag : Tree_borrow.tag) (tb : Tree_borrow.t) (t : t option) :
      (unit * t option, 'err, 'fix) Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node t =
      let@ _ = as_owned t in
      Tree.map_leaves t @@ fun tt ->
      match tt.node with
      | Owned (v, tb_st) ->
          let++^ tb_st' = Tree_borrow.access tb tag Read tb_st in
          { tt with node = Owned (v, tb_st') }
      | _ -> assert false
    in
    let rebuild_parent = Tree.with_children in
    let++ _, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
    ((), tree)

  let alloc ?(zeroed = false) size =
    let st = if zeroed then Zeros else Uninit Totally in
    alloc (st, Tree_borrow.empty_state) size
end
