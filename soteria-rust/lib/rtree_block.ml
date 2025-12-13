open Soteria.Symex.Compo_res
open Typed
open Typed.Infix
module BV = Typed.BitVec
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

    module SBoundedInt = struct
      include Typed
      include Typed.Infix

      type sint = T.sint
      type sbool = T.sbool

      let zero () = Typed.BitVec.usize Z.zero
      let ( <@ ) = Typed.Infix.( <$@ )
      let ( <=@ ) = Typed.Infix.( <=$@ )

      (* We assume addition/overflow within the range of an allocation may never overflow.
         This allows extremely good reductions around inequalities, which Tree_block relies on.  *)
      let ( +@ ) = Typed.Infix.( +!!@ )
      let ( -@ ) = Typed.Infix.( -!!@ )

      let in_bound (v : sint Typed.t) : sbool Typed.t =
        let max = Layout.max_value_z (TInt Isize) in
        let max = Typed.BitVec.usize max in
        v >=@ zero () &&@ (v <=@ max)
    end

    type qty = Totally | Partially [@@deriving show { with_path = false }]
    type value = Init of rust_val | Zeros | Uninit of qty | Any | Lazy
    type t = value * Tree_borrow.tb_state

    let pp_value ft =
      let open Fmt in
      function
      | Zeros -> pf ft "Zeros"
      | Uninit qty -> pf ft "Uninit %a" pp_qty qty
      | Lazy -> pf ft "Lazy"
      | Init mv -> pp_rust_val ft mv
      | Any -> pf ft "Any"

    let pp ft (v, _) = pp_value ft v

    let merge ~left:(v1, tb1) ~right:(v2, tb2) =
      let tb = Tree_borrow.merge tb1 tb2 in
      match (v1, v2) with
      | Zeros, Zeros -> (Zeros, tb)
      | Uninit Totally, Uninit Totally -> (Uninit Totally, tb)
      | Uninit _, Uninit _ -> (Uninit Partially, tb)
      | _, _ -> (Lazy, tb)

    let rec split_rval (v : rust_val) at =
      match v with
      | Ptr (ptr, meta) ->
          let* v = Sptr.decay ptr in
          let* v =
            match meta with
            | Thin -> return v
            | Len len -> return (BV.concat v len)
            | VTable ptr ->
                let+ v2 = Sptr.decay ptr in
                BV.concat v v2
          in
          split_rval (Int v) at
      | Int v ->
          (* get our starting size and unsigned integer *)
          let size = Typed.size_of_int v / 8 in
          let+ at =
            match BV.to_z at with
            | Some at -> return (Z.to_int at)
            | _ -> (
                (* as per the contract of [split], we assume [at] is in [[1, size)] *)
                let options = List.init (size - 1) (( + ) 1) in
                let* res =
                  match_on options ~constr:(fun x ->
                      Typed.sem_eq at (BV.usizei x))
                in
                match res with Some i -> return i | None -> vanish ())
          in
          let mask_l = BV.extract 0 ((at * 8) - 1) v in
          let mask_r = BV.extract (at * 8) ((size * 8) - 1) v in
          (Rust_val.Int mask_l, Rust_val.Int mask_r)
      | _ ->
          Fmt.kstr not_impl "Split unsupported: %a at %a" pp_rust_val v
            Typed.ppa at

    let split ~(at : T.sint Typed.t) (node, tb) =
      let open TB.Split_tree in
      match node with
      | (Uninit Totally | Zeros | Any) as v ->
          return (Leaf (v, tb), Leaf (v, tb))
      | Init value ->
          let+ vl, vr = split_rval value at in
          (Leaf (Init vl, tb), Leaf (Init vr, tb))
      | Lazy | Uninit Partially ->
          failwith "Should never split an intermediate node"

    type serialized = SInit of rust_val | SUninit | SZeros | SAny
    [@@deriving show { with_path = false }]

    let subst_serialized f = function
      | SInit v -> SInit (Rust_val.subst Sptr.subst f v)
      | v -> v

    let iter_vars_serialized v f =
      match v with SInit v -> Rust_val.iter_vars Sptr.iter_vars v f | _ -> ()

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

    let produce (s : serialized) (t : tree) : tree DecayMapMonad.t =
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

  let sint_to_int v =
    match BitVec.to_z v with
    | Some z -> return (Z.to_int z)
    | None -> not_impl "Cannot convert size to int"

  let collect_leaves (t : Tree.t) =
    Result.fold_iter (Tree.iter_leaves_rev t) ~init:[] ~f:(fun vs leaf ->
        let offset, _ = leaf.range in
        let offset = offset -!@ fst t.range in
        match leaf.node with
        | NotOwned Totally -> miss_no_fix ~reason:"decode" ()
        | Owned (Uninit Totally, _) -> Result.ok vs
        | Owned (Zeros, _) ->
            let+ size = sint_to_int (Range.size leaf.range) in
            let value = BitVec.zero (size * 8) in
            Ok ((Rust_val.Int value, offset) :: vs)
        | Owned (Init value, _) -> Result.ok ((value, offset) :: vs)
        | Owned (Any, _) ->
            L.info (fun m -> m "Reading from Any memory, vanishing.");
            vanish ()
        | NotOwned Partially | Owned ((Lazy | Uninit Partially), _) ->
            failwith "Iterating over an intermediate node?")

  let decode_mem_val ~ty = function
    | Init value -> Encoder.transmute_one ~to_ty:ty value
    | Zeros ->
        let**^ size = Layout.size_of ty in
        let* size = sint_to_int size in
        let zero = BV.zero (size * 8) in
        Encoder.transmute_one ~to_ty:ty (Int zero)
    | Uninit _ -> Result.error `UninitializedMemoryAccess
    | Any ->
        (* We don't know if this read is valid, as memory could be uninitialised.
         We have to approximate and vanish. *)
        not_impl "Reading from Any memory, vanishing."
    | Lazy -> failwith "Should have been handled earlier"

  let decode_lazy ~ty (t : Tree.t) =
    (* The tree spans the entire type we're interested in.
       Furthermore, we only read/write scalars (int, float, pointers...) which
       cover the whole range with no gaps.
       For lazy nodes, we convert all of these to bitvectors, the concatenate
       them and call the encoder to decode the full value. *)
    let** leaves = collect_leaves t in
    let** leaves =
      fold_list leaves ~init:[] ~f:(fun acc (v, _) ->
          match v with
          | Int bv -> ok (bv :: acc)
          | Ptr (ptr, Thin) ->
              let+ bv = Sptr.decay ptr in
              Ok (bv :: acc)
          | Float f ->
              let+ bv = Encoder.float_to_bv_bits f in
              Ok (bv :: acc)
          | _ ->
              Fmt.kstr not_impl "Unexpected rust_val in lazy decoding: %a"
                pp_rust_val v)
    in
    match leaves with
    | hd :: tl ->
        let bv = List.fold_left BV.concat hd tl in
        Encoder.transmute_one ~to_ty:ty (Int bv)
    | _ -> failwith "Impossible"

  let decode_tree ~ty (t : Tree.t) =
    match t.node with
    | NotOwned _ -> miss []
    | Owned (Lazy, _) -> decode_lazy ~ty t
    | Owned (node, _) -> decode_mem_val ~ty node

  let uninit range tb : Tree.t =
    Tree.make ~node:(Owned (Uninit Totally, tb)) ~range ()

  let zeros range tb : Tree.t = Tree.make ~node:(Owned (Zeros, tb)) ~range ()

  let as_owned t f =
    match t.node with
    | NotOwned _ -> miss_no_fix ~reason:"as_owned" ()
    | Owned (v, tb) -> f (v, tb)

  let load ~(ignore_borrow : bool) (ofs : [< T.sint ] Typed.t) (ty : Types.ty)
      (tag : Tree_borrow.tag option) (tb : Tree_borrow.t) (t : t option) :
      (rust_val * t option, 'err, 'fix) Result.t =
    let**^ size = Layout.size_of ty in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes () =
      let+^ v = Layout.nondet ty in
      (* we're basically guaranteed this won't error (ie. layout error) by now,
         so we can safely unwrap. *)
      let v = get_ok v in
      [ [ MemVal { offset = ofs; len = size; v = SInit v } ] ]
    in
    let@ t = with_bound_and_owned_check ~mk_fixes t bound in
    let replace_node t =
      let@ v, tb_st = as_owned t in
      let++^ tb_st' =
        match (ignore_borrow, tag) with
        | false, Some tag -> Tree_borrow.access tag Read tb tb_st
        | true, _ | _, None -> Rustsymex.Result.ok tb_st
      in
      { t with node = Owned (v, tb_st') }
    in
    let rebuild_parent = Tree.with_children in
    let** framed, tree =
      Tree.frame_range t ~replace_node ~rebuild_parent range
    in
    let++ sval = decode_tree ~ty framed in
    (sval, tree)

  let store (ofs : [< T.sint ] Typed.t) (value : rust_val)
      (tag : Tree_borrow.tag option) (tb : Tree_borrow.t) (t : t option) :
      (unit * t option, 'err, 'fix) Result.t =
    let size = Typed.BitVec.usizei @@ Rust_val.size_of value in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node t =
      let@ _, tb_st = as_owned t in
      let++^ tb_st' =
        match tag with
        | Some tag -> Tree_borrow.access tag Write tb tb_st
        | None -> Rustsymex.Result.ok tb_st
      in
      { node = Owned (Init value, tb_st'); range; children = None }
    in
    let rebuild_parent = Tree.of_children in
    let** node, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
    let++ () =
      match node.node with
      | NotOwned _ -> miss_no_fix ~reason:"store" ()
      | _ -> Result.ok ()
    in
    ((), tree)

  let get_init_leaves (ofs : [< T.sint ] Typed.t)
      (size : [< T.nonzero ] Typed.t) (t : t option) :
      (Encoder.cval_info list * t option, 'err, 'fix) Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs (Typed.cast size) in
    let@ t = with_bound_and_owned_check t bound in
    let replace_node node = Result.ok node in
    let rebuild_parent = Tree.with_children in
    let** framed, tree =
      Tree.frame_range t ~replace_node ~rebuild_parent range
    in
    let++ leaves = collect_leaves framed in
    (leaves, tree)

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
      let tb_st' = Tree_borrow.set_protector ~protected:true tag tb tb_st in
      let++^ tb_st' = Tree_borrow.access tag Read tb tb_st' in
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
      let tb_st' = Tree_borrow.set_protector ~protected:false tag tb tb_st in
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
          let++^ tb_st' = Tree_borrow.access tag Read tb tb_st in
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
