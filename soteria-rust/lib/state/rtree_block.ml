open Soteria.Symex.Compo_res
open Typed.Infix
module BV = Typed.BV
open Charon
open Syntaxes.FunctionWrap
module DecayMapMonad = Sptr.DecayMapMonad
open DecayMapMonad
open Result
open Syntax

module Make (Tree_borrows : Tree_borrows.M(DecayMapMonad).S) (Sptr : Sptr.S) =
struct
  module Encoder = Value_codec.Encoder (Sptr)

  type rust_val = Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp Sptr.pp

  module MemVal = struct
    module TB = Soteria.Sym_states.Tree_block
    module S_bool = Typed.Bool

    module S_bounded_int = struct
      include Typed
      include Typed.BV

      type t = Typed.T.sint

      let of_z = Typed.BitVec.usize
      let zero () = of_z Z.zero
      let one () = of_z Z.one
      let lt = Typed.Infix.( <$@ )
      let leq = Typed.Infix.( <=$@ )

      (* We assume addition/overflow within the range of an allocation may never
         overflow. This allows extremely good reductions around inequalities,
         which Tree_block relies on. *)
      let add = Typed.Infix.( +!!@ )
      let sub = Typed.Infix.( -!!@ )

      let is_in_bound (v : t Typed.t) : sbool Typed.t =
        let max = Layout.max_value_z (TInt Isize) in
        let max = Typed.BitVec.usize max in
        v <=@ max
    end

    type qty = Totally | Partially [@@deriving show { with_path = false }]
    type leaf = Init of rust_val | Zeros | Uninit | Any | Unowned
    type t = Leaf of leaf * Tree_borrows.tb_state option | Lazy

    let pp_leaf ft =
      let open Fmt in
      function
      | Zeros -> pf ft "Zeros"
      | Uninit -> pf ft "Uninit"
      | Init mv -> pp_rust_val ft mv
      | Any -> pf ft "Any"
      | Unowned -> pf ft "Unowned"

    let pp ft =
      let open Fmt in
      function
      | Leaf (leaf, tb) ->
          pf ft "Leaf (%a, %a)" pp_leaf leaf
            (option ~none:(any "-") Tree_borrows.pp_tb_state)
            tb
      | Lazy -> pf ft "Lazy"

    let merge ~left ~right =
      match (left, right) with
      | Leaf (Unowned, tb_l), Leaf (Unowned, tb_r)
      | Leaf (Zeros, tb_l), Leaf (Zeros, tb_r)
      | Leaf (Uninit, tb_l), Leaf (Uninit, tb_r)
      | Leaf (Any, tb_l), Leaf (Any, tb_r)
        when Tree_borrows.equal_state tb_l tb_r ->
          left
      | _, _ -> Lazy

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
      | Float f ->
          let* v = Encoder.float_to_bv_bits f in
          split_rval (Int v) at
      | Int v ->
          (* get our starting size and unsigned integer *)
          let size = Typed.size_of_int v / 8 in
          let+ at =
            match BV.to_z at with
            | Some at -> return (Z.to_int at)
            | _ -> (
                (* as per the contract of [split], we assume [at] is in [[1,
                   size)] *)
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

    let split ~at node =
      match node with
      | Leaf ((Uninit | Zeros | Any | Unowned), _) ->
          return TB.Split_tree.(Leaf node, Leaf node)
      | Leaf (Init value, tb) ->
          let+ vl, vr = split_rval value at in
          let ll = Leaf (Init vl, tb) in
          let lr = Leaf (Init vr, tb) in
          TB.Split_tree.(Leaf ll, Leaf lr)
      | Lazy -> failwith "Should never split an intermediate node"

    type serialized =
      | SInit of rust_val
      | SUninit
      | SZeros
      | SAny
      | STree_borrow_st of Tree_borrows.serialized_state
      | STree_borrow of Tree_borrows.serialized
    [@@deriving show { with_path = false }]

    let lift_tb_st_fix s = STree_borrow_st s

    let lift_tb_st_miss tb_s =
      let+? tb_s in
      List.map lift_tb_st_fix tb_s

    let lift_tb_fix s = STree_borrow s

    let lift_tb_miss tb_s =
      let+? tb_s in
      List.map
        (function
          | Tree_borrows.Structure s -> lift_tb_fix s
          | Tree_borrows.State s -> lift_tb_st_fix s)
        tb_s

    let subst_serialized f = function
      | SInit v -> SInit (Rust_val.subst Sptr.subst f v)
      | STree_borrow_st tb ->
          STree_borrow_st (Tree_borrows.subst_serialized_state f tb)
      | STree_borrow s -> STree_borrow (Tree_borrows.subst_serialized f s)
      | (SUninit | SZeros | SAny) as v -> v

    let iter_vars_serialized v f =
      match v with
      | SInit v -> Rust_val.iter_vars Sptr.iter_vars v f
      | STree_borrow_st s -> Tree_borrows.iter_vars_serialized_state s f
      | STree_borrow s -> Tree_borrows.iter_vars_serialized s f
      | SUninit | SZeros | SAny -> ()

    let serialize : t -> serialized Seq.t option = function
      | Leaf (Unowned, Some tb) ->
          Some
            (Tree_borrows.serialize_state tb
            |> List.map lift_tb_st_fix
            |> List.to_seq)
      | Leaf (Unowned, None) -> failwith "Impossible: unowned with no TB state"
      | Leaf (leaf, tb) ->
          let leaf_ser =
            match leaf with
            | Init v -> SInit v
            | Uninit -> SUninit
            | Zeros -> SZeros
            | Any -> SAny
            | Unowned -> assert false
          in
          let tb_ser =
            Option.fold ~none:[] ~some:Tree_borrows.serialize_state tb
            |> List.map lift_tb_st_fix
            |> List.to_seq
          in
          Some (Seq.cons leaf_ser tb_ser)
      | Lazy -> None

    let mk_fix_typed ty () =
      let+^ v = Encoder.nondet_valid ty in
      (* we're basically guaranteed this won't error (ie. layout error) by now,
         so we can safely unwrap. *)
      let v = get_ok v in
      [ SInit v ]

    let mk_fix_any () = [ Any ]

    type tree = (t, Typed.(T.sint t)) TB.tree

    let mk_leaf (t : tree) (v : leaf) tb : tree =
      let node =
        match (v, tb) with
        | Unowned, None -> TB.NotOwned Totally
        | _, _ -> TB.Owned (Leaf (v, tb))
      in
      { range = t.range; node; children = None }

    let consume (s : serialized) (t : tree) : (tree, 'e, 'f) Result.t =
      match (s, t.node) with
      | _, NotOwned _ -> miss_no_fix ~reason:"rtree_block consume notowned" ()
      | _, Owned Lazy -> not_impl "Consume on lazy node"
      (* init *)
      | SInit _, _ -> not_impl "Consume typed value on rust_val equality."
      (* any *)
      | SAny, Owned (Leaf (_, tb)) -> ok (mk_leaf t Unowned tb)
      (* uninit *)
      | SUninit, Owned (Leaf (Uninit, tb)) -> ok (mk_leaf t Unowned tb)
      | SUninit, Owned (Leaf _) -> vanish ()
      (* zeros *)
      | SZeros, Owned (Leaf (Zeros, tb)) -> ok (mk_leaf t Unowned tb)
      | SZeros, Owned (Leaf (Init _, _)) -> not_impl "Assume rust_val == 0s"
      | SZeros, Owned (Leaf _) -> vanish ()
      (* tree borrows *)
      | STree_borrow_st s, Owned (Leaf (v, tb)) ->
          let++ tb' = lift_tb_st_miss @@ Tree_borrows.consume_state s tb in
          mk_leaf t v tb'
      | STree_borrow _, _ -> not_impl "Consume TB structure in tree block?"

    let rec produce (s : serialized) (t : tree) : tree DecayMapMonad.t =
      match (s, t.node) with
      | ( (SInit _ | SZeros | SUninit | SAny),
          (NotOwned Totally | Owned (Leaf (Unowned, _))) ) ->
          let v =
            match s with
            | SInit v -> Init v
            | SZeros -> Zeros
            | SUninit -> Uninit
            | SAny -> Any
            | _ -> assert false
          in
          let tb =
            match t.node with
            | Owned (Leaf (Unowned, tb)) -> tb
            | NotOwned Totally -> None
            | _ -> assert false
          in
          return (mk_leaf t v tb)
      | (SInit _ | SZeros | SUninit | SAny), Owned (Leaf _) -> vanish ()
      | (SInit _ | SZeros | SUninit | SAny), (Owned Lazy | NotOwned Partially)
        ->
          (* FIXME: this is not correct if the leaves of the tree only contain
             tree borrows information (i.e. they are all [NotOwned Totally] or
             [Owned (Leaf Unowned, _)]). For now we just do a sanity check and
             fail if our assumption is wrong. *)
          let rec check_has_value (t : tree) =
            match t.node with
            | Owned (Leaf (Unowned, _)) | NotOwned Totally -> return ()
            | Owned (Leaf _) -> vanish ()
            | Owned Lazy | NotOwned Partially ->
                let l, r = Option.get t.children in
                let* () = check_has_value l in
                check_has_value r
          in
          let* () = check_has_value t in
          failwith "assumption was wrong; unsound! this should be unreachable"
      (* Tree borrows: we produce recursively, as we don't want to merge the
         leaves *)
      | STree_borrow_st s, NotOwned Totally ->
          let+ tb = Tree_borrows.produce_state s None in
          mk_leaf t Unowned tb
      | STree_borrow_st s, Owned (Leaf (v, tb)) ->
          let+ tb = Tree_borrows.produce_state s tb in
          mk_leaf t v tb
      | STree_borrow_st _, (Owned Lazy | NotOwned Partially) ->
          let l, r = Option.get t.children in
          let* l = produce s l in
          let+ r = produce s r in
          { t with children = Some (l, r) }
      | STree_borrow _, _ -> not_impl "Produce TB structure in tree block?"

    let assert_exclusively_owned _ = Result.ok ()
  end

  open MemVal
  include Soteria.Sym_states.Tree_block.Make (DecayMapMonad) (MemVal)

  module Tree = struct
    include Tree

    let map_leaves_tb f (t : t) =
      map_leaves t @@ fun leaf ->
      match leaf.node with
      | NotOwned Totally -> failwith "impossible: iterating over non-owned node"
      | NotOwned Partially | Owned Lazy ->
          failwith "impossible: iterating over intermediate node"
      | Owned (Leaf (v, tb)) ->
          let++ tb' = lift_tb_miss @@ f tb in
          { leaf with node = Owned (Leaf (v, tb')) }

    let iter_leaves_rev (t : t) =
      iter_leaves_rev t
      |> Iter.filter_map @@ fun leaf ->
         match leaf.node with
         | NotOwned Totally ->
             failwith "impossible: iterating over non-owned node"
         | NotOwned Partially | Owned Lazy ->
             failwith "impossible: iterating over intermediate node"
         | Owned (Leaf (v, tb)) -> Some (leaf.range, v, tb)
  end

  let lift_symex x = SM.lift @@ DecayMapMonad.lift x

  let sint_to_int v =
    match BV.to_z v with
    | Some z -> return (Z.to_int z)
    | None -> not_impl "Cannot convert size to int"

  let mk_fix_typed ofs ty () =
    let*^ len = Layout.size_of ty in
    let len = get_ok len in
    let+ fixes = mk_fix_typed ty () in
    List.map (fun v -> [ MemVal { offset = ofs; len; v } ]) fixes

  let mk_fix_any ofs len () = [ [ MemVal { offset = ofs; len; v = SAny } ] ]
  let mk_fix_any_s ofs len () = return (mk_fix_any ofs len ())

  let mk_fix_tb ofs len () =
    return
      [
        List.map
          (fun v -> MemVal { offset = ofs; len; v = MemVal.lift_tb_st_fix v })
          (Tree_borrows.fix_empty_state ());
      ]

  let collect_leaves ~uninit (t : Tree.t) =
    Result.fold_iter (Tree.iter_leaves_rev t) ~init:[]
      ~f:(fun vs (range, v, _tb) ->
        let offset, _ = range in
        let offset = offset -!@ fst t.range in
        match v with
        | Uninit -> (
            match uninit with
            | `Ignore -> Result.ok vs
            | `Error -> Result.error `UninitializedMemoryAccess)
        | Zeros ->
            let+ size = sint_to_int (Range.size range) in
            let value = BV.zero (size * 8) in
            Ok ((Rust_val.Int value, offset) :: vs)
        | Init value -> Result.ok ((value, offset) :: vs)
        | Any ->
            L.info (fun m -> m "Reading from Any memory, vanishing.");
            vanish ()
        | Unowned -> miss (mk_fix_any offset (Range.size range) ()))

  let decode_mem_val ~ty = function
    | Init value ->
        let+ res = Encoder.transmute_one ~to_ty:ty value in
        Ok res
    | Zeros ->
        let**^ size = Layout.size_of ty in
        let* size = sint_to_int size in
        let zero = BV.zero (size * 8) in
        let+ res = Encoder.transmute_one ~to_ty:ty (Int zero) in
        Ok res
    | Uninit -> Result.error `UninitializedMemoryAccess
    | Any ->
        (* We don't know if this read is valid, as memory could be
           uninitialised. We have to approximate and vanish. *)
        not_impl "Reading from Any memory, vanishing."
    | Unowned -> failwith "Unowned leaf, should have been caught before"

  let decode_lazy ~ty (t : Tree.t) =
    (* The tree spans the entire type we're interested in. Furthermore, we only
       read/write scalars (int, float, pointers...) which cover the whole range
       with no gaps. For lazy nodes, we convert all of these to bitvectors, the
       concatenate them and call the encoder to decode the full value. *)
    let** leaves = collect_leaves ~uninit:`Error t in
    let* leaves =
      DecayMapMonad.map_list leaves ~f:(fun (v, _) ->
          match v with
          | Int bv -> return bv
          | Ptr (ptr, Thin) -> Sptr.decay ptr
          | Float f -> Encoder.float_to_bv_bits f
          | _ ->
              Fmt.kstr not_impl "Unexpected rust_val in lazy decoding: %a"
                pp_rust_val v)
    in
    match List.rev leaves with
    | hd :: tl ->
        let bv = List.fold_left BV.concat hd tl in
        let+ res = Encoder.transmute_one ~to_ty:ty (Int bv) in
        Ok res
    | _ -> failwith "Impossible"

  let decode_tree ~ty (t : Tree.t) =
    match t.node with
    | NotOwned _ -> miss []
    | Owned Lazy -> decode_lazy ~ty t
    | Owned (Leaf (node, _)) -> decode_mem_val ~ty node

  let merge_tree_borrows t =
    DecayMapMonad.Result.fold_iter ~init:None
      ~f:(fun acc ((offset, len), _, tb_st) ->
        match (tb_st, acc) with
        | None, _ ->
            (* Missing state; we must miss *)
            lift_miss ~offset ~len
            @@ lift_tb_st_miss
            @@ Result.miss [ Tree_borrows.fix_empty_state () ]
        | Some _, None -> Result.ok tb_st
        | Some tb_st, Some acc ->
            let+ res = Tree_borrows.merge tb_st acc in
            Ok (Some res))
      (Tree.iter_leaves_rev t)

  let init range v tb : Tree.t =
    Tree.make ~node:(Owned (Leaf (Init v, tb))) ~range ()

  let uninit range tb : Tree.t =
    Tree.make ~node:(Owned (Leaf (Uninit, tb))) ~range ()

  let zeros range tb : Tree.t =
    Tree.make ~node:(Owned (Leaf (Zeros, tb))) ~range ()

  let as_owned ?mk_fixes t f =
    match (t.node, mk_fixes) with
    | Owned _, _ -> f t
    | NotOwned _, None -> miss_no_fix ~reason:"as_owned" ()
    | NotOwned _, Some mk_fixes ->
        let+ fixes = mk_fixes () in
        Missing fixes

  let check_owned (ofs : Typed.([< T.sint ] t))
      (size : Typed.([< T.nonzero ] t)) =
    let _, bound = Range.of_low_and_size ofs (Typed.cast size) in
    with_bound_check bound (fun t -> DecayMapMonad.Result.ok ((), t))

  (* Memory operations *)

  let load ~(ignore_borrow : bool) (ofs : Typed.([< T.sint ] t)) (ty : Types.ty)
      (tag : Tree_borrows.tag option) (tb : Tree_borrows.t option) =
    let open SM.Syntax in
    let** size = lift_symex @@ Layout.size_of ty in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_typed ofs ty in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          match (ignore_borrow, tag) with
          | false, Some tag ->
              lift_miss ~offset:ofs ~len:bound
              @@ Tree.map_leaves_tb (Tree_borrows.access tag Read tb) t
          | true, _ | _, None -> Result.ok t
        in
        let rebuild_parent = Tree.of_children in
        let** framed, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        let++ sval = decode_tree ~ty framed in
        (sval, tree))

  let store (ofs : Typed.([< T.sint ] t)) (value : rust_val)
      (tag : Tree_borrows.tag option) (tb : Tree_borrows.t option) :
      (unit, 'err, 'fix) SM.Result.t =
    let open SM.Syntax in
    let** size = lift_symex @@ Value_codec.size_of value in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          let** tb_st = merge_tree_borrows t in
          match tag with
          | Some tag ->
              let++ tb_st' =
                lift_miss ~offset:ofs ~len:bound
                @@ lift_tb_miss
                @@ Tree_borrows.access tag Write tb tb_st
              in
              init range value tb_st'
          | None -> ok (init range value tb_st)
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let get_init_leaves (ofs : Typed.([< T.sint ] t))
      (size : Typed.([< T.nonzero ] t)) :
      ((rust_val * Typed.(T.sint t)) list, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs (Typed.cast size) in
    with_bound_check bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node node = Result.ok node in
        let rebuild_parent = Tree.with_children in
        let** framed, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        let++ leaves = collect_leaves ~uninit:`Ignore framed in
        (leaves, tree))

  let uninit_range (ofs : Typed.([< T.sint ] t)) (size : Typed.([< T.sint ] t))
      : (unit, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ _ = as_owned ~mk_fixes t in
          let++ tb_st = merge_tree_borrows t in
          uninit range tb_st
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let zero_range (ofs : Typed.([< T.sint ] t)) (size : Typed.([< T.sint ] t)) :
      (unit, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          let++ tb_st = merge_tree_borrows t in
          zeros range tb_st
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let alloc ?(zeroed = false) size =
    let st = if zeroed then Zeros else Uninit in
    let+ tb_st = Tree_borrows.init_st () in
    alloc (Leaf (st, Some tb_st)) size

  (* Tree borrow updates *)

  let with_tb_access (ofs : Typed.([< T.sint ] t))
      (size : Typed.([< T.sint ] t)) f =
    (* TODO: figure out [mk_fixes] for tree borrows state! *)
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_tb ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ t = as_owned t in
          lift_miss ~offset:ofs ~len:bound @@ Tree.map_leaves_tb f t
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let unprotect ofs size tag tb =
    with_tb_access ofs size (Tree_borrows.set_protector ~protected:false tag tb)

  let tb_access ofs size tag tb =
    with_tb_access ofs size (Tree_borrows.access tag Read tb)
end
