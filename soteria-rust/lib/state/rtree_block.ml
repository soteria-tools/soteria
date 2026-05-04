open Compo_res
open Typed.Infix
module BV = Typed.BV
open Charon
open Syntaxes.FunctionWrap
module DecayMap = Sptr.DecayMap
open DecayMap.SM
open Result
open Syntax

module Make (Borrows : Tree_borrows.M(DecayMap.SM).S) (Sptr : Sptr.S) = struct
  module Encoder = Value_codec.Encoder (Sptr)

  type rust_val = Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp Sptr.pp

  module MemVal = struct
    module TB = Soteria.Sym_states.Tree_block
    module S_bool = Typed.Bool

    module S_int = struct
      include Typed
      include Typed.BV

      type t = Typed.T.sint Typed.t [@@deriving show { with_path = false }]

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

      let is_in_bound (v : t) : sbool Typed.t =
        let max = Layout.max_value_z (TInt Isize) in
        let max = Typed.BitVec.usize max in
        v <=@ max

      type syn = Typed.Expr.t [@@deriving show { with_path = false }]

      let to_syn = Typed.Expr.of_value
      let subst = Typed.Expr.subst
      let learn_eq = Consumer.learn_eq
      let exprs_syn x = [ x ]
      let fresh () = nondet (Typed.t_usize ())
    end

    type qty = Totally | Partially [@@deriving show { with_path = false }]
    type leaf = Init of rust_val | Zeros | Uninit | Any | Unowned
    type t = Leaf of leaf * Borrows.State.t option | Lazy

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
            (option ~none:(any "-") Borrows.State.pp)
            tb
      | Lazy -> pf ft "Lazy"

    let merge ~left ~right =
      match (left, right) with
      | Leaf (Unowned, tb_l), Leaf (Unowned, tb_r)
      | Leaf (Zeros, tb_l), Leaf (Zeros, tb_r)
      | Leaf (Uninit, tb_l), Leaf (Uninit, tb_r)
      | Leaf (Any, tb_l), Leaf (Any, tb_r)
        when Borrows.State.equal tb_l tb_r ->
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

    type syn =
      | SInit of Sptr.syn Rust_val.syn
      | SUninit
      | SZeros
      | SAny
      | STree_borrow_st of Borrows.State.syn
      | STree_borrow of Borrows.Tree.syn
    [@@deriving show { with_path = false }]

    let ins_outs = function
      | SInit v -> ([], Rust_val.exprs_syn Sptr.exprs_syn v)
      | SUninit | SZeros | SAny -> ([], [])
      | STree_borrow_st s -> Borrows.State.ins_outs s
      | STree_borrow s -> Borrows.Tree.ins_outs s

    let lift_tb_st_fix s = STree_borrow_st s

    let lift_tb_st_miss tb_s =
      let+? tb_s in
      List.map lift_tb_st_fix tb_s

    let lift_tb_fix s = STree_borrow s

    let lift_tb_miss tb_s =
      let+? tb_s in
      List.map
        (function
          | `Structure s -> lift_tb_fix s | `State s -> lift_tb_st_fix s)
        tb_s

    let to_syn : t -> syn Seq.t option = function
      | Leaf (Unowned, Some tb) ->
          Some
            (Borrows.State.to_syn tb |> List.map lift_tb_st_fix |> List.to_seq)
      | Leaf (Unowned, None) -> failwith "Impossible: unowned with no TB state"
      | Leaf (leaf, tb) ->
          let leaf_ser =
            match leaf with
            | Init v -> SInit ((Rust_val.to_syn Sptr.to_syn) v)
            | Uninit -> SUninit
            | Zeros -> SZeros
            | Any -> SAny
            | Unowned -> assert false
          in
          let tb_ser =
            Option.fold ~none:[] ~some:Borrows.State.to_syn tb
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
      [ SInit (Rust_val.to_syn Sptr.to_syn v) ]

    let mk_fix_any () = [ Any ]

    type tree = (t, Typed.(T.sint t)) TB.tree

    let mk_leaf (t : tree) (v : leaf) tb : tree =
      let node =
        match (v, tb) with
        | Unowned, None -> TB.NotOwned Totally
        | _, _ -> TB.Owned (Leaf (v, tb))
      in
      { range = t.range; node; children = None }

    module Rust_val_consumer = Rust_val.Learn_eq (DecayMap.SM)

    let consume (s : syn) (t : tree) : (tree, syn list) DecayMap.SM.Consumer.t =
      let open DecayMap.SM.Consumer in
      let open Syntax in
      let* v, tb =
        match t.node with
        | NotOwned _ -> miss_no_fix ~reason:"rtree_block consume notowned" ()
        | Owned Lazy -> lift @@ not_impl "Consume on lazy node"
        | Owned (Leaf (v, tb)) -> ok (v, tb)
      in
      let* v =
        match (s, v) with
        (* init *)
        | SInit e, Init v ->
            let+ () = Rust_val_consumer.learn_eq Sptr.learn_eq e v in
            Unowned
        | SInit _, Zeros -> lift @@ not_impl "Assume rust_val.syn == 0s"
        | SInit _, _ -> lfail Typed.v_false
        (* any *)
        | SAny, _ -> ok Unowned
        (* uninit *)
        | SUninit, Uninit -> ok Unowned
        | SUninit, _ -> lfail Typed.v_false
        (* zeros *)
        | SZeros, Zeros -> ok Unowned
        | SZeros, Init _ -> lift @@ not_impl "Assume rust_val == 0s"
        | SZeros, _ -> lfail Typed.v_false
        (* unrelated to value *)
        | (STree_borrow_st _ | STree_borrow _), _ -> ok v
      in
      let+ tb =
        match s with
        | STree_borrow_st s ->
            let+? fixes = Borrows.State.consume s tb in
            List.map lift_tb_st_fix fixes
        | STree_borrow _ ->
            failwith
              "TB structure syn in tree block, should have been caught before"
        (* unrelated to tree borrows *)
        | SInit _ | SZeros | SUninit | SAny -> ok tb
      in
      mk_leaf t v tb

    let rec produce (s : syn) (t : tree) : tree DecayMap.SM.Producer.t =
      let open DecayMap.SM.Producer in
      let open Syntax in
      match (s, t.node) with
      | ( (SInit _ | SZeros | SUninit | SAny),
          (NotOwned Totally | Owned (Leaf (Unowned, _))) ) ->
          let* v =
            match s with
            | SInit v ->
                let+ v = Producer.apply_subst (Rust_val.subst Sptr.subst) v in
                Init v
            | SZeros -> return Zeros
            | SUninit -> return Uninit
            | SAny -> return Any
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
          let l, r = Option.get t.children in
          let* sl, sr =
            match s with
            | SZeros | SUninit | SAny -> return (s, s)
            | SInit v ->
                let* v = Producer.apply_subst (Rust_val.subst Sptr.subst) v in
                let _, middle = l.range in
                let+^ vl, vr = split_rval v middle in
                (* HACK: is this sound? Doing it this way because we can't have
                   a split_rval for exprs, given it requires decaying pointers.
                   An alternative would be to have a [produce_leaf] which takes
                   in a [Leaf _], so we only subst once. *)
                let vl = Rust_val.to_syn Sptr.to_syn vl in
                let vr = Rust_val.to_syn Sptr.to_syn vr in
                (SInit vl, SInit vr)
            | _ -> assert false
          in
          let* l = produce sl l in
          let+ r = produce sr r in
          let node : 'a TB.node =
            match (l.node, r.node) with
            | NotOwned Totally, NotOwned Totally -> NotOwned Totally
            | NotOwned _, _ | _, NotOwned _ -> NotOwned Partially
            | Owned left, Owned right -> Owned (merge ~left ~right)
          in
          { t with node; children = Some (l, r) }
      (* Tree borrows: we produce recursively, as we don't want to merge the
         leaves *)
      | STree_borrow_st s, NotOwned Totally ->
          let+ tb = Borrows.State.produce s None in
          mk_leaf t Unowned tb
      | STree_borrow_st s, Owned (Leaf (v, tb)) ->
          let+ tb = Borrows.State.produce s tb in
          mk_leaf t v tb
      | STree_borrow_st _, (Owned Lazy | NotOwned Partially) ->
          let l, r = Option.get t.children in
          let* l = produce s l in
          let+ r = produce s r in
          { t with children = Some (l, r) }
      | STree_borrow _, _ ->
          failwith
            "TB structure syn in tree block, should have been caught before"

    let rec assert_exclusively_owned (t : tree) =
      match t.node with
      | NotOwned Totally | Owned (Leaf (Unowned, _)) -> miss [ [ SAny ] ]
      | NotOwned Partially | Owned Lazy ->
          let l, r = Option.get t.children in
          let** () = assert_exclusively_owned l in
          assert_exclusively_owned r
      | Owned (Leaf ((Zeros | Uninit | Any | Init _), tb)) ->
          lift_tb_st_miss @@ Borrows.State.assert_exclusively_owned tb
  end

  open MemVal
  include Soteria.Sym_states.Tree_block.Make (DecayMap.SM) (MemVal)

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

  let lift_symex x = SM.lift @@ DecayMap.SM.lift x

  let sint_to_int v =
    match BV.to_z v with
    | Some z -> return (Z.to_int z)
    | None -> not_impl "Cannot convert size to int"

  let mk_fix_typed offset ty () =
    let*^ len = Layout.size_of ty in
    let len = get_ok len in
    let+ fixes = mk_fix_typed ty () in
    [ lift_fixes ~offset ~len fixes ]

  let mk_fix_any offset len () = [ lift_fixes ~offset ~len [ SAny ] ]
  let mk_fix_any_s ofs len () = return (mk_fix_any ofs len ())

  let mk_fix_tb offset len () =
    return
      [
        lift_fixes ~offset ~len
        @@ List.map MemVal.lift_tb_st_fix (Borrows.State.fix_empty ());
      ]

  let collect_leaves ~uninit (t : Tree.t) =
    fold_iter (Tree.iter_leaves_rev t) ~init:[] ~f:(fun vs (range, v, _tb) ->
        let offset, _ = range in
        let offset = offset -!@ fst t.range in
        match v with
        | Uninit -> (
            match uninit with
            | `Ignore -> ok vs
            | `Error -> error `UninitializedMemoryAccess)
        | Zeros ->
            let+ size = sint_to_int (Range.size range) in
            let value = BV.zero (size * 8) in
            Ok ((Rust_val.Int value, offset) :: vs)
        | Init value -> ok ((value, offset) :: vs)
        | Any ->
            [%l.info "Reading from Any memory, vanishing."];
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
    | Uninit -> error `UninitializedMemoryAccess
    | Any ->
        (* We don't know if this read is valid, as memory could be
           uninitialised. We have to approximate and vanish. *)
        not_impl "Reading from Any memory, vanishing."
    | Unowned ->
        let+ fix = MemVal.mk_fix_typed ty () in
        Missing [ fix ]

  let decode_lazy ~ty (t : Tree.t) =
    (* The tree spans the entire type we're interested in. Furthermore, we only
       read/write scalars (int, float, pointers...) which cover the whole range
       with no gaps. For lazy nodes, we convert all of these to bitvectors, the
       concatenate them and call the encoder to decode the full value. *)
    let** leaves = collect_leaves ~uninit:`Error t in
    let* leaves =
      DecayMap.SM.map_list leaves ~f:(fun (v, _) ->
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
    | Owned (Leaf (node, _)) ->
        let offset, len = t.range in
        lift_miss ~offset ~len @@ decode_mem_val ~ty node

  let merge_Borrows t =
    fold_iter ~init:None
      ~f:(fun acc ((offset, len), _, tb_st) ->
        match (tb_st, acc) with
        | None, _ ->
            (* Missing state; we must miss *)
            lift_miss ~offset ~len
            @@ lift_tb_st_miss
            @@ Result.miss [ Borrows.State.fix_empty () ]
        | Some _, None -> Result.ok tb_st
        | Some tb_st, Some acc ->
            let+ res = Borrows.State.merge tb_st acc in
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
    with_bound_check bound (fun t -> ok ((), t))

  (* Memory operations *)

  let load ~(ignore_borrow : bool) (ofs : Typed.([< T.sint ] t)) (ty : Types.ty)
      (tag : Borrows.Tag.t option) (tb : Borrows.Tree.t option) =
    let open SM.Syntax in
    let** size = lift_symex @@ Layout.size_of ty in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_typed ofs ty in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMap.SM.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          match (ignore_borrow, tag) with
          | false, Some tag ->
              lift_miss ~offset:ofs ~len:bound
              @@ Tree.map_leaves_tb (Borrows.State.access tag Read tb) t
          | true, _ | _, None -> Result.ok t
        in
        let rebuild_parent = Tree.of_children in
        let** framed, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        let++ sval = decode_tree ~ty framed in
        (sval, tree))

  let store (ofs : Typed.([< T.sint ] t)) (value : rust_val)
      (tag : Borrows.Tag.t option) (tb : Borrows.Tree.t option) :
      (unit, 'err, 'fix) SM.Result.t =
    let open SM.Syntax in
    let** size = lift_symex @@ Value_codec.size_of value in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMap.SM.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          let** tb_st = merge_Borrows t in
          match tag with
          | Some tag ->
              let++ tb_st' =
                lift_miss ~offset:ofs ~len:bound
                @@ lift_tb_miss
                @@ Borrows.State.access tag Write tb tb_st
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
        let open DecayMap.SM.Syntax in
        let replace_node node = ok node in
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
        let open DecayMap.SM.Syntax in
        let replace_node t =
          let@ _ = as_owned ~mk_fixes t in
          let++ tb_st = merge_Borrows t in
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
        let open DecayMap.SM.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          let++ tb_st = merge_Borrows t in
          zeros range tb_st
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let alloc ?(zeroed = false) size =
    let st = if zeroed then Zeros else Uninit in
    let+ tb_st = Borrows.State.init () in
    alloc (Leaf (st, Some tb_st)) size

  (* Tree borrow updates *)

  let with_tb_access (ofs : Typed.([< T.sint ] t))
      (size : Typed.([< T.sint ] t)) f =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_tb ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMap.SM.Syntax in
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
    with_tb_access ofs size
      (Borrows.State.set_protector ~protected:false tag tb)

  let tb_access ofs size tag tb =
    with_tb_access ofs size (Borrows.State.access tag Read tb)
end
