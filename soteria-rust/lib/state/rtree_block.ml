open Compo_res
open Svalue
open Typed.Infix
module BV = Typed.BV
open Charon
open Syntaxes.FunctionWrap
module DecayMap = Sptr.DecayMap
open DecayMap.SM
open Result
open Syntax

module Make (Borrows : Tree_borrows.M(DecayMap.SM).S) = struct
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

    type leaf =
      | Scalar of Typed.(T.scalar t)
          (** A leaf value: either a [TBitVector], [TFloat] or [TFullPtr] with
              no metadata. *)
      | Aggregate of Typed.(T.aggregate t) * Types.ty
          (** A whole aggregate value, stored unencoded along with its type; it
              is only shallowly decomposed when an access requires it. *)
      | Zeros
      | Uninit
      | Any
      | Unowned

    type t = Leaf of leaf * Borrows.State.t option | Lazy

    let pp_leaf ft =
      let open Fmt in
      function
      | Zeros -> pf ft "Zeros"
      | Uninit -> pf ft "Uninit"
      | Scalar mv -> Typed.ppa ft mv
      | Aggregate (mv, ty) ->
          pf ft "%a : %a" Typed.ppa mv Common.Charon_util.pp_ty ty
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

    (** Converts a scalar to its bitvector representation. *)
    let scalar_to_bv (v : Typed.([< T.scalar ] t)) =
      match%ty v with
      | TBitVector _ -> return v
      | TExtension TFullPtr ->
          let ptr, meta = Typed.Ptr.split v in
          assert (Option.is_none meta);
          Sptr.decay ptr
      | TFloat _ -> Value_codec.float_to_bv_bits v
      | _ -> L.failwith "scalar_to_bv on non-scalar"

    let split_scalar (v : Typed.([< T.scalar ] t)) at =
      let* v = scalar_to_bv v in
      (* get our starting size and unsigned integer *)
      let size = Typed.size_of_int v / 8 in
      let+ at =
        match BV.to_z at with
        | Some at -> return (Z.to_int at)
        | _ -> (
            (* HACK: we need to branch on the concrete size, because the actual
               bitvector sort of the value must have a concrete size.

               As per the contract of [split], we know [at ∈ [1, size)] *)
            let options = List.init (size - 1) (( + ) 1) in
            let* res =
              match_on options ~constr:(fun x -> Typed.sem_eq at (BV.usizei x))
            in
            match res with Some i -> return i | None -> vanish ())
      in
      let mask_l = BV.extract 0 ((at * 8) - 1) v in
      let mask_r = BV.extract (at * 8) ((size * 8) - 1) v in
      (mask_l, mask_r)

    (** Builds a right-leaning split tree out of subtrees paired with the offset
        of their (exclusive) upper end; [base] is the offset at which the
        subtree starts. *)
    let rec mk_split_tree base = function
      | [] -> L.failwith "mk_split_tree: no parts"
      | [ (t, _) ] -> t
      | (t, high) :: rest ->
          TB.Split_tree.Node (t, high -!!@ base, mk_split_tree high rest)

    module Semi_concrete = Soteria.Data.S_list.Semi_concrete (DecayMap.SM)

    (** Sorts parts [(_, offset, _)] by their offset; if offsets are symbolic,
        this may branch. *)
    let sort_parts parts =
      Semi_concrete.sort ~leq:(fun (_, o, _) (_, o', _) -> o <=@ o') parts

    (** Shallowly explodes an aggregate value into leaves, one component deep:
        components that are themselves aggregates are kept whole, and padding
        gaps become [Uninit]. Returns the parts as
        [(leaf, offset, past-the-end offset)], sorted by offset, contiguously
        covering the type's full size. If the layout is symbolic, this may
        branch. *)
    let explode_shallow value ty =
      let*^ parts =
        Value_codec.encode ~depth:1 ~offset:(BV.usizei 0) value ty
      in
      let parts = get_ok parts in
      let*^ total = Layout.size_of ty in
      let total = get_ok total in
      let parts =
        Iter.to_list parts
        |> List.map (fun Typed.{ value; offset; size } ->
            let lf =
              match value with
              | Scalar v -> Scalar v
              | Aggregate (v, ty) -> Aggregate (Typed.cast v, ty)
            in
            (lf, offset, offset +!!@ (size :> Typed.T.sint Typed.t)))
      in
      let* parts = sort_parts parts in
      (* fill the padding gaps with [Uninit] *)
      let rec fill cur = function
        | [] ->
            if%sat cur <@ total then return [ (Uninit, cur, total) ]
            else return []
        | ((_, o, e) as p) :: rest ->
            let* rest = fill e rest in
            if%sat cur <@ o then return ((Uninit, cur, o) :: p :: rest)
            else return (p :: rest)
      in
      fill (BV.usizei 0) parts

    let rec split ~at node =
      match node with
      | Leaf ((Uninit | Zeros | Any | Unowned), _) ->
          return TB.Split_tree.(Leaf node, Leaf node)
      | Leaf (Scalar value, tb) ->
          let+ vl, vr = split_scalar value at in
          let ll = Leaf (Scalar vl, tb) in
          let lr = Leaf (Scalar vr, tb) in
          TB.Split_tree.(Leaf ll, Leaf lr)
      | Leaf (Aggregate (value, ty), tb) ->
          let* parts = explode_shallow (Typed.as_any value) ty in
          split_parts ~tb [] parts at
      | Lazy -> L.failwith "Should never split an intermediate node"

    (** Splits the exploded parts (as per {!explode_shallow}) at [at],
        recursively splitting the component that contains [at], if any. [left]
        accumulates the (reversed) components before [at]. *)
    and split_parts ~tb left parts at =
      let mk_el (lf, _, e) = (TB.Split_tree.Leaf (Leaf (lf, tb)), e) in
      (* as per the contract of [split], [at ∈ [1, size)], so it must fall
         within one of the parts: the list cannot be empty. *)
      let ((lf, o, e) as p), rest = List.take_first parts in
      if%sat at ==@ o then
        return
          ( mk_split_tree (BV.usizei 0) (List.rev left),
            mk_split_tree at (List.map mk_el parts) )
      else if%sat at <@ e then
        (* [at] falls strictly inside this component *)
        let+ sub_l, sub_r = split ~at:(at -!!@ o) (Leaf (lf, tb)) in
        ( mk_split_tree (BV.usizei 0) (List.rev ((sub_l, at) :: left)),
          mk_split_tree at ((sub_r, e) :: List.map mk_el rest) )
      else split_parts ~tb (mk_el p :: left) rest at

    type syn =
      | SScalar of Typed.Expr.t
      | SAggregate of Typed.Expr.t * Types.ty
      | SUninit
      | SZeros
      | SAny
      | STree_borrow_st of Borrows.State.syn
      | STree_borrow of Borrows.Tree.syn
    [@@deriving show { with_path = false }]

    let ins_outs = function
      | SScalar v | SAggregate (v, _) -> ([], [ v ])
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
      | Leaf (Unowned, None) ->
          L.failwith "Impossible: unowned with no TB state"
      | Leaf (leaf, tb) ->
          let leaf_ser =
            match leaf with
            | Scalar v -> SScalar (Typed.Expr.of_value v)
            | Aggregate (v, ty) -> SAggregate (Typed.Expr.of_value v, ty)
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
      (* we're basically guaranteed these won't error (ie. layout error) by now,
         so we can safely unwrap. *)
      let*^ ty = Layout.normalise ty in
      let ty = get_ok ty in
      let*^ layout = Layout.layout_of ty in
      let layout = get_ok layout in
      let+^ v = Value_codec.nondet_valid ty in
      let v = get_ok v in
      let v = Typed.Expr.of_value v in
      [ (if Layout.is_aggregate layout then SScalar v else SAggregate (v, ty)) ]

    type tree = (t, Typed.(T.sint t)) TB.tree

    let mk_leaf (t : tree) (v : leaf) tb : tree =
      let node =
        match (v, tb) with
        | Unowned, None -> TB.NotOwned Totally
        | _, _ -> TB.Owned (Leaf (v, tb))
      in
      TB.build_tree_leaf ~range:t.range ~node ()

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
        | SScalar e, Scalar v ->
            let+ () = learn_eq e v in
            Unowned
        | SScalar _, Zeros -> lift @@ not_impl "Assume rust_val.syn == 0s"
        | SScalar _, Aggregate _ | SAggregate _, Scalar _ ->
            lift @@ not_impl "consume aggregate/scalar mismatch"
        | SScalar _, _ -> lfail Typed.v_false
        (* whole values *)
        | SAggregate (e, ty_s), Aggregate (v, ty_v) ->
            if Types.equal_ty ty_s ty_v then
              let+ () = learn_eq e v in
              Unowned
            else lift @@ not_impl "consume whole values of different types"
        | SAggregate _, Zeros -> lift @@ not_impl "Assume aggregate value == 0s"
        | SAggregate _, _ -> lfail Typed.v_false
        (* any *)
        | SAny, _ -> ok Unowned
        (* uninit *)
        | SUninit, Uninit -> ok Unowned
        | SUninit, _ -> lfail Typed.v_false
        (* zeros *)
        | SZeros, Zeros -> ok Unowned
        | SZeros, (Scalar _ | Aggregate _) ->
            lift @@ not_impl "Assume rust_val == 0s"
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
            L.failwith
              "TB structure syn in tree block, should have been caught before"
        (* unrelated to tree borrows *)
        | SScalar _ | SAggregate _ | SZeros | SUninit | SAny -> ok tb
      in
      mk_leaf t v tb

    let rec produce (s : syn) (t : tree) : tree DecayMap.SM.Producer.t =
      let open DecayMap.SM.Producer in
      let open Syntax in
      match (s, t.node) with
      | ( (SScalar _ | SAggregate _ | SZeros | SUninit | SAny),
          (NotOwned Totally | Owned (Leaf (Unowned, _))) ) ->
          let* v =
            match s with
            | SScalar v ->
                let+ v = Producer.apply_subst Typed.Expr.subst v in
                Scalar v
            | SAggregate (v, ty) ->
                let+ v = Producer.apply_subst Typed.Expr.subst v in
                Aggregate (v, ty)
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
      | (SScalar _ | SAggregate _ | SZeros | SUninit | SAny), Owned (Leaf _) ->
          vanish ()
      | ( (SScalar _ | SAggregate _ | SZeros | SUninit | SAny),
          (Owned Lazy | NotOwned Partially) ) ->
          let l, r = Option.get t.children in
          let* sl, sr =
            match s with
            | SZeros | SUninit | SAny -> return (s, s)
            | SScalar v ->
                let* v = Producer.apply_subst Typed.Expr.subst v in
                let _, middle = l.range in
                let+^ vl, vr = split_scalar v middle in
                (* HACK: is this sound? Doing it this way because we can't have
                   a split_scalar for exprs, given it requires decaying
                   pointers. An alternative would be to have a [produce_leaf]
                   which takes in a [Leaf _], so we only subst once. *)
                let vl = Typed.Expr.of_value vl in
                let vr = Typed.Expr.of_value vr in
                (SScalar vl, SScalar vr)
            | SAggregate _ ->
                lift @@ not_impl "Produce SAggregate on a partially split tree"
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
          TB.make_tree_raw ~node ~range:t.range ~children:(l, r) ()
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
          TB.make_tree_raw ~node:t.node ~range:t.range ~children:(l, r) ()
      | STree_borrow _, _ ->
          L.failwith
            "TB structure syn in tree block, should have been caught before"

    let rec assert_exclusively_owned (t : tree) =
      match t.node with
      | NotOwned Totally | Owned (Leaf (Unowned, _)) -> miss [ [ SAny ] ]
      | NotOwned Partially | Owned Lazy ->
          let l, r = Option.get t.children in
          let** () = assert_exclusively_owned l in
          assert_exclusively_owned r
      | Owned (Leaf ((Zeros | Uninit | Any | Scalar _ | Aggregate _), tb)) ->
          lift_tb_st_miss @@ Borrows.State.assert_exclusively_owned tb
  end

  open MemVal
  include Soteria.Sym_states.Tree_block.Make (DecayMap.SM) (MemVal)

  module Range = struct
    include Range

    let[@inline] of_low_and_size low (size : Typed.([< T.nonzero ] t)) =
      of_low_and_size low (size :> Typed.(T.sint t))
  end

  module Tree = struct
    include Tree

    let map_leaves_tb f =
      map_leaves @@ function
      | TB.NotOwned Totally ->
          L.failwith "impossible: iterating over non-owned node"
      | NotOwned Partially | Owned Lazy ->
          L.failwith "impossible: iterating over intermediate node"
      | Owned (Leaf (v, tb)) ->
          let++ tb' = lift_tb_miss @@ f tb in
          TB.Owned (Leaf (v, tb'))

    let iter_leaves_rev (t : t) =
      iter_leaves_rev t
      |> Iter.filter_map @@ fun (leaf : _ tree) ->
         match leaf.node with
         | NotOwned Totally -> Some (leaf.range, Unowned, None)
         | NotOwned Partially | Owned Lazy ->
             L.failwith "impossible: iterating over intermediate node"
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

  let mk_fix_any offset (len : Typed.([< T.nonzero ] t)) () =
    [ lift_fixes ~offset ~len:(len :> Typed.(T.sint t)) [ SAny ] ]

  let mk_fix_any_s ofs len () = return (mk_fix_any ofs len ())

  let mk_fix_tb offset (len : Typed.([< T.nonzero ] t)) () =
    return
      [
        lift_fixes ~offset ~len:(len :> Typed.(T.sint t))
        @@ List.map MemVal.lift_tb_st_fix (Borrows.State.fix_empty ());
      ]

  let collect_leaves ~uninit (t : Tree.t) =
    fold_iter (Tree.iter_leaves_rev t) ~init:[] ~f:(fun vs (range, v, _tb) ->
        let offset, _ = range in
        let offset = offset -!@ fst t.range in
        let size = Typed.cast_nonzero @@ Range.size range in
        match v with
        | Uninit -> (
            match uninit with
            | `Ignore -> ok vs
            | `Error -> error `UninitializedMemoryAccess)
        | Zeros ->
            let+ sizei = sint_to_int size in
            let value = BV.zero (sizei * 8) in
            Ok (Typed.{ value = Scalar value; offset; size } :: vs)
        | Scalar value -> ok (Typed.{ value = Scalar value; offset; size } :: vs)
        | Aggregate (value, vty) ->
            ok (Typed.{ value = Aggregate (value, vty); offset; size } :: vs)
        | Any -> (
            match uninit with
            | `Ignore -> ok vs
            | `Error ->
                if Soteria.Symex.Approx.As_ctx.is_ux () then (
                  [%l.info "Reading from Any memory, vanishing."];
                  vanish ())
                else error `UninitializedMemoryAccess)
        | Unowned -> miss (mk_fix_any offset size ()))

  let decode_mem_val ~ty = function
    | Scalar value ->
        let+ res = Value_codec.transmute_one ~to_ty:ty value in
        Ok res
    | Aggregate _ ->
        L.failwith "decode_mem_val: aggregate values handled in decode_tree"
    | Zeros ->
        let**^ size = Layout.size_of ty in
        let* size = sint_to_int size in
        let zero = BV.zero (size * 8) in
        let+ res = Value_codec.transmute_one ~to_ty:ty zero in
        Ok res
    | Uninit -> error `UninitializedMemoryAccess
    | Any ->
        (* We don't know if this read is valid, as memory could be
           uninitialised. We have to approximate and vanish. *)
        if Soteria.Symex.Approx.As_ctx.is_ux () then (
          [%l.info "Reading from Any memory, vanishing."];
          vanish ())
        else error `UninitializedMemoryAccess
    | Unowned ->
        let+ fix = MemVal.mk_fix_typed ty () in
        Missing [ fix ]

  (** Converts a whole aggregate value of type [vty] to a scalar representation.
      If it is made of one scalar part, returns that; otherwise converts all
      parts into bitvectors (decaying pointers) and concatenates them. Errors if
      the type has any padding, as those bytes are uninitialised. *)
  let whole_to_scalar (value : Typed.([< T.aggregate ] t)) (vty : Types.ty) =
    let**^ parts =
      Value_codec.encode ~offset:(BV.usizei 0) (Typed.as_any value) vty
    in
    let parts =
      parts
      |> Iter.map (fun Typed.{ value; offset; size } ->
          (* We used [encode] with full depth, so we only get scalars *)
          match value with
          | Scalar v -> (v, offset, offset +!!@ (size :> Typed.T.sint Typed.t))
          | Aggregate _ -> L.failwith "impossible: aggregate after encode")
      |> Iter.to_list
    in
    let* parts = sort_parts parts in
    (* the parts must contiguously cover the whole type, as padding bytes are
       uninitialised *)
    let**^ total = Layout.size_of vty in
    let rec is_contiguous cur = function
      | [] -> cur ==@ total
      | (_, o, e) :: rest -> cur ==@ o &&@ is_contiguous e rest
    in
    let** () =
      if%sat is_contiguous (BV.usizei 0) parts then ok ()
      else error `UninitializedMemoryAccess
    in
    match parts with
    | [ (scalar, _, _) ] -> ok scalar
    | _ :: _ :: _ ->
        let+ bvs =
          DecayMap.SM.map_list parts ~f:(fun (v, _, _) -> scalar_to_bv v)
        in
        let hd, tl = List.take_first @@ List.rev bvs in
        let res = List.fold_left BV.concat hd tl in
        Ok Typed.((res : T.sint t :> T.scalar t))
    | [] -> L.failwith "whole_to_scalar: value has no parts"

  let decode_lazy ~ty (t : Tree.t) =
    (* The tree spans the entire type we're interested in. Furthermore, we only
       read/write scalars which cover the whole range with no gaps. For lazy
       nodes, we convert all of these to bitvectors, the concatenate them and
       call the Value_codec to decode the full value.

       Note we avoid converting to bitvectors if there is a single scalar, to
       not unnecessarily decay pointers. *)
    let** leaves = collect_leaves ~uninit:`Error t in
    let** leaves =
      Result.map_list leaves ~f:(fun Typed.{ value; _ } ->
          match value with
          | Scalar v -> ok v
          | Aggregate (v, ty) -> whole_to_scalar v ty)
    in
    match List.rev leaves with
    | [ scalar ] ->
        let+ res = Value_codec.transmute_one ~to_ty:ty scalar in
        Ok res
    | hd :: tl ->
        let* hd = scalar_to_bv hd in
        let* tl = DecayMap.SM.map_list tl ~f:scalar_to_bv in
        let bv = List.fold_left BV.concat hd tl in
        let+ res = Value_codec.transmute_one ~to_ty:ty bv in
        Ok res
    | _ -> L.failwith "Impossible"

  let decode_tree ~ty (t : Tree.t) =
    match t.node with
    | NotOwned _ -> miss []
    | Owned (Leaf (Aggregate (value, vty), _)) when Types.equal_ty ty vty ->
        ok (Typed.as_any value)
    | Owned (Leaf (Aggregate _, _)) -> decode_lazy ~ty t
    | Owned Lazy -> decode_lazy ~ty t
    | Owned (Leaf (node, _)) ->
        let offset, len = t.range in
        lift_miss ~offset ~len @@ decode_mem_val ~ty node

  let merge_borrows t =
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

  let init ?ty range (v : Typed.block_value) tb : Tree.t =
    let leaf =
      match v with
      | Scalar v -> Scalar v
      | Aggregate (v, ty) -> Aggregate (v, ty)
    in
    Tree.make ~node:(TB.Owned (Leaf (leaf, tb))) ~range ()

  let uninit range tb : Tree.t =
    Tree.make ~node:(TB.Owned (Leaf (Uninit, tb))) ~range ()

  let zeros range tb : Tree.t =
    Tree.make ~node:(TB.Owned (Leaf (Zeros, tb))) ~range ()

  let as_owned ?mk_fixes (t : _ tree) f =
    match (t.node, mk_fixes) with
    | Owned _, _ -> f t
    | NotOwned _, None -> miss_no_fix ~reason:"as_owned" ()
    | NotOwned _, Some mk_fixes ->
        let+ fixes = mk_fixes () in
        Missing fixes

  let check_owned (ofs : Typed.([< T.sint ] t))
      (size : Typed.([< T.nonzero ] t)) =
    let open DecayMap.SM.Syntax in
    let _, bound = Range.of_low_and_size ofs size in
    let mk_fixes () =
      let+ bound = DecayMap.SM.nondet (Typed.t_usize ()) in
      [ [ Bound (Expr.of_value bound) ] ]
    in
    with_bound_check ~mk_fixes bound (fun t -> ok ((), t))

  (* Memory operations *)

  let load ~(ignore_borrow : bool) (ofs : Typed.([< T.sint ] t)) (ty : Types.ty)
      (tag : Ptr_tag.t option) (tb : Borrows.Tree.t option) =
    let open SM.Syntax in
    let** size = lift_symex @@ Layout.size_of ty in
    (* we expect ZSTs to never be read through here. *)
    let size = Typed.BV.cast_nonzero size in
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

  (** Reads a value of type [ty] over the given range, if the tree stores it
      whole, i.e. as a single leaf holding a value of that exact type; returns
      [None] otherwise, in which case the value must be read component by
      component. Performs the borrow read access when the read succeeds. *)
  let load_whole ~(ignore_borrow : bool) (ofs : Typed.([< T.sint ] t))
      (ty : Types.ty) (tag : Ptr_tag.t option) (tb : Borrows.Tree.t option) =
    let open SM.Syntax in
    let** size = lift_symex @@ Layout.size_of ty in
    let size = Typed.BV.cast_nonzero size in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_typed ofs ty in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMap.SM.Syntax in
        let as_whole (t : Tree.t) =
          match t.node with
          | Owned (Leaf (Aggregate (v, vty), _)) when Types.equal_ty ty vty ->
              Some (Typed.as_any v)
          | _ -> None
        in
        let replace_node (t : Tree.t) =
          match (as_whole t, ignore_borrow, tag) with
          | Some _, false, Some tag ->
              lift_miss ~offset:ofs ~len:bound
              @@ Tree.map_leaves_tb (Borrows.State.access tag Read tb) t
          | _ -> Result.ok t
        in
        let rebuild_parent = Tree.of_children in
        let** framed, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        Result.ok (as_whole framed, tree))

  (** Stores [value] at the given range. If [ty] is provided, the value is an
      aggregate of that type, and is stored whole, to be decomposed lazily if a
      smaller access requires it. *)
  let store (ofs : Typed.([< T.sint ] t)) (size : Typed.([< T.nonzero ] t))
      (value : Typed.block_value) (tag : Ptr_tag.t option)
      (tb : Borrows.Tree.t option) : (unit, 'err, 'fix) SM.Result.t =
    let open SM.Syntax in
    (* manually coerce so types line up *)
    let ofs = (ofs :> Typed.(T.sint t)) in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMap.SM.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          let** tb_st = merge_borrows t in
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
      (size : Typed.([< T.nonzero ] t)) =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    with_bound_check bound (fun t ->
        let open DecayMap.SM.Syntax in
        let replace_node node = ok node in
        let rebuild_parent = Tree.with_children in
        let** framed, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        let++ leaves = collect_leaves ~uninit:`Ignore framed in
        (leaves, tree))

  let uninit_range (ofs : Typed.([< T.sint ] t))
      (size : Typed.([< T.nonzero ] t)) : (unit, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMap.SM.Syntax in
        let replace_node t =
          let@ _ = as_owned ~mk_fixes t in
          let++ tb_st = merge_borrows t in
          uninit range tb_st
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let zero_range (ofs : Typed.([< T.sint ] t)) (size : Typed.([< T.nonzero ] t))
      : (unit, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMap.SM.Syntax in
        let replace_node t =
          let@ t = as_owned ~mk_fixes t in
          let++ tb_st = merge_borrows t in
          zeros range tb_st
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let alloc ?(zeroed = false) (size : Typed.([< T.nonzero ] t)) =
    let st = if zeroed then Zeros else Uninit in
    let+ tb_st = Borrows.State.init () in
    alloc (Leaf (st, Some tb_st)) (size :> Typed.(T.sint t))

  module Decoder = Value_codec.Decoder (struct
    module SM = SM

    type nonrec syn = syn
  end)

  (** Applies the given parser. [on_access] is called at the start of any actual
      access to memory (which means it may not be called, e.g. with ZSTs that
      don't need any accesses to be parsed). *)
  let apply_parser (type a) ?(on_access = DecayMap.SM.Result.ok) ~ignore_borrow
      tag tb (parser : a Decoder.ParserMonad.t) :
      (a, Error.t, syn list) SM.Result.t =
    let open SM.Syntax in
    let handler ty ofs =
      let**^ () = on_access () in
      load ~ignore_borrow ofs ty tag tb
    in
    let get_all size ofs =
      let**^ () = on_access () in
      get_init_leaves ofs size
    in
    let query_whole ty ofs =
      let**^ () = on_access () in
      load_whole ~ignore_borrow ofs ty tag tb
    in
    Decoder.ParserMonad.parse ~handler ~get_all ~query_whole parser

  (* Tree borrow updates *)

  let with_tb_access (ofs : Typed.([< T.sint ] t))
      (size : Typed.([< T.nonzero ] t)) f =
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

  let unprotect ofs (size : Typed.([< T.nonzero ] t)) tag tb =
    with_tb_access ofs size
      (Borrows.State.set_protector ~protected:false tag tb)

  let tb_access ofs (size : Typed.([< T.nonzero ] t)) tag tb =
    with_tb_access ofs size (Borrows.State.access tag Read tb)
end
