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
  module Encoder = Value_codec.Encoder (Sptr)

  type rust_val = Sptr.t Rust_val.t

  let pp_rust_val = Rust_val.pp Sptr.pp

  module MemVal = struct
    module TB = Soteria.Sym_states.Tree_block
    module S_bool = Typed.Bool

    module S_bounded_int = struct
      include Typed
      include Typed.BV

      type t = T.sint

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

      let is_in_bound (v : t Typed.t) : T.sbool Typed.t =
        let max = Layout.max_value_z (TInt Isize) in
        let max = Typed.BitVec.usize max in
        v <=@ max
    end

    type qty = Totally | Partially [@@deriving show { with_path = false }]
    type 'a value_raw = Init of 'a | Zeros | Uninit of qty | Any | Lazy
    type value = rust_val value_raw
    type value_syn = Sptr.syn Rust_val.syn value_raw
    type t = value * Tree_borrow.tb_state
    type syn = value_syn
    (* FIXME: we need to serialize the [tb_state]. Could also define it as a
       separate type to avoid Uninit Partially and Lazy *)

    let ins_outs x =
      let outs =
        match x with
        | Init v -> Rust_val.exprs_syn Sptr.exprs_syn v
        | Zeros | Uninit _ | Any | Lazy -> []
      in
      ([], outs)

    let value_to_syn v =
      let open Syntaxes.Option in
      let+ res =
        match v with
        | Init v -> Some (Init ((Rust_val.to_syn Sptr.to_syn) v))
        | Zeros -> Some Zeros
        | Uninit q -> Some (Uninit q)
        | Any | Lazy -> None
      in
      Seq.singleton res

    let to_syn ((v, _) : t) : syn Seq.t option = value_to_syn v

    let pp_value_raw pp_rv ft =
      let open Fmt in
      function
      | Zeros -> pf ft "Zeros"
      | Uninit qty -> pf ft "Uninit %a" pp_qty qty
      | Lazy -> pf ft "Lazy"
      | Init mv -> pp_rv ft mv
      | Any -> pf ft "Any"

    let pp_value = pp_value_raw pp_rust_val
    let pp ft (v, _) = pp_value ft v
    let pp_value_syn = pp_value_raw (Rust_val.pp_syn Sptr.pp_syn)
    let pp_syn ft v = pp_value_syn ft v

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

    let mk_fix_typed ty () =
      let+^ v = Encoder.nondet_valid ty in
      (* we're basically guaranteed this won't error (ie. layout error) by now,
         so we can safely unwrap. *)
      let v = get_ok v in
      [ Init (Rust_val.to_syn Sptr.to_syn v) ]

    let mk_fix_any () = [ Any ]

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

    let consume (_s : syn) (_t : tree) : (tree, syn list) Consumer.t =
      failwith "TODO"

    (* let consume (s : serialized) (t : tree) : (tree, 'e, 'f) Result.t = match
       (s, t.node) with | _, NotOwned _ -> miss [] (* init *) | SInit _, _ ->
       not_impl "Consume typed value on rust_val equality." (* any *) | SAny,
       Owned _ -> ok (not_owned t) (* uninit *) | SUninit, Owned (Uninit
       Totally, _) -> ok (not_owned t) | SUninit, _ -> vanish () (* zeros *) |
       SZeros, Owned (Zeros, _) -> ok (not_owned t) | SZeros, Owned (Init _, _)
       -> not_impl "Assume rust_val == 0s" | SZeros, _ -> vanish () *)

    let produce (syn : syn) (tree : tree) : tree DecayMapMonad.Producer.t =
      let open DecayMapMonad.Producer in
      let open Syntax in
      match (syn, tree.node) with
      | _, (Owned _ | NotOwned Partially) -> vanish ()
      | Init v, NotOwned Totally ->
          let+ v = Producer.apply_subst (Rust_val.subst Sptr.subst) v in
          owned tree (Init v)
      | Zeros, NotOwned Totally -> return (owned tree Zeros)
      | Uninit Totally, NotOwned Totally -> return (owned tree (Uninit Totally))
      | Any, NotOwned Totally -> return (owned tree Any)
      | (Uninit Partially | Lazy), _ -> failwith "Unreachable!"

    let assert_exclusively_owned _ = Result.ok ()
  end

  open MemVal
  include Soteria.Sym_states.Tree_block.Make (DecayMapMonad) (MemVal)

  let lift_symex x = SM.lift @@ DecayMapMonad.lift x

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
    | Init value ->
        let+ res = Encoder.transmute_one ~to_ty:ty value in
        Ok res
    | Zeros ->
        let**^ size = Layout.size_of ty in
        let* size = sint_to_int size in
        let zero = BV.zero (size * 8) in
        let+ res = Encoder.transmute_one ~to_ty:ty (Int zero) in
        Ok res
    | Uninit _ -> Result.error `UninitializedMemoryAccess
    | Any ->
        (* We don't know if this read is valid, as memory could be
           uninitialised. We have to approximate and vanish. *)
        not_impl "Reading from Any memory, vanishing."
    | Lazy -> failwith "Should have been handled earlier"

  let decode_lazy ~ty (t : Tree.t) =
    (* The tree spans the entire type we're interested in. Furthermore, we only
       read/write scalars (int, float, pointers...) which cover the whole range
       with no gaps. For lazy nodes, we convert all of these to bitvectors, the
       concatenate them and call the encoder to decode the full value. *)
    let** leaves = collect_leaves t in
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
    | Owned (Lazy, _) -> decode_lazy ~ty t
    | Owned (node, _) -> decode_mem_val ~ty node

  let uninit range tb : Tree.t =
    Tree.make ~node:(Owned (Uninit Totally, tb)) ~range ()

  let zeros range tb : Tree.t = Tree.make ~node:(Owned (Zeros, tb)) ~range ()

  let mk_fix_typed offset ty () =
    let*^ len = Layout.size_of ty in
    let len = get_ok len in
    let+ fixes = mk_fix_typed ty () in
    [ lift_fixes ~offset ~len fixes ]
  (* List.map (fun v -> [ MemVal { offset = Expr.of_value ofs; len =
     Expr.of_value len; v } ]) fixes *)

  let mk_fix_any offset len () = [ lift_fixes ~offset ~len [ Any ] ]
  let mk_fix_any_s ofs len () = return (mk_fix_any ofs len ())

  let as_owned ?mk_fixes t f =
    match (t.node, mk_fixes) with
    | Owned (v, tb), _ -> f (v, tb)
    | NotOwned _, None -> miss_no_fix ~reason:"as_owned" ()
    | NotOwned _, Some mk_fixes ->
        let+ fixes = mk_fixes () in
        Missing fixes

  let check_owned (ofs : [< T.sint ] Typed.t) (size : [< T.nonzero ] Typed.t) =
    let _, bound = Range.of_low_and_size ofs (Typed.cast size) in
    with_bound_check bound (fun t -> DecayMapMonad.Result.ok ((), t))

  (* Memory operations *)

  let load ~(ignore_borrow : bool) (ofs : [< T.sint ] Typed.t) (ty : Types.ty)
      (tag : Tree_borrow.tag option) (tb : Tree_borrow.t) =
    let open SM.Syntax in
    let** size = lift_symex @@ Layout.size_of ty in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_typed ofs ty in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ v, tb_st = as_owned ~mk_fixes t in
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
        (sval, tree))

  let store (ofs : [< T.sint ] Typed.t) (value : rust_val)
      (tag : Tree_borrow.tag option) (tb : Tree_borrow.t) :
      (unit, 'err, 'fix) SM.Result.t =
    let open SM.Syntax in
    let** size = lift_symex @@ Value_codec.size_of value in
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ _, tb_st = as_owned ~mk_fixes t in
          let++^ tb_st' =
            match tag with
            | Some tag -> Tree_borrow.access tag Write tb tb_st
            | None -> Rustsymex.Result.ok tb_st
          in
          { node = Owned (Init value, tb_st'); range; children = None }
        in
        let rebuild_parent = Tree.of_children in
        let** node, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        let++ () =
          match node.node with
          | NotOwned _ -> miss_no_fix ~reason:"store" ()
          | _ -> Result.ok ()
        in
        ((), tree))

  let get_init_leaves (ofs : [< T.sint ] Typed.t)
      (size : [< T.nonzero ] Typed.t) :
      ((rust_val * T.sint Typed.t) list, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs (Typed.cast size) in
    with_bound_check bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node node = Result.ok node in
        let rebuild_parent = Tree.with_children in
        let** framed, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        let++ leaves = collect_leaves framed in
        (leaves, tree))

  let uninit_range (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t) :
      (unit, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ _ = as_owned ~mk_fixes t in
          Tree.map_leaves t @@ fun tt ->
          match tt.node with
          | Owned (_, tb) -> Result.ok (uninit tt.range tb)
          | _ -> assert false
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let zero_range (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t) :
      (unit, 'err, 'fix) SM.Result.t =
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    let mk_fixes = mk_fix_any_s ofs size in
    with_bound_check ~mk_fixes bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ _, tb = as_owned ~mk_fixes t in
          (* Is there something to do with the tree borrow here? *)
          Result.ok @@ zeros range tb
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let alloc ?(zeroed = false) size =
    let st = if zeroed then Zeros else Uninit Totally in
    alloc (st, Tree_borrow.empty_state) size

  (* Tree borrow updates *)

  let with_tb_access (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t) f
      =
    (* TODO: figure out [mk_fixes] for tree borrows state! *)
    let ((_, bound) as range) = Range.of_low_and_size ofs size in
    with_bound_check bound (fun t ->
        let open DecayMapMonad.Syntax in
        let replace_node t =
          let@ v, tb_st = as_owned t in
          let++^ tb_st' = f tb_st in
          { t with node = Owned (v, tb_st') }
        in
        let rebuild_parent = Tree.of_children in
        let++ _, tree =
          Tree.frame_range t ~replace_node ~rebuild_parent range
        in
        ((), tree))

  let protect ofs size tag tb =
    with_tb_access ofs size (fun tb_st ->
        (* We need to do two things: protect this tag for the block, and perform
           a read, as all function calls perform one on the parameters. *)
        Tree_borrow.set_protector ~protected:true tag tb tb_st
        |> Tree_borrow.access tag Read tb)

  let unprotect ofs size tag tb =
    with_tb_access ofs size (fun tb_st ->
        Rustsymex.Result.ok
        @@ Tree_borrow.set_protector ~protected:false tag tb tb_st)

  let tb_access (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t)
      (tag : Tree_borrow.tag) (tb : Tree_borrow.t) :
      (unit, 'err, 'fix) SM.Result.t =
    with_tb_access ofs size (Tree_borrow.access tag Read tb)
end
