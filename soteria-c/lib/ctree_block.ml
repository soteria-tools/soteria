open Csymex.Syntax
open Typed
open Csymex
open Csymex.Result
module Ctype = Cerb_frontend.Ctype

module MemVal = struct
  module TB = Soteria.Sym_states.Tree_block
  module S_bool = Typed.Bool

  module S_int = struct
    include Typed
    include Typed.BitVec

    type t = T.sint Typed.t [@@deriving show { with_path = false }]

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
      let open Typed.Infix in
      v <=@ Typed.BitVec.isize_max

    type syn = Typed.Expr.t [@@deriving show { with_path = false }]

    let to_syn = Typed.Expr.of_value
    let subst = Typed.Expr.subst
    let learn_eq = Consumer.learn_eq
    let exprs_syn x = [ x ]
    let fresh () = nondet Typed.t_usize
  end

  let pp_init ft (v, ty) =
    let open Fmt in
    pf ft "%a : %a" Typed.ppa v Fmt_ail.pp_ty ty

  type qty = Totally | Partially [@@deriving show]

  type t =
    | Init of (T.cval Typed.t * Ctype.ctype) [@printer pp_init]
    | Zeros
    | Uninit of qty
    | Any
    | Lazy
  [@@deriving show { with_path = false }]

  let any_of_type (Ctype.Ctype (_, kty) as ty) =
    match kty with
    | Basic (Integer int_ty) ->
        let* size = Layout.size_of_int_ty_unsupported int_ty in
        let* constrs =
          Csymex.of_opt_not_impl ~msg:"Int constraints"
            (Layout.int_constraints int_ty)
        in
        let* value = Csymex.nondet (Typed.t_int (8 * size)) in
        let+ () = Csymex.assume (constrs value) in
        ((value :> T.cval Typed.t), ty)
    | _ -> Fmt.kstr not_impl "Nondet of type %a" Fmt_ail.pp_ty ty

  let merge ~left ~right =
    match (left, right) with
    | Zeros, Zeros -> Zeros
    | Uninit Totally, Uninit Totally -> Uninit Totally
    | Uninit _, Uninit _ -> Uninit Partially
    | Init (v1, _), Init (v2, _)
      when Typed.BitVec.sure_is_zero v1 && Typed.BitVec.sure_is_zero v2 ->
        Zeros
    | _, _ -> Lazy

  let split ~at:_ node =
    let open TB.Split_tree in
    match node with
    | (Uninit Totally | Zeros | Any) as v -> return (Leaf v, Leaf v)
    | Init i -> Fmt.kstr not_impl "Splitting %a" pp_init i
    | Uninit Partially | Lazy ->
        failwith "Should never split an intermediate node"

  let decode ~ty t : ([> T.sint ] Typed.t, 'err, 'fix) Csymex.Result.t =
    match t with
    | Uninit _ ->
        [%l.trace "Uninitialized Memory Access detected!"];
        Result.error `UninitializedMemoryAccess
    | Zeros -> (
        match ty with
        | Ctype.Ctype (_, Basic (Integer ity)) ->
            let+ size = Layout.size_of_int_ty_unsupported ity in
            Compo_res.ok (BitVec.zero (8 * size))
        | Ctype (_, Basic (Floating fty)) ->
            let precision = Layout.precision fty in
            Result.ok (Typed.Float.mk precision "+0.0")
        | Ctype.Ctype (_, Pointer _) -> Result.ok Typed.Ptr.null
        | _ ->
            Fmt.kstr not_impl "Cannot decode Zeros for type %a" Fmt_ail.pp_ty ty
        )
    | Lazy -> Fmt.kstr not_impl "Lazy memory access, cannot decode"
    | Init (value, tyw) ->
        if Ctype.ctypeEqual ty tyw then Result.ok value
        else
          Fmt.kstr not_impl "Type mismatch when decoding value: %a vs %a"
            Fmt_ail.pp_ty ty Fmt_ail.pp_ty tyw
    | Any ->
        (* We don't know if this read is valid, as memory could be
           uninitialised. We have to approximate and vanish. *)
        Csymex.not_impl "Reading from Any memory, vanishing."

  type syn =
    | SInit of (Expr.t * Ctype.ctype)
        [@printer Fmt.(pair ~sep:(Fmt.any " : ") Expr.pp Fmt_ail.pp_ty)]
    | SUninit
    | SZeros
    | SAny
  [@@deriving show { with_path = false }]

  let ins_outs = function
    | SInit (v, _) -> ([], [ v ])
    | SUninit | SZeros | SAny -> ([], [])

  let to_syn (t : t) : syn Seq.t option =
    match t with
    | Init (v, ty) ->
        let v = Expr.of_value v in
        Some (Seq.return (SInit (v, ty)))
    | Uninit Totally -> Some (Seq.return SUninit)
    | Zeros -> Some (Seq.return SZeros)
    | Any -> Some (Seq.return SAny)
    | Lazy | Uninit Partially -> None

  let mk_fix_typed ty () =
    let* v = Layout.nondet_c_ty ty in
    return [ SInit (Expr.of_value v, ty) ]

  let mk_fix_any () = [ Any ]

  type tree = (t, T.sint Typed.t) TB.tree

  let not_owned (t : tree) : tree =
    { t with node = NotOwned Totally; children = None }

  let owned (t : tree) (v : t) : tree =
    { t with node = Owned v; children = None }

  let consume (s : syn) (t : tree) : (tree, syn list) Consumer.t =
    let open Consumer in
    let open Syntax in
    match (s, t.node) with
    (* init *)
    | s, NotOwned Totally -> miss [ [ s ] ]
    | _, NotOwned Partially ->
        miss_no_fix ~reason:"partially missing consume" ()
    | SInit (v, ty), Owned node -> (
        let*^ sval_res = decode ~ty node in
        match sval_res with
        | Ok sv ->
            let+ () = learn_eq v sv in
            not_owned t
        | Missing f -> miss f
        | Error `UninitializedMemoryAccess ->
            [%l.info "Consuming a value from uninit, logical failure"];
            lfail Typed.v_false)
    (* any *)
    | SAny, Owned _ -> ok (not_owned t)
    (* uninit *)
    | SUninit, Owned (Uninit Totally) -> ok (not_owned t)
    | SUninit, _ ->
        [%l.info "Consuming uninit but no uninit, logical failure"];
        lfail Typed.v_false
    (* zeros *)
    | SZeros, Owned Zeros -> ok (not_owned t)
    | SZeros, Owned (Init (v, cty)) ->
        let*^ size = Layout.size_of_s cty in
        let*^ size =
          of_opt_not_impl ~msg:"Consuming zeros with unknown size"
          @@ Typed.BitVec.to_z size
        in
        let size = Z.to_int size in
        let zero = Expr.of_value @@ BitVec.zero (8 * size) in
        let+ () = learn_eq zero v in
        not_owned t
    | SZeros, _ ->
        [%l.info "Consuming zero but not zero, logical failure"];
        lfail Typed.v_false

  let produce (s : syn) (t : tree) : tree Producer.t =
    let open Producer in
    let open Syntax in
    match (s, t.node) with
    | _, (Owned _ | NotOwned Partially) -> vanish ()
    | SInit (v, ty), NotOwned Totally ->
        let+ v = Producer.apply_subst Expr.subst v in
        owned t (Init (v, ty))
    | SZeros, NotOwned Totally -> return (owned t Zeros)
    | SUninit, NotOwned Totally -> return (owned t (Uninit Totally))
    | SAny, NotOwned Totally -> return (owned t Any)

  let assert_exclusively_owned (t : tree) =
    match t.node with
    | NotOwned _ -> miss [ [ SAny ] ]
    | Owned _ -> Result.ok ()
end

open MemVal
include Tree_block (MemVal)

let log_fixes fixes =
  [%l.trace "MISSING WITH FIXES: %a" Fmt.Dump.(list @@ list pp_syn) fixes];
  fixes

let range_of_low_and_type low ty =
  let+ size = Layout.size_of_s ty in
  Range.of_low_and_size low size

let sval_leaf ~range ~value ~ty =
  if Typed.BitVec.sure_is_zero value then
    Tree.make ~node:(Owned Zeros) ~range ?children:None ()
  else Tree.make ~node:(Owned (Init (value, ty))) ~range ?children:None ()

let zeros ~range = Tree.make ~node:(Owned Zeros) ~range ()
let uninit ~range = Tree.make ~node:(Owned (Uninit Totally)) ~range ()

let mk_fix_typed offset ty () =
  let* len = Layout.size_of_s ty in
  let+ fixes = mk_fix_typed ty () in
  [ lift_fixes ~offset ~len fixes ]

let mk_fix_any offset len () = [ lift_fixes ~offset ~len [ SAny ] ]

let mk_fix_any_s ofs len () =
  let fixes = mk_fix_any ofs len () in
  Csymex.return (log_fixes fixes)

let decode ~ty ~ofs node =
  match node with
  | TB.Owned node -> MemVal.decode ~ty node
  | TB.NotOwned _ ->
      let+ fixes = mk_fix_typed ofs ty () in
      Compo_res.miss (log_fixes fixes)

let load (ofs : [< T.sint ] Typed.t) (ty : Ctype.ctype) :
    (T.cval Typed.t, 'err, syn list) SM.Result.t =
  let open SM.Syntax in
  let*^ ((_, bound) as range) = range_of_low_and_type ofs ty in
  with_bound_check ~mk_fixes:(mk_fix_typed ofs ty) bound (fun t ->
      let open Csymex.Syntax in
      let replace_node node = Result.ok node in
      let rebuild_parent = Tree.with_children in
      let** framed, tree =
        Tree.frame_range t ~replace_node ~rebuild_parent range
      in
      let++ sval = decode ~ty ~ofs framed.node in
      ((sval :> T.cval Typed.t), tree))

let store (low : [< T.sint ] Typed.t) (ty : Ctype.ctype)
    (sval : [< T.cval ] Typed.t) : (unit, 'err, syn list) SM.Result.t =
  let open SM.Syntax in
  let*^ ((_, bound) as range) = range_of_low_and_type low ty in
  let len = Range.size range in
  with_bound_check ~mk_fixes:(mk_fix_any_s low len) bound (fun t ->
      let open Csymex.Syntax in
      let replace_node _ = Result.ok @@ sval_leaf ~range ~value:sval ~ty in
      let rebuild_parent = Tree.of_children in
      let** node, tree =
        Tree.frame_range t ~replace_node ~rebuild_parent range
      in
      let++ () =
        match node.node with
        | NotOwned Totally ->
            let* len = Layout.size_of_s ty in
            let fixes = mk_fix_any low len () in
            Result.miss (log_fixes fixes)
        | NotOwned Partially -> miss_no_fix ~reason:"partially missing store" ()
        | _ -> Result.ok ()
      in
      ((), tree))

let zero_range (ofs : [< T.sint ] Typed.t) (size : [< T.sint ] Typed.t) :
    (unit, 'err, syn list) SM.Result.t =
  let ((_, bound) as range) = Range.of_low_and_size ofs size in
  let len = Range.size range in
  with_bound_check ~mk_fixes:(mk_fix_any_s ofs len) bound (fun t ->
      let replace_node t =
        match t.node with
        | NotOwned Totally ->
            let fix = mk_fix_any ofs size () in
            Result.miss (log_fixes fix)
        | NotOwned Partially ->
            miss_no_fix ~reason:"partially missing zero_range" ()
        | Owned _ -> Result.ok @@ zeros ~range
      in
      let rebuild_parent = Tree.of_children in
      let++ _, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
      ((), tree))

let deinit (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t) :
    (unit, 'err, 'fix) SM.Result.t =
  let ((_, bound) as range) = Range.of_low_and_size low len in
  with_bound_check ~mk_fixes:(mk_fix_any_s low len) bound (fun t ->
      let replace_node _ = Result.ok @@ uninit ~range in
      let rebuild_parent = Tree.of_children in
      let** node, tree =
        Tree.frame_range t ~replace_node ~rebuild_parent range
      in
      let++ () =
        match node.node with
        | NotOwned Totally ->
            let fixes = mk_fix_any low len () in
            Result.miss (log_fixes fixes)
        | NotOwned Partially ->
            miss_no_fix ~reason:"partially missing deinit" ()
        | _ -> Result.ok ()
      in
      ((), tree))

let alloc ~zeroed size = alloc (if zeroed then Zeros else Uninit Totally) size
