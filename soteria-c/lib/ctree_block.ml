open Soteria.Symex.Compo_res
open Csymex.Syntax
open Typed
open Typed.Infix
open Typed.Syntax
open Csymex
open Csymex.Result
module Ctype = Cerb_frontend.Ctype

module MemVal = struct
  module TB = Soteria.Sym_states.Tree_block
  module Symex = Csymex

  module SBoundedInt = struct
    include Typed
    include Typed.Infix

    type sint = T.sint
    type sbool = T.sbool

    let zero () = Usize.(0s)
    let ( <@ ) = Typed.Infix.( <$@ )
    let ( <=@ ) = Typed.Infix.( <=$@ )

    (* We assume addition/overflow within the range of an allocation may never overflow.
       This allows extremely good reductions around inequalities, which Tree_block relies on.  *)
    let ( +@ ) = Typed.Infix.( +!!@ )
    let ( -@ ) = Typed.Infix.( -!!@ )

    let in_bound (v : sint Typed.t) : sbool Typed.t =
      v >=@ zero () &&@ (v <=@ Typed.BitVec.isize_max)
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
        L.trace (fun m -> m "Uninitialized Memory Access detected!");
        Result.error `UninitializedMemoryAccess
    | Zeros -> (
        match ty with
        | Ctype.Ctype (_, Basic (Integer ity)) ->
            let+ size = Layout.size_of_int_ty_unsupported ity in
            Soteria.Symex.Compo_res.ok (BitVec.zero (8 * size))
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
        (* We don't know if this read is valid, as memory could be uninitialised.
           We have to approximate and vanish. *)
        Csymex.not_impl "Reading from Any memory, vanishing."

  type serialized =
    | SInit of (T.cval Typed.t * Ctype.ctype)
        [@printer Fmt.(pair ~sep:(Fmt.any " : ") Typed.ppa Fmt_ail.pp_ty)]
    | SUninit
    | SZeros
    | SAny
  [@@deriving show { with_path = false }]

  let subst_serialized f = function
    | SInit (v, ty) -> SInit (Typed.subst f v, ty)
    | v -> v

  let iter_vars_serialized v f =
    match v with SInit (v, _) -> Typed.iter_vars v f | _ -> ()

  (* TODO: serialize tree borrow information! *)
  let serialize (t : t) : serialized Seq.t option =
    match t with
    | Init v -> Some (Seq.return (SInit v))
    | Uninit Totally -> Some (Seq.return SUninit)
    | Zeros -> Some (Seq.return SZeros)
    | Any -> Some (Seq.return SAny)
    | Lazy | Uninit Partially -> None

  let mk_fix_typed ty () =
    let* v = Layout.nondet_c_ty ty in
    return [ SInit (v, ty) ]

  let mk_fix_any () = [ Any ]

  type tree = (t, T.sint Typed.t) TB.tree

  let not_owned (t : tree) : tree =
    { t with node = NotOwned Totally; children = None }

  let owned (t : tree) (v : t) : tree =
    { t with node = Owned v; children = None }

  let consume (s : serialized) (t : tree) : (tree, 'e, 'f) Result.t =
    match (s, t.node) with
    (* init *)
    | SInit (_, ty), NotOwned _ ->
        let+ fixes = mk_fix_typed ty () in
        Missing fixes
    | SInit (v, ty), Owned node -> (
        let* sval_res = decode ~ty node in
        match sval_res with
        | Ok sv ->
            let+ () = assume [ sv ==?@ v ] in
            Ok (not_owned t)
        | Missing f -> miss f
        | Error `UninitializedMemoryAccess -> vanish ())
    (* any *)
    | SAny, NotOwned _ -> miss_no_fix ~reason:"consume_any" ()
    | SAny, Owned _ -> ok (not_owned t)
    (* uninit *)
    | SUninit, NotOwned _ -> miss_no_fix ~reason:"consume_uninit" ()
    | SUninit, Owned (Uninit Totally) -> ok (not_owned t)
    | SUninit, _ ->
        L.info (fun m -> m "Consuming uninit but no uninit, vanishing");
        vanish ()
    (* zeros *)
    | SZeros, NotOwned _ -> miss_no_fix ~reason:"consume_zeros" ()
    | SZeros, Owned Zeros -> ok (not_owned t)
    | SZeros, Owned (Init (v, cty)) ->
        let* size = Layout.size_of_s cty in
        let* size =
          of_opt_not_impl ~msg:"Consuming zeros with unknown size"
          @@ Typed.BitVec.to_z size
        in
        let size = Z.to_int size in
        let+ () = Csymex.assume [ v ==?@ BitVec.zero (8 * size) ] in
        Ok (not_owned t)
    | SZeros, _ ->
        L.info (fun m -> m "Consuming zero but not zero, vanishing");
        vanish ()

  let produce (s : serialized) (t : tree) : tree Csymex.t =
    match (s, t.node) with
    | _, (Owned _ | NotOwned Partially) -> vanish ()
    | SInit v, NotOwned Totally -> return (owned t (Init v))
    | SZeros, NotOwned Totally -> return (owned t Zeros)
    | SUninit, NotOwned Totally -> return (owned t (Uninit Totally))
    | SAny, NotOwned Totally -> return (owned t Any)

  let assert_exclusively_owned _ = Result.ok ()
end

open MemVal
include Tree_block (MemVal)

let log_fixes fixes =
  L.trace (fun m ->
      m "MISSING WITH FIXES: %a" (Fmt.Dump.list pp_serialized) fixes);
  fixes

let range_of_low_and_type low ty =
  let+ size = Layout.size_of_s ty in
  Range.of_low_and_size low size

let sval_leaf ~range ~value ~ty =
  Tree.make ~node:(Owned (Init (value, ty))) ~range ?children:None ()

let uninit ~range = Tree.make ~node:(Owned (Uninit Totally)) ~range ()

let mk_fix_typed ofs ty () =
  let* len = Layout.size_of_s ty in
  let+ fixes = mk_fix_typed ty () in
  List.map (fun v -> [ MemVal { offset = ofs; len; v } ]) fixes

let mk_fix_any ofs len () = [ [ MemVal { offset = ofs; len; v = SAny } ] ]

let decode ~ty ~ofs node =
  match node with
  | TB.Owned node -> MemVal.decode ~ty node
  | TB.NotOwned _ ->
      let+ fixes = mk_fix_typed ofs ty () in
      Soteria.Symex.Compo_res.miss (log_fixes fixes)

let load (ofs : [< T.sint ] Typed.t) (ty : Ctype.ctype) (t : t option) :
    (T.cval Typed.t * t option, 'err, 'fix) Result.t =
  let* ((_, bound) as range) = range_of_low_and_type ofs ty in
  let@ t = with_bound_and_owned_check ~mk_fixes:(mk_fix_typed ofs ty) t bound in
  let replace_node node = Result.ok node in
  let rebuild_parent = Tree.with_children in
  let** framed, tree = Tree.frame_range t ~replace_node ~rebuild_parent range in
  let++ sval = decode ~ty ~ofs framed.node in
  ((sval :> T.cval Typed.t), tree)

let store (low : [< T.sint ] Typed.t) (ty : Ctype.ctype)
    (sval : [< T.cval ] Typed.t) (t : t option) :
    (unit * t option, 'err, 'fix) Result.t =
  let* ((_, bound) as range) = range_of_low_and_type low ty in
  match t with
  | None ->
      let fixes = mk_fix_any low (Range.size range) () in
      Result.miss (log_fixes fixes)
  | Some t ->
      let@ t = with_bound_check t bound in
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
      ((), tree)

let deinit (low : [< T.sint ] Typed.t) (len : [< T.sint ] Typed.t)
    (t : t option) : (unit * t option, 'err, 'fix) Result.t =
  match t with
  | None ->
      let fixes = mk_fix_any low len () in
      Result.miss (log_fixes fixes)
  | Some t ->
      let ((_, bound) as range) = Range.of_low_and_size low len in
      let@ t = with_bound_check t bound in
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
      ((), tree)

let alloc ~zeroed size = alloc (if zeroed then Zeros else Uninit Totally) size
