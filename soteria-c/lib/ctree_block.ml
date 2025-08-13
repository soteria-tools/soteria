(* This could be parametric on an integer type and Node type. But is there realy a reason ? *)

open Soteria_symex.Compo_res
open Csymex.Syntax
open Typed
open Typed.Infix
open Typed.Syntax
open Csymex
open Csymex.Result
module Ctype = Cerb_frontend.Ctype

let miss_no_fix_immediate ?(msg = "") () =
  let msg = "MISSING WITH NO FIX " ^ msg in
  L.info (fun m -> m "%s" msg);
  Csymex.push_give_up (msg, get_loc ());
  Missing []

let miss_no_fix ?msg () = Csymex.return (miss_no_fix_immediate ?msg ())

module MemVal = struct
  module TB = Soteria_symex.Tree_block
  module Symex = Csymex

  module SInt = struct
    include Typed
    include Typed.Infix

    type sint = T.sint
    type sbool = T.sbool
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
        let* constrs =
          Csymex.of_opt_not_impl ~msg:"Int constraints"
            (Layout.int_constraints int_ty)
        in
        let+ value = Csymex.nondet ~constrs Typed.t_int in
        ((value :> T.cval Typed.t), ty)
    | _ -> Fmt.kstr not_impl "Nondet of type %a" Fmt_ail.pp_ty ty

  let merge ~left ~right =
    match (left, right) with
    | Zeros, Zeros -> (Zeros, `Complete)
    | Uninit Totally, Uninit Totally -> (Uninit Totally, `Complete)
    | Uninit _, Uninit _ -> (Uninit Partially, `KeepChildren)
    | _, _ -> (Lazy, `KeepChildren)

  let split ~at:_ node =
    let open TB in
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
        | Ctype.Ctype (_, Basic (Integer _)) -> Result.ok 0s
        | Ctype (_, Basic (Floating fty)) ->
            let precision = Layout.precision fty in
            Result.ok (Typed.float precision "+0.0")
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
        L.info (fun m -> m "Reading from Any memory, vanishing.");
        Csymex.vanish ()

  type serialized =
    | SInit of (T.cval Typed.t * Ctype.ctype)
        [@printer Fmt.(pair ~sep:comma Typed.ppa Fmt_ail.pp_ty)]
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
  let serialize (t : t) : serialized option =
    match t with
    | Init v -> Some (SInit v)
    | Uninit Totally -> Some SUninit
    | Zeros -> Some SZeros
    | Any -> Some SAny
    | Lazy | Uninit Partially -> None

  let mk_fix_typed ty () =
    let* v = Layout.nondet_c_ty ty in
    return [ SInit (v, ty) ]

  let mk_fix_any () = [ Any ]

  let consume (s : serialized) (t : (t, T.sint Typed.t) TB.tree) =
    match s with
    | SInit (v, ty) -> (
        match t.node with
        | NotOwned _ ->
            let+ fixes = mk_fix_typed ty () in
            Missing fixes
        | Owned node -> (
            let* sval_res = decode ~ty node in
            match sval_res with
            | Ok sv ->
                let+ () = assume [ sv ==?@ v ] in
                Ok ()
            | Missing f -> miss f
            | Error `UninitializedMemoryAccess -> Csymex.vanish ()))
    | SAny -> (
        match t.node with
        | NotOwned _ -> miss_no_fix ~msg:"consume_any" ()
        | Owned _ -> ok ())
    | SUninit -> (
        match t.node with
        | NotOwned _ -> miss_no_fix ~msg:"consume_uninit" ()
        | Owned (Uninit Totally) -> ok ()
        | _ ->
            L.info (fun m -> m "Consuming uninit but no uninit, vanishing");
            Csymex.vanish ())
    | SZeros -> (
        match t.node with
        | NotOwned _ -> miss_no_fix ~msg:"consume_zeros" ()
        | Owned Zeros -> ok ()
        | Owned (Init (v, _)) ->
            let+ () = Csymex.assume [ v ==?@ 0s ] in
            Ok ()
        | _ ->
            L.info (fun m -> m "Consuming zero but not zero, vanishing");
            Csymex.vanish ())

  let produce : serialized -> t Csymex.t = function
    | SInit v -> return (Init v)
    | SZeros -> return Zeros
    | SUninit -> return (Uninit Totally)
    | SAny -> return Any
end

open MemVal
include Tree_block (MemVal)

let log_fixes fixes =
  L.trace (fun m ->
      m "MISSING WITH FIXES: %a" (Fmt.Dump.list pp_serialized) fixes);
  fixes

let range_of_low_and_type low ty =
  let+ size = Layout.size_of_s ty in
  (low, low +@ size)

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
      Soteria_symex.Compo_res.miss (log_fixes fixes)

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
        | NotOwned Partially -> miss_no_fix ~msg:"partially missing store" ()
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
        | NotOwned Partially -> miss_no_fix ~msg:"partially missing deinit" ()
        | _ -> Result.ok ()
      in
      ((), tree)

let alloc ~zeroed size = alloc (if zeroed then Zeros else Uninit Totally) size
