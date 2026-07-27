[@@@warning "-unused-constructor"]

open Charon
module Sv = Soteria.Bv_values.Svalue

(* the full values *)
type 'ghost sv =
  (('ghost, 'ghost ext_t, 'ghost ext_ty) Sv.t
  [@equal fun a b -> Sv.equal a b] [@compare fun a b -> Sv.compare a b])

and 'ghost svty = 'ghost ext_ty Sv.ty

(* pointers; for simplicity's sake, we hardcode the pointer type. We can improve
   this later, but we lack a clear usecase for parametric pointer types, and
   have a clear reason to want deeply-embedded pointers *)
and 'ghost ptr = {
  ptr : 'ghost sv;
  tag : Ptr_tag.t option;
  size : 'ghost sv;
  align : 'ghost sv;
}

(** The value inside a {!block}: either a scalar or an aggregate that can be
    split further. *)
and ('sc, 'ag) block_value = Scalar of 'sc | Aggregate of 'ag * Types.ty

(** A block of an encoded value: a value, along with the offset it is at and the
    size it spans. [ty] is [Some] iff the value is a whole aggregate of that
    type. *)
and ('sc, 'ag, 'ofs, 'sz) block = {
  value : ('sc, 'ag) block_value;
  offset : 'ofs;
  size : 'sz;
}

(* values *)
and 'ghost ext_ty =
  | TEnum of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** the type decl ref of an {b enum} *)
  | TUnion of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** the type decl ref of a {b union} *)
  | TTuple of 'ghost svty list  (** structs and tuples (ordered fields) *)
  | TArray of 'ghost svty * Z.t
      (** arrays (all elements share the same type) *)
  | TThinPtr
  | TFullPtr
  | TPtrMeta
      (** the type of a pointer's metadata (unit, length or metadata). this is
          not exposed to the user! *)
  | TPolyType

and 'g ptr_meta = MetaLen of 'g sv | MetaVTable of 'g sv | MetaUnit

and 'ghost ext_t =
  | Ptr of 'ghost sv * 'ghost sv  (** pointer, with meta *)
  | PtrMeta of 'ghost ptr_meta
  | ThinPtr of 'ghost ptr
      (** thin pointer, without metadata but with extra info on the pointer *)
  | Enum of Types.variant_id * 'ghost sv list  (** variant id * values *)
  | Tuple of 'ghost sv list  (** structs and tuples: ordered values *)
  | Array of 'ghost sv Iarray.t
      (** arrays: ordered values, all of the same type *)
  | Union of ('ghost sv, 'ghost sv, 'ghost sv, 'ghost sv) block list
      (** list of blocks in the union *)
  | PolyVal of Charon.Types.type_var_id
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)
[@@deriving eq, ord]

let pp_block_value pp_v pp_ag ft = function
  | Scalar v -> pp_v ft v
  | Aggregate (ag, ty) -> Fmt.pf ft "%a : %a" pp_ag ag Types.pp_ty ty

let pp_block pp_v pp_ag pp_ofs pp_sz ft { value; offset; size } =
  Fmt.pf ft "(%a: %a-%a)" pp_ofs offset
    (pp_block_value pp_v pp_ag)
    value pp_sz size

let rec pp_ext_ty ft : 'ghost ext_ty -> unit = function
  | TEnum ty -> Crate.pp_type_decl_ref ft ty
  | TUnion ty -> Crate.pp_type_decl_ref ft ty
  | TTuple tys -> Fmt.(brackets (list ~sep:semi pp_svty)) ft tys
  | TArray (ty, n) -> Fmt.pf ft "[%a; %a]" pp_svty ty Z.pp n
  | TThinPtr -> Fmt.string ft "TThinPtr"
  | TFullPtr -> Fmt.string ft "TFullPtr"
  | TPtrMeta -> Fmt.string ft "TPtrMeta"
  | TPolyType -> Fmt.string ft "TPolyType"

and pp_svty ft (ty : 'ghost svty) = Sv.pp_ty pp_ext_ty ft ty

module Rust_ext :
  Sv.Value_ext
    with type 'ghost t = 'ghost ext_t
     and type 'ghost ty = 'ghost ext_ty = struct
  type 'ghost ty = 'ghost ext_ty
  type 'ghost t = 'ghost ext_t

  let equal = equal_ext_t
  let compare = compare_ext_t
  let equal_ty = equal_ext_ty
  let compare_ty = compare_ext_ty
  let pp_ty ft (ty : 'ghost ty) = pp_ext_ty ft ty

  let pp pp ft v =
    match v with
    | Ptr (ptr, meta) -> Fmt.pf ft "Ptr(%a, %a)" pp ptr pp meta
    | PtrMeta MetaUnit -> Fmt.pf ft "()"
    | PtrMeta (MetaLen len) -> Fmt.pf ft "len(%a)" pp len
    | PtrMeta (MetaVTable vtable) -> Fmt.pf ft "vtable(%a)" pp vtable
    | ThinPtr ptr ->
        Fmt.pf ft "%a[%a]" pp ptr.ptr
          Fmt.(option ~none:(any "*") Ptr_tag.pp)
          ptr.tag
    | Enum (v, vals) ->
        Fmt.pf ft "Enum(%a: %a)" Types.pp_variant_id v
          (Fmt.list ~sep:(Fmt.any ", ") pp)
          vals
    | Tuple vals -> Fmt.pf ft "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp) vals
    | Array vals -> Fmt.pf ft "[%a]" (Iarray.pp ~sep:(Fmt.any ", ") pp) vals
    | Union vs ->
        Fmt.pf ft "Union(%a)"
          (Fmt.list ~sep:(Fmt.any ", ") (pp_block pp pp pp pp))
          vs
    | PolyVal tid -> Fmt.pf ft "PolyVal(%a)" Charon.Types.pp_type_var_id tid

  let iter_vars_ptr iter_vars { ptr; size; align; tag = _ } =
    iter_vars ptr;
    iter_vars size;
    iter_vars align

  let iter_vars_block_value iter_vars = function
    | Scalar v -> iter_vars v
    | Aggregate (ag, _ty) -> iter_vars ag

  let iter_vars_block iter_vars { value; offset; size } =
    iter_vars_block_value iter_vars value;
    iter_vars offset;
    iter_vars size

  (* TODO: derivable *)
  let iter_vars iter_vars = function
    | Ptr (ptr, meta) ->
        iter_vars ptr;
        iter_vars meta
    | PtrMeta MetaUnit -> ()
    | PtrMeta (MetaLen len | MetaVTable len) -> iter_vars len
    | ThinPtr ptr -> iter_vars_ptr iter_vars ptr
    | Enum (_, vals) | Tuple vals -> List.iter iter_vars vals
    | Array vals -> Iarray.iter iter_vars vals
    | Union vs -> List.iter (iter_vars_block iter_vars) vs
    | PolyVal _ -> ()

  (* Allocation-free structural hash *)
  let[@inline] combine h x = (h * 65599) + x

  let rec hash_ty : 'ghost ty -> int = function
    | TEnum ty -> combine 0 (Hashtbl.hash ty)
    | TUnion ty -> combine 1 (Hashtbl.hash ty)
    | TTuple tys ->
        List.fold_left (fun acc ty -> combine acc (Sv.hash_ty hash_ty ty)) 2 tys
    | TThinPtr -> 3
    | TFullPtr -> 4
    | TPolyType -> 5
    | TArray (ty, n) -> combine (combine 6 (Sv.hash_ty hash_ty ty)) (Z.hash n)
    | TPtrMeta -> 7

  (* TODO: so derivable *)
  let hash = function
    | Ptr (ptr, meta) -> combine (combine ptr.tag 1) meta.tag
    | PtrMeta MetaUnit -> combine 2 0
    | PtrMeta (MetaLen v) -> combine 2 (combine v.tag 1)
    | PtrMeta (MetaVTable v) -> combine 2 (combine v.tag 2)
    | ThinPtr { ptr; tag; size; align } ->
        combine
          (combine (combine (combine ptr.tag 3) size.tag) align.tag)
          (Option.fold ~none:(-1) ~some:Ptr_tag.hash tag)
    | Enum (var, vals) ->
        List.fold_left
          (fun acc (v : _ sv) -> combine acc v.tag)
          (combine (Types.VariantId.to_int var) 4)
          vals
    | Tuple vals ->
        List.fold_left (fun acc (v : _ sv) -> combine acc v.tag) 5 vals
    | Array vals ->
        Iarray.fold_left (fun acc (v : _ sv) -> combine acc v.tag) 8 vals
    | Union vs ->
        List.fold_left
          (fun acc { value; offset : _ sv; size : _ sv } ->
            let v =
              match value with
              | Scalar (v : _ sv) -> v.tag
              | Aggregate ((ag : _ sv), ty) -> combine ag.tag (Hashtbl.hash ty)
            in
            combine (combine (combine acc v) offset.tag) size.tag)
          6 vs
    | PolyVal x -> combine 7 (Types.TypeVarId.to_int x)

  (* TODO: re-apply the smart constructors here *)
  let mk _ty v : _ Sv.t_kind = Extension v

  (* TODO: re-apply the smart constructors here *)
  let eval _eval x = x

  let rec apply_list apply ~missing_var s vs =
    match vs with
    | [] -> ([], s)
    | v :: vs ->
        let v, s = apply ~missing_var s v in
        let vs, s = apply_list apply ~missing_var s vs in
        (v :: vs, s)

  let apply_iarray apply ~missing_var s arr =
    let n = Iarray.length arr in
    if n = 0 then (arr, s)
    else begin
      let s = ref s in
      let out =
        Iarray.map
          (fun prev ->
            let v, s' = apply ~missing_var !s prev in
            s := s';
            v)
          arr
      in
      (out, !s)
    end

  let apply_subst_ptr apply ~missing_var s { ptr; size; align; tag } =
    let ptr, s = apply ~missing_var s ptr in
    let size, s = apply ~missing_var s size in
    let align, s = apply ~missing_var s align in
    ({ ptr; size; align; tag }, s)

  let apply_subst_block_value apply ~missing_var s = function
    | Scalar v ->
        let v, s = apply ~missing_var s v in
        (Scalar v, s)
    | Aggregate (ag, ty) ->
        let ag, s = apply ~missing_var s ag in
        (Aggregate (ag, ty), s)

  let apply_subst_block apply ~missing_var s { value; offset; size } =
    let value, s = apply_subst_block_value apply ~missing_var s value in
    let offset, s = apply ~missing_var s offset in
    let size, s = apply ~missing_var s size in
    ({ value; offset; size }, s)

  (* TODO: derivable *)
  let apply_subst apply ~missing_var s = function
    | Ptr (v, m) ->
        let v, s = apply ~missing_var s v in
        let m, s = apply ~missing_var s m in
        (Ptr (v, m), s)
    | PtrMeta MetaUnit -> (PtrMeta MetaUnit, s)
    | PtrMeta (MetaLen v) ->
        let v, s = apply ~missing_var s v in
        (PtrMeta (MetaLen v), s)
    | PtrMeta (MetaVTable v) ->
        let v, s = apply ~missing_var s v in
        (PtrMeta (MetaVTable v), s)
    | ThinPtr ptr ->
        let ptr, s = apply_subst_ptr apply ~missing_var s ptr in
        (ThinPtr ptr, s)
    | Enum (var, vs) ->
        let vs, s = apply_list apply ~missing_var s vs in
        (Enum (var, vs), s)
    | Tuple vs ->
        let vs, s = apply_list apply ~missing_var s vs in
        (Tuple vs, s)
    | Array vs ->
        let vs, s = apply_iarray apply ~missing_var s vs in
        (Array vs, s)
    | Union vs ->
        let vs, s = apply_list (apply_subst_block apply) ~missing_var s vs in
        (Union vs, s)
    | PolyVal _ as v -> (v, s)

  let encode_ty _ = failwith "TODO: encode Rust ext_ty"
  let encode_value _ = failwith "TODO: encode Rust ext_t"
end
