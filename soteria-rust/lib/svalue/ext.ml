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

(* values *)
and 'ghost ext_ty =
  | TEnum of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** the type decl ref of an {b enum} *)
  | TUnion of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** the type decl ref of a {b union} *)
  | TTuple of 'ghost svty list
      (** structs, tuples and arrays (ordered fields) *)
  | TThinPtr
  | TFullPtr
  | TPolyType

and 'ghost ext_t =
  | Ptr of 'ghost sv * 'ghost sv option  (** pointer, with optional meta *)
  | ThinPtr of 'ghost ptr
      (** thin pointer, without metadata but with extra info on the pointer *)
  | Enum of 'ghost sv * 'ghost sv list  (** discriminant * values *)
  | Tuple of 'ghost sv list  (** contains ordered values *)
  | Union of ('ghost sv * 'ghost sv * 'ghost sv) list
      (** list of blocks in the union, with their offset and size *)
  | PolyVal of Charon.Types.type_var_id
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)
[@@deriving eq, ord]

let rec pp_ext_ty ft : 'ghost ext_ty -> unit = function
  | TEnum ty -> Crate.pp_type_decl_ref ft ty
  | TUnion ty -> Crate.pp_type_decl_ref ft ty
  | TTuple tys -> Fmt.(brackets (list ~sep:semi pp_svty)) ft tys
  | TThinPtr -> Fmt.string ft "TThinPtr"
  | TFullPtr -> Fmt.string ft "TFullPtr"
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
    | Ptr (ptr, None) -> Fmt.pf ft "Ptr(%a)" pp ptr
    | Ptr (ptr, Some meta) -> Fmt.pf ft "Ptr(%a, %a)" pp ptr pp meta
    | ThinPtr ptr ->
        Fmt.pf ft "%a[%a]" pp ptr.ptr
          Fmt.(option ~none:(any "*") Ptr_tag.pp)
          ptr.tag
    | Enum (disc, vals) ->
        Fmt.pf ft "Enum(%a: %a)" pp disc (Fmt.list ~sep:(Fmt.any ", ") pp) vals
    | Tuple vals -> Fmt.pf ft "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp) vals
    | Union vs ->
        let pp_block ft (v, ofs, size) =
          Fmt.pf ft "(%a: %a-%a)" pp ofs pp v pp size
        in
        Fmt.pf ft "Union(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_block) vs
    | PolyVal tid -> Fmt.pf ft "PolyVal(%a)" Charon.Types.pp_type_var_id tid

  let iter_vars_ptr iter_vars { ptr; size; align; tag = _ } =
    iter_vars ptr;
    iter_vars size;
    iter_vars align

  (* TODO: derivable *)
  let iter_vars iter_vars = function
    | Ptr (ptr, None) -> iter_vars ptr
    | Ptr (ptr, Some meta) ->
        iter_vars ptr;
        iter_vars meta
    | ThinPtr ptr -> iter_vars_ptr iter_vars ptr
    | Enum (disc, vals) ->
        iter_vars disc;
        List.iter iter_vars vals
    | Tuple vals -> List.iter iter_vars vals
    | Union vs ->
        List.iter
          (fun (v, ofs, size) ->
            iter_vars v;
            iter_vars ofs;
            iter_vars size)
          vs
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

  (* TODO: so derivable *)
  let hash = function
    | Ptr (ptr, None) -> combine (combine ptr.tag 1) 0
    | Ptr (ptr, Some meta) -> combine (combine ptr.tag 2) meta.tag
    | ThinPtr { ptr; tag; size; align } ->
        combine
          (combine (combine (combine ptr.tag 3) size.tag) align.tag)
          (Option.fold ~none:(-1) ~some:Ptr_tag.hash tag)
    | Enum (disc, vals) ->
        List.fold_left
          (fun acc (v : _ sv) -> combine acc v.tag)
          (combine disc.tag 4) vals
    | Tuple vals ->
        List.fold_left (fun acc (v : _ sv) -> combine acc v.tag) 5 vals
    | Union vs ->
        List.fold_left
          (fun acc ((v : _ sv), (ofs : _ sv), (size : _ sv)) ->
            combine (combine (combine acc v.tag) ofs.tag) size.tag)
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

  let apply_subst_ptr apply ~missing_var s { ptr; size; align; tag } =
    let ptr, s = apply ~missing_var s ptr in
    let size, s = apply ~missing_var s size in
    let align, s = apply ~missing_var s align in
    ({ ptr; size; align; tag }, s)

  (* TODO: derivable *)
  let apply_subst apply ~missing_var s = function
    | Ptr (v, None) ->
        let v, s = apply ~missing_var s v in
        (Ptr (v, None), s)
    | Ptr (v, Some meta) ->
        let v, s = apply ~missing_var s v in
        let meta, s = apply ~missing_var s meta in
        (Ptr (v, Some meta), s)
    | ThinPtr ptr ->
        let ptr, s = apply_subst_ptr apply ~missing_var s ptr in
        (ThinPtr ptr, s)
    | Enum (discr, vs) ->
        let discr, s = apply ~missing_var s discr in
        let vs, s = apply_list apply ~missing_var s vs in
        (Enum (discr, vs), s)
    | Tuple vs ->
        let vs, s = apply_list apply ~missing_var s vs in
        (Tuple vs, s)
    | Union vs ->
        let apply ~missing_var s (v, ofs, size) =
          let v, s = apply ~missing_var s v in
          let ofs, s = apply ~missing_var s ofs in
          let size, s = apply ~missing_var s size in
          ((v, ofs, size), s)
        in
        let vs, s = apply_list apply ~missing_var s vs in
        (Union vs, s)
    | PolyVal _ as v -> (v, s)

  let encode_ty _ = failwith "TODO: encode Rust ext_ty"
  let encode_value _ = failwith "TODO: encode Rust ext_t"
end
