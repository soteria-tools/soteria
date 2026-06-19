[@@@warning "-unused-constructor"]

open Charon

(* the full values *)
type sv = (ext_t, ext_ty) Soteria.Bv_values.Svalue.t
and svty = ext_ty Soteria.Bv_values.Svalue.ty

(* pointers; for simplicity's sake, we hardcode the pointer type. We can improve
   this later, but we lack a clear usecase for parametric pointer types, and
   have a clear reason to want deeply-embedded pointers *)
and ptr = { ptr : sv; tag : Ptr_tag.t option; size : sv; align : sv }

(* values *)
and ext_ty =
  | TAdt of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
      (** invariant: the type decl ref must be that of an {b enum or union};
          structs and tuples go through [TTuple] *)
  | TTuple of svty list
  | TThinPtr
  | TFullPtr
  | TPolyType
[@@deriving eq, ord, show]

and ext_t =
  | Ptr of sv * sv option  (** pointer, with optional meta *)
  | ThinPtr of ptr
      (** thin pointer, without metadata but with extra info on the pointer *)
  | Enum of sv * sv list  (** discriminant * values *)
  | Tuple of sv list  (** contains ordered values *)
  | Union of (sv * sv * sv) list
      (** list of blocks in the union, with their offset and size *)
  | PolyVal of Charon.Types.type_var_id
      (** The opaque value of a type variable, identified by (type variable
          index, unique identifier). *)
[@@deriving eq, ord]

module Rust_ext :
  Soteria.Bv_values.Svalue.Value_ext with type t = ext_t and type ty = ext_ty =
struct
  type ty = ext_ty =
    | TAdt of (Types.type_decl_ref[@printer Crate.pp_type_decl_ref])
    | TTuple of svty list
    | TThinPtr
    | TFullPtr
    | TPolyType
  [@@deriving eq, ord, show]

  type t = ext_t =
    | Ptr of sv * sv option  (** pointer, with optional meta *)
    | ThinPtr of ptr
        (** thin pointer, without metadata but with extra info on the pointer *)
    | Enum of sv * sv list  (** discriminant * values *)
    | Tuple of sv list  (** contains ordered values *)
    | Union of (sv * sv * sv) list
        (** list of blocks in the union, with their offset and size *)
    | PolyVal of Charon.Types.type_var_id
        (** The opaque value of a type variable, identified by (type variable
            index, unique identifier). *)
  [@@deriving eq, ord]

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

  (* TODO: so derivable *)
  let hash = function
    | Ptr (ptr, None) -> Hashtbl.hash (ptr.tag, 0, 0)
    | Ptr (ptr, Some meta) -> Hashtbl.hash (ptr.tag, 1, meta.tag)
    | ThinPtr { ptr; tag; size; align } ->
        Hashtbl.hash (ptr.tag, size.tag, align.tag, tag)
    | Enum (disc, vals) ->
        Hashtbl.hash (disc, List.map (fun (v : sv) -> v.tag) vals)
    | Tuple vals -> Hashtbl.hash (List.map (fun (v : sv) -> v.tag) vals)
    | Union vs ->
        Hashtbl.hash
          (List.map
             (fun ((v : sv), (ofs : sv), (size : sv)) ->
               (v.tag, ofs.tag, size.tag))
             vs)
    | PolyVal x -> Hashtbl.hash x

  (* TODO: ?? *)
  let mk _ty _v = failwith ""

  (* TODO: TBD *)
  let eval _eval x = x

  let rec apply_list apply ~missing_var s vs =
    match vs with
    | [] -> ([], s)
    | v :: vs ->
        let v, s = apply ~missing_var s v in
        let vs, s = apply_list apply ~missing_var s vs in
        (v :: vs, s)

  and apply_subst_ptr apply ~missing_var s { ptr; size; align; tag } =
    let ptr, s = apply ~missing_var s ptr in
    let size, s = apply ~missing_var s size in
    let align, s = apply ~missing_var s align in
    ({ ptr; size; align; tag }, s)

  (* TODO: derivable *)
  and apply_subst apply ~missing_var s = function
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
        (* FIXME(OCaml 5.5): this can be removed with polymorphic functions *)
        let rec apply_list ~missing_var s vs =
          match vs with
          | [] -> ([], s)
          | v :: vs ->
              let v, s = apply ~missing_var s v in
              let vs, s = apply_list ~missing_var s vs in
              (v :: vs, s)
        in
        let vs, s = apply_list ~missing_var s vs in
        (Union vs, s)
    | PolyVal _ as v -> (v, s)

  let encode_ty _ = failwith ""
  let encode_value _ = failwith ""
end
