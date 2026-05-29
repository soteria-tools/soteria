[@@@warning "-unused-constructor"]

open Charon

module Rust_ext : Soteria.Bv_values.Svalue.Value_ext = struct
  (* the full values *)
  type sv = (t, ty) Soteria.Bv_values.Svalue.t
  and svty = ty Soteria.Bv_values.Svalue.ty

  (* types *)
  and ty = Adt of Types.type_decl_ref

  (* pointers; for simplicity's sake, we hardcode the pointer type. We can
     improve this later, but we lack a clear usecase for parametric pointer
     types, and have a clear reason to want deeply-embedded pointers *)
  and ptr = { ptr : sv; tag : Ptr_tag.t option; size : sv; align : sv }

  (* values *)
  and meta = Thin | Len of sv | VTable of sv
  and full_ptr = sv * meta

  and t =
    | Ptr of full_ptr
        (** pointer, parametric to enable Ruxt, with optional meta *)
    | Enum of sv * sv list  (** discriminant * values *)
    | Tuple of sv list  (** contains ordered values *)
    | Union of (sv * sv) list
        (** list of blocks in the union, with their offset *)
    | PolyVal of Charon.Types.type_var_id
        (** The opaque value of a type variable, identified by (type variable
            index, unique identifier). *)
  [@@deriving eq, ord]

  let pp_ty fmt = function Adt t -> Crate.pp_type_decl_ref fmt t

  let pp_meta pp fmt = function
    | Thin -> Fmt.pf fmt "-"
    | Len v | VTable v -> pp fmt v

  let pp_full_ptr pp fmt = function
    | p, Thin -> Fmt.pf fmt "(%a)" pp p
    | p, meta -> Fmt.pf fmt "(%a, %a)" pp p (pp_meta pp) meta

  let pp pp ft v =
    match v with
    | Ptr ptr -> Fmt.pf ft "Ptr%a" (pp_full_ptr pp) ptr
    | Enum (disc, vals) ->
        Fmt.pf ft "Enum(%a: %a)" pp disc (Fmt.list ~sep:(Fmt.any ", ") pp) vals
    | Tuple vals -> Fmt.pf ft "(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp) vals
    | Union vs ->
        let pp_block ft (v, ofs) = Fmt.pf ft "(%a: %a)" pp ofs pp v in
        Fmt.pf ft "Union(%a)" (Fmt.list ~sep:(Fmt.any ", ") pp_block) vs
    | PolyVal tid -> Fmt.pf ft "PolyVal(%a)" Charon.Types.pp_type_var_id tid

  (* TODO: derivable *)
  let iter_vars iter_vars = function
    | Ptr (ptr, Thin) -> iter_vars ptr
    | Ptr (ptr, VTable meta) | Ptr (ptr, Len meta) ->
        iter_vars ptr;
        iter_vars meta
    | Enum (disc, vals) ->
        iter_vars disc;
        List.iter iter_vars vals
    | Tuple vals -> List.iter iter_vars vals
    | Union vs ->
        List.iter
          (fun (v, ofs) ->
            iter_vars v;
            iter_vars ofs)
          vs
    | PolyVal _ -> ()

  (* TODO: so derivable *)
  let hash = function
    | Ptr (ptr, Thin) -> Hashtbl.hash (ptr.tag, 0, 0)
    | Ptr (ptr, Len meta) -> Hashtbl.hash (ptr.tag, 1, meta.tag)
    | Ptr (ptr, VTable meta) -> Hashtbl.hash (ptr.tag, 2, meta.tag)
    | Enum (disc, vals) ->
        Hashtbl.hash (disc, List.map (fun (v : sv) -> v.tag) vals)
    | Tuple vals -> Hashtbl.hash (List.map (fun (v : sv) -> v.tag) vals)
    | Union vs ->
        Hashtbl.hash
          (List.map (fun ((v : sv), (ofs : sv)) -> (v.tag, ofs.tag)) vs)
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

  (* TODO: derivable *)
  and apply_subst apply ~missing_var s = function
    | Ptr (v, Thin) ->
        let v, s = apply ~missing_var s v in
        (Ptr (v, Thin), s)
    | Ptr (v, VTable meta) ->
        let v, s = apply ~missing_var s v in
        let meta, s = apply ~missing_var s meta in
        (Ptr (v, VTable meta), s)
    | Ptr (v, Len meta) ->
        let v, s = apply ~missing_var s v in
        let meta, s = apply ~missing_var s meta in
        (Ptr (v, Len meta), s)
    | Enum (discr, vs) ->
        let discr, s = apply ~missing_var s discr in
        let vs, s = apply_list apply ~missing_var s vs in
        (Enum (discr, vs), s)
    | Tuple vs ->
        let vs, s = apply_list apply ~missing_var s vs in
        (Tuple vs, s)
    | Union vs ->
        let apply ~missing_var s (v, ofs) =
          let v, s = apply ~missing_var s v in
          let ofs, s = apply ~missing_var s ofs in
          ((v, ofs), s)
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
