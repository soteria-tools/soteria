open Cerb_frontend
module T = Typed.T

type field = { name : string; value : t }

and t =
  | Basic of T.cval Typed.t
  | Struct of { tag : Ctype.struct_tag; fields : field list }
  | Array of { elems : t list; elems_ty : Ctype.ctype }

let rec pp_field ft { name; value } = Fmt.pf ft "%s=@ %a" name pp value

and pp ft =
  let open Fmt in
  function
  | Basic v -> Typed.ppa ft v
  | Struct { tag; fields } ->
      pf ft "%a%a" (parens Symbol_std.pp) tag
        (braces (list ~sep:comma pp_field))
        fields
  | Array { elems; elems_ty } ->
      pf ft "([%a])[%a]" Fmt_ail.pp_ty elems_ty (Fmt.list ~sep:comma pp) elems

let int_z z = Basic (Typed.int_z z)
let int i = Basic (Typed.int i)
let null = Basic Typed.Ptr.null

let basic_or_unsupported ~msg v =
  match v with
  | Basic v -> Csymex.return v
  | Struct _ | Array _ ->
      Fmt.kstr Csymex.not_impl "Not a basic value (%s): %a" msg pp v

let rec iter_vars v f =
  match v with
  | Basic v -> Typed.iter_vars v f
  | Struct { fields; _ } ->
      List.iter (fun { value; _ } -> iter_vars value f) fields
  | Array { elems; _ } -> List.iter (fun e -> iter_vars e f) elems

let rec subst f v =
  match v with
  | Basic v -> Basic (Typed.subst f v)
  | Struct { tag; fields } ->
      let fields =
        List.map (fun { name; value } -> { name; value = subst f value }) fields
      in
      Struct { tag; fields }
  | Array { elems; elems_ty } ->
      let elems = List.map (fun e -> subst f e) elems in
      Array { elems; elems_ty }
