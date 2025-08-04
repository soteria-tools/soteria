open Cerb_frontend
module T = Typed.T

type field = { name : string; value : t }

and t =
  | Basic of T.cval Typed.t
  | Struct of { tag : Ctype.struct_tag; fields : field list }

let rec pp_field ft { name; value } = Fmt.pf ft "%s=@ %a" name pp value

and pp ft = function
  | Basic v -> Typed.ppa ft v
  | Struct { tag; fields } ->
      let open Fmt in
      pf ft "%a%a" (parens Symbol_std.pp) tag
        (braces (list ~sep:comma pp_field))
        fields

let int_z z = Basic (Typed.int_z z)
let int i = Basic (Typed.int i)
let null = Basic Typed.Ptr.null

let basic_or_unsupported v =
  match v with
  | Basic v -> Csymex.return v
  | Struct _ -> Fmt.kstr Csymex.not_impl "Struct is not a basic value: %a" pp v

let rec iter_vars v f =
  match v with
  | Basic v -> Typed.iter_vars v f
  | Struct { fields; _ } ->
      List.iter (fun { value; _ } -> iter_vars value f) fields

let rec subst f v =
  match v with
  | Basic v -> Basic (Typed.subst f v)
  | Struct { tag; fields } ->
      let fields =
        List.map (fun { name; value } -> { name; value = subst f value }) fields
      in
      Struct { tag; fields }
