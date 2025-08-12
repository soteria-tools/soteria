module T = Typed.T

type t = Basic of T.cval Typed.t | Struct of t list

let rec pp ft =
  let open Fmt in
  function
  | Basic v -> Typed.ppa ft v
  | Struct fields -> braces (list ~sep:comma pp) ft fields

let int_z z = Basic (Typed.int_z z)
let int i = Basic (Typed.int i)
let null = Basic Typed.Ptr.null

let basic_or_unsupported ~msg v =
  match v with
  | Basic v -> Csymex.return v
  | Struct _ -> Fmt.kstr Csymex.not_impl "Not a basic value (%s): %a" msg pp v

let rec iter_vars v f =
  match v with
  | Basic v -> Typed.iter_vars v f
  | Struct fields -> List.iter (fun value -> iter_vars value f) fields

let rec subst f v =
  match v with
  | Basic v -> Basic (Typed.subst f v)
  | Struct fields ->
      let fields = List.map (subst f) fields in
      Struct fields
