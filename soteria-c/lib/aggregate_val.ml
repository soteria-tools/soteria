module T = Typed.T

type t = Basic of T.cval Typed.t | Struct of t list

let rec pp ft =
  let open Fmt in
  function
  | Basic v -> Typed.ppa ft v
  | Struct fields -> braces (list ~sep:comma pp) ft fields

let int_z size z = Basic (Typed.BitVec.mk_masked size z)
let int size i = Basic (Typed.BitVec.mki_masked size i)
let void = Basic (Typed.BitVec.zero 8)
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
