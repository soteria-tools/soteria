module T = Typed.T
module Expr = Typed.Expr

type 'v agv = Basic of 'v | Struct of 'v agv list | Array of 'v agv list
type t = T.cval Typed.t agv
type syn = Expr.t agv

let rec to_syn = function
  | Basic v -> Basic (Typed.Expr.of_value v)
  | Struct fields -> Struct (List.map to_syn fields)
  | Array elements -> Array (List.map to_syn elements)

let rec subst f = function
  | Basic v -> Basic (Expr.subst f v)
  | Struct fields -> Struct (List.map (subst f) fields)
  | Array elements -> Array (List.map (subst f) elements)

let rec pp_agv pp_v ft =
  let open Fmt in
  function
  | Basic v -> pp_v ft v
  | Struct fields -> braces (list ~sep:comma (pp_agv pp_v)) ft fields
  | Array elements -> brackets (list ~sep:comma (pp_agv pp_v)) ft elements

let pp ft v = (pp_agv Typed.ppa) ft v
let pp_syn ft v = (pp_agv Typed.Expr.pp) ft v
let int_z size z = Basic (Typed.BitVec.mk_masked size z)
let int size i = Basic (Typed.BitVec.mki_masked size i)

let c_int i =
  let c_int_size =
    Option.get
      (Cerb_frontend.Ocaml_implementation.DefaultImpl.impl.sizeof_ity
         (Signed Int_))
  in
  int (c_int_size * 8) i

let void = Basic (Typed.BitVec.zero 8)
let null = Basic Typed.Ptr.null

let basic_or_unsupported ~msg v =
  match v with
  | Basic v -> Csymex.return v
  | Struct _ | Array _ ->
      Fmt.kstr Csymex.not_impl "Not a basic value (%s): %a" msg pp v

let rec iter_vars v f =
  match v with
  | Basic v -> Svalue.iter_vars v f
  | Struct values | Array values ->
      List.iter (fun value -> iter_vars value f) values
