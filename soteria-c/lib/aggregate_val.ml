module T = Typed.T
module Expr = Typed.Expr

type 'v agv = Basic of 'v | Struct of 'v agv list | Array of 'v agv list

(* A basic value can be of any kind (int/ptr/float), so it is held
   existentially-wrapped. *)
type t = Svalue.packed agv
type syn = Expr.t agv

let rec to_syn = function
  | Basic v -> Basic v
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

let pp ft v = (pp_agv Typed.Expr.pp) ft v
let pp_syn ft v = (pp_agv Typed.Expr.pp) ft v

(* Wrap a typed value into a basic aggregate leaf. *)
let basic v = Basic (Svalue.Packed v)
let int_z size z = Basic (Svalue.Packed (Typed.BitVec.mk_masked size z))
let int size i = Basic (Svalue.Packed (Typed.BitVec.mki_masked size i))

let c_int i =
  let c_int_size =
    Option.get
      (Cerb_frontend.Ocaml_implementation.DefaultImpl.impl.sizeof_ity
         (Signed Int_))
  in
  int (c_int_size * 8) i

let void = Basic (Svalue.Packed (Typed.BitVec.zero 8))
let null = Basic (Svalue.Packed Typed.Ptr.null)

let basic_or_unsupported ~msg v =
  match v with
  | Basic v -> Csymex.return v
  | Struct _ | Array _ ->
      Fmt.kstr Csymex.not_impl "Not a basic value (%s): %a" msg pp v

let rec iter_vars v f =
  match v with
  | Basic (Svalue.Packed v) -> Svalue.iter_vars v f
  | Struct values | Array values ->
      List.iter (fun value -> iter_vars value f) values
