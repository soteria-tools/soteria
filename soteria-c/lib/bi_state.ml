open Csymex
include Bi (State)

let pp_pretty ~ignore_freed:_ _ft _t = failwith "to_reimplement"
(* Bi.pp (State.pp_pretty ~ignore_freed) State.pp_serialized *)

let empty = None
let load ptr ty = wrap (State.load ptr ty)
let store ptr ty v = wrap (State.store ptr ty v)
let zero_range dst size = wrap (State.zero_range dst size)
let free ptr = wrap (State.free ptr)
let alloc ?zeroed size = wrap (State.alloc ?zeroed size)
let alloc_ty ty = wrap (State.alloc_ty ty)
let error e = wrap (State.error e)
let get_global sym st = wrap_no_fail (State.get_global sym) st

let copy_nonoverlapping ~dst ~src ~size =
  wrap (State.copy_nonoverlapping ~dst ~src ~size)

let produce_aggregate ptr ty v : unit SM.t =
  let open SM.Syntax in
  let* st = SM.get_state () in
  let st, fixes = of_opt st in
  let*^ (), st = State.produce_aggregate ptr ty v st in
  SM.set_state (to_opt (st, fixes))

(* let consume s t = Bi.consume ~produce:State.produce State.consume s t *)

let to_spec st_opt =
  let st, pre = of_opt st_opt in
  (pre, Option.fold ~none:[] ~some:State.serialize st)
