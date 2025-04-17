open Csymex

type t = (State.t, State.serialized) Bi.t
type serialized = State.serialized
type 'a err = 'a State.err * t

let add_to_call_trace (err, state) trace_elem =
  (State.add_to_call_trace err trace_elem, state)

let pp = Bi.pp State.pp State.pp_serialized

let pp_pretty ~ignore_freed =
  Bi.pp (State.pp_pretty ~ignore_freed) State.pp_serialized

let empty = (State.empty, [])
let bi_wrap f = (Bi.wrap ~produce:State.produce) f
let load ptr ty = bi_wrap (State.load ptr ty)
let store ptr ty v = bi_wrap (State.store ptr ty v)
let free ptr = bi_wrap (State.free ptr)
let alloc size = bi_wrap (State.alloc size)
let alloc_ty ty = bi_wrap (State.alloc_ty ty)
let error e = bi_wrap (State.error e)
let get_global sym st = Bi.wrap_no_fail (State.get_global sym) st

let copy_nonoverlapping ~dst ~src ~size =
  bi_wrap (State.copy_nonoverlapping ~dst ~src ~size)

let produce s t = Bi.produce State.produce s t
let consume s t = Bi.consume ~produce:State.produce State.consume s t
let to_spec ((st, pre) : t) = (pre, State.serialize st)
