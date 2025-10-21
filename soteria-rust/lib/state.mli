type serialized_atom =
  Typed.T.sloc Typed.t
  * Rtree_block.Make(Sptr.ArithPtr).serialized Rustsymex.Freeable.serialized

type serialized_global = Typed.T.sloc Typed.t
type serialized_ = Heap of serialized_atom | Global of serialized_global

include
  State_intf.S
    with type 'a err = 'a * Charon.Meta.span Soteria.Terminal.Call_trace.t
     and type serialized = serialized_
     and module Sptr = Sptr.ArithPtr
