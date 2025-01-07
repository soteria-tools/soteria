open Csymex

type t = (Heap.t, Heap.serialized) Bi.t
type serialized = Heap.serialized
type 'a err = 'a Heap.err * t

let pp = Bi.pp Heap.pp Heap.pp_serialized

let pp_pretty ~ignore_freed =
  Bi.pp (Heap.pp_pretty ~ignore_freed) Heap.pp_serialized

let empty = (Heap.empty, [])
let bi_wrap f = (Bi.wrap ~produce:Heap.produce) f
let load ptr ty = bi_wrap (Heap.load ptr ty)
let store ptr ty v = bi_wrap (Heap.store ptr ty v)
let free ptr = bi_wrap (Heap.free ptr)
let alloc size = bi_wrap (Heap.alloc size)
let alloc_ty ty = bi_wrap (Heap.alloc_ty ty)
let error e = bi_wrap (Heap.error e)

let copy_nonoverlapping ~dst ~src ~size =
  bi_wrap (Heap.copy_nonoverlapping ~dst ~src ~size)

let produce s t = Bi.produce Heap.produce s t
let consume s t = Bi.consume ~produce:Heap.produce Heap.consume s t
let to_spec ((st, pre) : t) = (pre, Heap.serialize st)
