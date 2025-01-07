type t = Heap.t * Heap.serialized list

include
  Heap_intf.S
    with type t := t
    with type serialized = Heap.serialized
     and type 'a err = 'a Heap.err * t

val to_spec : t -> serialized list * serialized
