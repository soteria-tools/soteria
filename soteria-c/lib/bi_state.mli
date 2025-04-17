type t = State.t * State.serialized list

include
  State_intf.S
    with type t := t
    with type serialized = State.serialized
     and type 'a err = 'a State.err * t

val to_spec : t -> serialized list * serialized
