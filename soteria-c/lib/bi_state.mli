type t = State.t option * State.serialized list

include State_intf.S with type t := t with type serialized = State.serialized

val to_spec : t option -> serialized list * serialized list
