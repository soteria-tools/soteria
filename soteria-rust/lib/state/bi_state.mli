type t = Tree_state.t option * Tree_state.serialized list

include
  State_intf.S with type t := t with type serialized = Tree_state.serialized

val to_spec : t option -> serialized list * serialized list
