type t = State.t option * State.syn list

include State_intf.S with type t := t with type syn = State.syn

val to_spec : t option -> (pre:syn list * post:syn list)
