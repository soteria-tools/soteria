type t = State.t option * State.syn list
[@@mixins State_intf.S (syn = State.syn)]

val to_spec : t option -> (pre:syn list * post:syn list)
