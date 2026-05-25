(** Bi-abductive state for Soteria-C.

    A bi-abductive state pairs a regular concrete {!State.t} (the "current"
    state) with a list of {{!State.syn} synthesised} state fragments
    accumulated during abduction. Running the analysis over this state
    automatically discovers the heap shape required to make a program
    execution succeed, yielding a pre-/post-condition pair via {!to_spec}. *)

type t = State.t option * State.syn list
[@@mixins State_intf.S (syn = State.syn)]

(** [to_spec s] extracts the abduced specification: the synthesised
    pre-condition and the resulting post-condition. *)
val to_spec : t option -> (pre:syn list * post:syn list)
