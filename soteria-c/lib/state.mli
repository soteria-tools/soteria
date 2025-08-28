include
  State_intf.S
    with type 'a err = 'a * Cerb_location.t Soteria.Terminal.Call_trace.t

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized
