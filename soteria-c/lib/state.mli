include
  State_intf.S
    with type 'a err = 'a * Cerb_location.t Soteria.Terminal.Call_trace.t

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var_id.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var_id.t -> Svalue.Var_id.t) -> serialized -> serialized
