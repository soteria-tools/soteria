include State_intf.S

val serialize : t -> serialized list
val pp_serialized : Format.formatter -> serialized -> unit
val show_serialized : serialized -> string

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * 'a Typed.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized
