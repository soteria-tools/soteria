include Heap_intf.S

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * Svalue.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized
