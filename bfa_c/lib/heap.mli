include Heap_intf.S with type 'a err = 'a * Call_trace.t

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

val consume : serialized -> t -> (t, 'err, serialized list) Csymex.Result.t
val produce : serialized -> t -> t Csymex.t
