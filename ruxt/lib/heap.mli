open Soteria_rust_lib

module Tree_block : sig
  type serialized

  val iter_vars_serialized :
    serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit

  val subst_serialized :
    (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized
end

include
  State_intf.S
    with type 'a err = 'a * Charon.Meta.span Soteria_terminal.Call_trace.t
     and type serialized =
      (Typed.T.sloc Typed.t
      * (Tree_block.serialized Rustsymex.Freeable.serialized * bool))
      list

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

val consume : serialized -> t -> (t, 'err, serialized list) Rustsymex.Result.t
val produce : serialized -> t -> t Rustsymex.t
