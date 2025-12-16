open Soteria_rust_lib

module Meta : sig
  type t
end

module With_meta : sig
  type 'a t = { node : 'a; info : Meta.t option }
  type 'a serialized = 'a t

  val iter_vars_serialized : ('a -> 'b) -> 'a serialized -> 'b
end

module Freeable : sig
  type 'a serialized = 'a Soteria.Sym_states.Freeable.t

  val iter_vars_serialized : ('a -> 'b -> unit) -> 'a serialized -> 'b -> unit
end

module Tree_block : sig
  type serialized

  val iter_vars_serialized :
    serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit
end

type serialized_atom =
  Typed.T.sloc Typed.t
  * Tree_block.serialized Freeable.serialized With_meta.serialized

include
  State_intf.S
    with type 'a err = 'a * Charon.Meta.span_data Soteria.Terminal.Call_trace.t
     and type serialized = serialized_atom list * Typed.T.sloc Typed.t list

val serialize : t -> serialized
val pp_serialized : Format.formatter -> serialized -> unit

val iter_vars_serialized :
  serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

val consume :
  serialized_atom ->
  t ->
  (t, [> Rustsymex.lfail ], serialized) Rustsymex.Result.t

val produce : serialized -> t -> t Rustsymex.t
