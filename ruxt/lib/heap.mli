open Soteria_rust_lib

module Meta : sig
  type t
end

module Block : sig
  type serialized
end

module Freeable_block_with_meta : sig
  type ('a, 'info) with_info = { node : 'a; info : 'info option }

  type serialized =
    (Block.serialized Soteria.Sym_states.Freeable.freeable, Meta.t) with_info

  val iter_vars_serialized :
    serialized -> (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) -> unit
end

include
  State_intf.S
    with type serialized =
      Typed.T.sloc Typed.t * Freeable_block_with_meta.serialized

type serialized_globals = serialized list * Typed.T.sloc Typed.t list

val serialize : t option -> serialized_globals
val pp_serialized_globals : Format.formatter -> serialized_globals -> unit

val iter_vars_serialized :
  serialized_globals ->
  (Svalue.Var.t * [< Typed.T.cval ] Typed.ty -> unit) ->
  unit

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized_globals -> serialized_globals

val consume : serialized -> (unit, [> Rustsymex.lfail ], serialized) SM.Result.t
val produce : serialized -> unit SM.t
