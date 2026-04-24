open Cerb_frontend

include
  Soteria.Sym_states.Base.M(Csymex).S
    with type syn = Symbol_std.t * Typed.Expr.t

val empty : t option
val get : Symbol.sym -> t option -> (Typed.T.sloc Typed.t * t option) Csymex.t
