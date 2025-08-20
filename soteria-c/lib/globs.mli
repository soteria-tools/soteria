open Cerb_frontend

type t [@@deriving show]
type serialized = (Symbol.sym * Typed.T.sloc Typed.t) list [@@deriving show]

val serialize : t -> serialized

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

val iter_vars_serialized :
  serialized -> [< Typed.T.cval ] Typed.ty Svalue.Var.iter_vars

val get : Symbol.sym -> t -> (Typed.T.sloc Typed.t * t) Csymex.t
val produce : serialized -> t -> t Csymex.t
val consume : serialized -> t -> (t, 'a, serialized) Csymex.Result.t
val empty : t
