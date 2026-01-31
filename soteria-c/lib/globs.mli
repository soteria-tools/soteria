open Cerb_frontend

type t [@@deriving show]
type serialized = Symbol.sym * Typed.T.sloc Typed.t [@@deriving show]

module SM :
  Soteria.Sym_states.State_monad.S
    with type 'a Symex.t = 'a Csymex.t
     and type st = t option

val serialize : t -> serialized list

val subst_serialized :
  (Svalue.Var.t -> Svalue.Var.t) -> serialized -> serialized

val iter_vars_serialized : serialized -> 'a Typed.ty Svalue.Var.iter_vars
val get : Symbol.sym -> t option -> (Typed.T.sloc Typed.t * t option) Csymex.t
val produce : serialized -> t option -> (unit * t option) Csymex.t

(* val consume : serialized -> t -> (t, [> Csymex.lfail ], serialized)
   Csymex.Result.t *)

val empty : t option
