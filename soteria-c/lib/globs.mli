(** Symbolic state for C global variables.

    Tracks the mapping from each global C symbol to its allocated symbolic
    location. Globals are allocated lazily by {!get} the first time the
    interpreter encounters them. *)

open Cerb_frontend

include
  Soteria.Sym_states.Base.M(Csymex).S
    with type syn = Symbol_std.t * Typed.Expr.t

(** The initial (empty) globals state. *)
val empty : t option

(** [get sym s] returns the symbolic location of the global [sym], allocating
    it on the first lookup. *)
val get : Symbol.sym -> t option -> (Typed.T.sloc Typed.t * t option) Csymex.t
