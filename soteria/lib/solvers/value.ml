open Simple_smt

(** The type of values the solver operators on.

    This is different from {!Symex.Value.S}, which represents symbolic values in
    the context of symbolic execution. Here we are simply interested in how some
    arbitrary typed values can be encoded into SMT-LIB. *)
module type S = sig
  type t
  type ty

  (** A list of commands that need to be aknowledged on start up of the solver.
      These are also sent everytime {!Solver_interface.S.reset} is called. *)
  val init_commands : sexp list

  (** Encode a type into a SMT-LIB sort *)
  val sort_of_ty : ty -> sexp

  (** Encode a value into a SMT-LIB value *)
  val encode_value : t -> sexp
end
