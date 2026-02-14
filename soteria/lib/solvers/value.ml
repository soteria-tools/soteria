open Simple_smt

(** The type of values the solver operators on. *)
module type S = sig
  type t
  type ty

  (** A list of commands that need to be aknowledged on start up of the solver.
      These are also sent everytime {!Solver_interface.S.reset} is called. *)
  val init_commands : sexp list

  (** Encode a type into a SMTLib sort *)
  val sort_of_ty : ty -> sexp

  (** Encode a value into a SMTLib value *)
  val encode_value : t -> sexp
end
