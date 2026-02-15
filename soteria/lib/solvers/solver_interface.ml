(** The interface that SMT solver backends must implement to be used with the
    symbolic execution engine. This signature assumes the solver is mutable.

    This is different from {!Symex.Solver.S}, which represents a solver at a
    more abstract level (where a solver is broadly a path condition). Here
    instead, a solver is an interface into an actual SMT-LIB solver, and exposes
    the usual operations to declare variables, assert values, and check
    satisfiability. *)
module type S = sig
  (** The solver state type. *)
  type t

  (** The type of symbolic expressions. *)
  type value

  (** The type of value types (for variable declarations). *)
  type ty

  (** [init ()] creates a new solver instance. *)
  val init : unit -> t

  (** [add_constraint solver v] adds constraint [v] to the solver.*)
  val add_constraint : t -> value -> unit

  (** [check_sat solver] checks satisfiability of current constraints. This may
      return {!Symex.Solver_result.Unknown} if the solver cannot determine
      satisfiability or if it times out. *)
  val check_sat : t -> Symex.Solver_result.t

  (** [declare_var solver var ty] declares a variable with type [ty]. *)
  val declare_var : t -> Symex.Var.t -> ty -> unit

  (** [push solver n] pushes [n] levels onto the assertion stack.

      Constraints added after push can be removed by {!pop}. *)
  val push : t -> int -> unit

  (** [pop solver n] pops [n] levels from the assertion stack.

      Removes all constraints added since the corresponding {!push}. *)
  val pop : t -> int -> unit

  (** [reset solver] clears all constraints and declarations.

      Returns the solver to its initial state. *)
  val reset : t -> unit
end
