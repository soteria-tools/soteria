(** {1 SMT Solver Interface}

    This module defines the interface that SMT solver backends must implement
    to be used with the symbolic execution engine.

    {2 Overview}

    The solver interface provides:
    - Variable declaration
    - Constraint addition
    - Satisfiability checking
    - Incremental solving via push/pop

    Different solver backends (Z3, CVC5, etc.) implement this interface.

    {2 Usage}

    Solvers are typically used internally by the symbolic execution engine.
    Direct usage might look like:

    {[
      let solver = MySolver.init () in
      MySolver.declare_var solver x TInt;
      MySolver.add_constraint solver (x >@ 0);
      match MySolver.check_sat solver with
      | Sat -> (* formula is satisfiable *)
      | Unsat -> (* formula is unsatisfiable *)
      | Unknown -> (* could not determine *)
    ]}
*)

(** The signature for SMT solver implementations. *)
module type S = sig
  (** The solver state type. *)
  type t

  (** The type of constraint values (typically symbolic expressions). *)
  type value

  (** The type of value types (for variable declarations). *)
  type ty

  (** [init ()] creates a new solver instance. *)
  val init : unit -> t

  (** [add_constraint solver v] adds constraint [v] to the solver.

      The constraint is added to the current assertion stack level. *)
  val add_constraint : t -> value -> unit

  (** [check_sat solver] checks satisfiability of current constraints.

      Returns:
      - [Sat]: All constraints can be satisfied
      - [Unsat]: Constraints are contradictory
      - [Unknown]: Solver could not determine (timeout, etc.) *)
  val check_sat : t -> Symex.Solver_result.t

  (** [declare_var solver var ty] declares a variable with type [ty].

      Must be called before using the variable in constraints. *)
  val declare_var : t -> Symex.Var.t -> ty -> unit

  (** [push solver n] pushes [n] levels onto the assertion stack.

      Constraints added after push can be removed by pop. *)
  val push : t -> int -> unit

  (** [pop solver n] pops [n] levels from the assertion stack.

      Removes all constraints added since the corresponding push. *)
  val pop : t -> int -> unit

  (** [reset solver] clears all constraints and declarations.

      Returns the solver to its initial state. *)
  val reset : t -> unit
end
