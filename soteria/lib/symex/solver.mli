(** {1 Solver - SMT Solver Interface}

    This module defines the interface for SMT solvers used in symbolic execution.
    Solvers must support incremental solving with push/pop (save/backtrack)
    semantics for efficient path exploration.

    {2 Key Concepts}

    {b Incremental Solving}: The solver maintains a stack of constraint sets.
    [save] pushes a new level, [backtrack_n n] pops [n] levels. This enables
    efficient exploration of branching paths without re-adding constraints.

    {b Mutable State}: Solvers are inherently stateful. The interface reflects
    this with explicit state threading or effect-based access.

    {2 Implementation Notes}

    Implementations should:
    - Maintain a stack of constraint scopes for push/pop
    - Cache/simplify constraints when possible
    - Support timeout configuration for SAT checks
*)

open Soteria_std

(** {2 Mutable Incremental Solver}

    The primary solver interface. Implementations wrap an SMT solver (e.g., Z3)
    with support for incremental solving via save/backtrack. *)
module type Mutable_incremental = sig
  (** The solver state. Fully imperative - all operations mutate this. *)
  include Reversible.Mutable

  (** The value module used for symbolic values and constraints. *)
  module Value : Value.S

  type sbool_v := Value.sbool Value.t

  (** [add_constraints state ~simplified constraints] adds [constraints] to the
      solver's assertion set.

      @param simplified If true, indicates constraints are already simplified.
        If false (default), the solver may simplify them before adding. *)
  val add_constraints : t -> ?simplified:bool -> sbool_v list -> unit

  (** [sat state] checks satisfiability of the current constraint set.
      Returns {!Solver_result.Sat}, {!Solver_result.Unsat}, or
      {!Solver_result.Unknown} (e.g., on timeout). *)
  val sat : t -> Solver_result.t

  (** [simplify state value] simplifies [value] using the current constraint
      context. May use solver-based simplification or abstract analyses. *)
  val simplify : t -> 'a Value.t -> 'a Value.t

  (** [fresh_var state ty] creates a fresh variable of type [ty]. Variable
      names are unique within the solver session. *)
  val fresh_var : t -> 'a Value.ty -> Var.t

  (** [as_values state] returns the current path condition as a list of
      boolean constraints. Used for extracting path conditions after execution. *)
  val as_values : t -> sbool_v list
end

(** {2 Effect-based Wrappers}

    These functors convert mutable solvers into effect-based or pooled versions
    for more convenient use in the symbolic execution monad. *)

(** Wraps a mutable solver to use effects for state access.
    The solver state is accessed via an effect handler. *)
module Mutable_to_effectful (M : Mutable_incremental) : sig
  include Reversible.Effectful
  module Value : module type of M.Value

  val add_constraints : ?simplified:bool -> Value.sbool Value.t list -> unit
  val sat : unit -> Solver_result.t
  val simplify : 'a Value.t -> 'a Value.t
  val fresh_var : 'a Value.ty -> Var.t
  val as_values : unit -> Value.sbool Value.t list
end

(** Wraps a mutable solver to use a solver pool for resource management.
    Solver instances are reused across executions for efficiency. *)
module Mutable_to_pooled (M : Mutable_incremental) : sig
  include Reversible.Pooled
  module Value : module type of M.Value

  val add_constraints : ?simplified:bool -> Value.sbool Value.t list -> unit
  val sat : unit -> Solver_result.t
  val simplify : 'a Value.t -> 'a Value.t
  val fresh_var : 'a Value.ty -> Var.t
  val as_values : unit -> Value.sbool Value.t list
end
