(** {1 SMT Solver Results}

    This module defines the possible outcomes of SMT solver queries
    and utilities for interpreting them in different approximation modes.

    {2 Overview}

    An SMT query can return three possible results:
    - {b Sat}: The formula is satisfiable (has a model)
    - {b Unsat}: The formula is unsatisfiable (no model exists)
    - {b Unknown}: The solver could not determine satisfiability

    The interpretation of [Unknown] depends on the approximation mode
    (under-approximate vs over-approximate).
*)

(** The type of solver results. *)
type t =
  | Sat      (** The formula is satisfiable *)
  | Unsat    (** The formula is unsatisfiable *)
  | Unknown  (** Satisfiability could not be determined *)

(** [pp fmt result] pretty-prints the solver result. *)
val pp : Format.formatter -> t -> unit

(** [is_sat result] returns [true] if [result] is [Sat]. *)
val is_sat : t -> bool

(** [is_unsat result] returns [true] if [result] is [Unsat]. *)
val is_unsat : t -> bool

(** [admissible ~mode result] determines whether a branch should be taken
    based on the solver result and approximation mode.

    The decision matrix:
    - [Sat, _]: Always take the branch (definitely feasible)
    - [Unsat, _]: Never take the branch (definitely infeasible)
    - [Unknown, OX]: Take the branch (over-approximate: don't miss paths)
    - [Unknown, UX]: Don't take the branch (under-approximate: avoid spurious paths)

    @param mode The current approximation mode ({!Approx.UX} or {!Approx.OX})
    @return [true] if the branch should be explored, [false] otherwise *)
val admissible : mode:Approx.t -> t -> bool
