type t =
  | Sat  (** The formula is satisfiable *)
  | Unsat  (** The formula is unsatisfiable *)
  | Unknown
      (** Satisfiability could not be determined (e.g. due to a timeout) *)

let pp fmt = function
  | Sat -> Format.fprintf fmt "Sat"
  | Unsat -> Format.fprintf fmt "Unsat"
  | Unknown -> Format.fprintf fmt "Unknown"

let is_sat = function Sat -> true | Unsat | Unknown -> false
let is_unsat = function Unsat -> true | Sat | Unknown -> false

(** [admissible ~mode result] determines whether a branch should be taken based
    on the solver result and approximation mode.

    The decision matrix:
    - [Sat, _]: Always take the branch (definitely feasible)
    - [Unsat, _]: Never take the branch (definitely infeasible)
    - [Unknown, OX]: Take the branch (over-approximate: don't miss paths)
    - [Unknown, UX]: Don't take the branch (under-approximate: avoid possibly
      unsatisfiable paths) *)
let admissible ~(mode : Approx.t) res =
  match (res, mode) with
  (* A Sat branch is always taken. If it's unknown and in OX mode, we must take
     it to avoid dropping potentially valid branches *)
  | Sat, _ | Unknown, OX -> true
  (* An Unsat branch is always dropped. If it's unknown and in UX mode, we must
     drop it to avoid taking potentially invalid branches *)
  | Unsat, _ | Unknown, UX -> false
