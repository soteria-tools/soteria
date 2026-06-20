(** Ready-to-use Z3 solvers over a built typed layer [Typed] (from
    {!Typed.Make}). Each bundles SMT {!Encoding} with the {!Analyses}-based
    simplifiers.

    There are two variations of the solver, for experimentation:
    - {!Z3_solver} will re-send all relevant constraints to Z3 on each query,
      using [reset] to clear the solver state. We have found this to provide the
      best performance.

    - {!Z3_incremental_solver} will use Z3's native incremental [push]/[pop]
      interface instead of re-sending constraints. Though this sends less data
      to Z3, we have found that it can be slower in practice. Still, we provide
      it as an option, and maintain it for future experimentation. *)

(** Tracks the path condition with its own simplification state and re-sends the
    relevant constraints to Z3 on each query. *)
module Z3_solver (Typed : Typed_intf.Solver_value) :
  Symex.Solver.Mutable_incremental with module Value = Typed

(** Uses Z3's native incremental push/pop interface instead of re-sending
    constraints. *)
module Z3_incremental_solver (Typed : Typed_intf.Solver_value) :
  Symex.Solver.Mutable_incremental with module Value = Typed
