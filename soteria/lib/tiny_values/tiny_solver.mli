(** Z3-backed solvers for {!Typed} tiny (unbounded-integer / boolean) values.

    These modules satisfy the {{!Soteria.Symex.Solver.Mutable_incremental}
    [Mutable_incremental]} interface, so they can be passed to
    {{!Soteria.Symex.Make} [Symex.Make]} to instantiate a symbolic execution
    engine over tiny values. *)

(** Non-incremental Z3 solver: each query is solved from scratch. Typically
    faster on small isolated queries. *)
module Z3_solver : Symex.Solver.Mutable_incremental with module Value = Typed

(** Incremental Z3 solver: re-uses the underlying solver state across
    push/pop. *)
module Z3_incremental_solver :
  Symex.Solver.Mutable_incremental with module Value = Typed
