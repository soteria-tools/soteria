module Z3_solver (Typed : Typed_intf.S) :
  Symex.Solver.Mutable_incremental with module Value = Typed

module Z3_incremental_solver (Typed : Typed_intf.S) :
  Symex.Solver.Mutable_incremental with module Value = Typed
