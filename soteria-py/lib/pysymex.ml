module SYMEX =
  Soteria_symex.Symex.Make_iter
    (struct
      let fuel : Soteria_symex.Fuel_gauge.t = { steps = 1000; branching = 10 }
    end)
    (C_solver.Z3_solver)
