module L = Soteria_logs.Logs.L

module SYMEX =
  Soteria_symex.Symex.Make_iter
    (struct
      let fuel : Soteria_symex.Fuel_gauge.t = { steps = 1000; branching = 10 }
    end)
    (C_solver.Z3_solver)

include SYMEX
include Syntaxes.FunctionWrap

let not_impl msg =
  let msg = "MISSING FEATURE, VANISHING: " ^ msg in
  L.info (fun m -> m "%s" msg);
  vanish ()
