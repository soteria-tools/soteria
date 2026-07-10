module Raw_symex =
  Soteria.Symex.Make (Soteria.Bv_values.Bv_solver.Z3_solver (Value.Typed))

module Monad = Soteria.Sym_states.State_monad.Make (Raw_symex) (Error.Trace)
include Monad

let get_trace () = get_state ()

let with_location ~location process trace =
  let open Raw_symex.Syntax in
  let+ result, final_trace =
    process { trace with Error.Trace.location = Some location }
  in
  ( result,
    { final_trace with Error.Trace.location = trace.Error.Trace.location } )

let with_call ~location ~message process trace =
  let open Raw_symex.Syntax in
  let call =
    Soteria.Terminal.Call_trace.mk_element ~loc:location ~msg:message ()
  in
  let call_trace = call :: trace.Error.Trace.call_trace in
  let+ result, final_trace = process { trace with Error.Trace.call_trace } in
  ( result,
    { final_trace with Error.Trace.call_trace = trace.Error.Trace.call_trace }
  )

let error ?message reason =
  let open Syntax in
  let* trace = get_trace () in
  Result.error (Error.decorate ?message trace reason)

let not_impl description = give_up ("Unsupported: " ^ description)

let run ?stats ?flamegraph ?fuel ~mode process =
  run_with_state ~state:Error.Trace.empty process
  |> Raw_symex.map fst
  |> Raw_symex.run ?stats ?flamegraph ?fuel ~mode

module Result = struct
  include Result

  let drop_state = function
    | Soteria.Soteria_std.Compo_res.Ok (value, _) ->
        Soteria.Soteria_std.Compo_res.Ok value
    | Error (error, _) -> Error error
    | Missing fixes -> Missing fixes

  let run ?stats ?flamegraph ?fuel ?fail_fast ~mode process =
    run_with_state ~state:Error.Trace.empty process
    |> Raw_symex.map drop_state
    |> Raw_symex.Result.run ?stats ?flamegraph ?fuel ?fail_fast ~mode
end
