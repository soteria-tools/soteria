module Php_value = Value

module Raw_symex =
  Soteria.Symex.Make (Soteria.Bv_values.Bv_solver.Z3_solver (Php_value.Typed))

module Monad = Soteria.Sym_states.State_monad.Make (Raw_symex) (Error.Trace)
include Monad

let get_trace () = get_state ()
let base_branch_on = branch_on

let record_symbolic_input value =
  let open Syntax in
  let* trace = get_state () in
  let index = List.length trace.Error.Trace.symbolic_inputs_rev in
  let input : Error.Trace.symbolic_input =
    { name = Printf.sprintf "input%d" index; value }
  in
  set_state
    {
      trace with
      Error.Trace.symbolic_inputs_rev = input :: trace.symbolic_inputs_rev;
    }

let expect_failure () =
  let open Syntax in
  let* trace = get_state () in
  set_state { trace with Error.Trace.expect_failure = true }

let consume_fuel_steps count =
  let open Syntax in
  let* trace = get_state () in
  let status, fuel =
    Soteria.Symex.Fuel_gauge.consume_fuel_steps count trace.Error.Trace.fuel
  in
  let* () = set_state { trace with Error.Trace.fuel } in
  match status with
  | Soteria.Symex.Fuel_gauge.Not_exhausted -> return ()
  | Exhausted -> give_up "Step fuel exhausted"

let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
  match Php_value.Typed.Bool.to_bool guard with
  | Some _ ->
      base_branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_
  | None -> (
      let open Syntax in
      let* trace = get_state () in
      let status, fuel =
        Soteria.Symex.Fuel_gauge.consume_branching 1 trace.Error.Trace.fuel
      in
      let* () = set_state { trace with Error.Trace.fuel } in
      match status with
      | Soteria.Symex.Fuel_gauge.Not_exhausted ->
          base_branch_on ?left_branch_name ?right_branch_name guard ~then_
            ~else_
      | Exhausted -> give_up "Branching fuel exhausted")

module Syntax = struct
  include Monad.Syntax

  module Symex_syntax = struct
    let branch_on = branch_on
    let branch_on_take_one = branch_on_take_one
    let if_sure = if_sure
  end
end

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

let error_at trace reason = Result.error (Error.decorate trace reason)
let not_impl description = give_up ("Unsupported: " ^ description)

let run ?stats ?flamegraph ?fuel ~mode process =
  let state =
    Option.fold ~none:Error.Trace.empty ~some:Error.Trace.with_fuel fuel
  in
  run_with_state ~state process
  |> Raw_symex.map fst
  |> Raw_symex.run ?stats ?flamegraph ~mode

module Result = struct
  include Result

  let drop_state = function
    | Soteria.Soteria_std.Compo_res.Ok (value, _) ->
        Soteria.Soteria_std.Compo_res.Ok value
    | Error (error, _) -> Error error
    | Missing fixes -> Missing fixes

  let run ?stats ?flamegraph ?fuel ?fail_fast ~mode process =
    let state =
      Option.fold ~none:Error.Trace.empty ~some:Error.Trace.with_fuel fuel
    in
    run_with_state ~state process
    |> Raw_symex.map drop_state
    |> Raw_symex.Result.run ?stats ?flamegraph ?fail_fast ~mode

  let run_with_trace ?stats ?flamegraph ?fuel ?fail_fast ~mode process =
    let state =
      Option.fold ~none:Error.Trace.empty ~some:Error.Trace.with_fuel fuel
    in
    run_with_state ~state process
    |> Raw_symex.Result.run ?stats ?flamegraph ?fail_fast ~mode
end
