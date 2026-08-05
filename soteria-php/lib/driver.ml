let parse filename =
  match Frontend.parse_file filename with
  | Ok program ->
      program
      |> Php_ir.to_yojson
      |> Yojson.Safe.pretty_to_string
      |> print_endline;
      0
  | Error error ->
      Format.eprintf "Frontend error: %a@." Frontend.Error.pp error;
      2

let discover filename =
  match Frontend.parse_file filename with
  | Ok program ->
      List.iter print_endline (Interp.discover_entry_points program);
      0
  | Error error ->
      Format.eprintf "Frontend error: %a@." Frontend.Error.pp error;
      2

let default_fuel =
  let open Soteria.Symex.Fuel_gauge in
  { steps = Finite 1_000; branching = Finite 100 }

type failure = {
  error : Error.with_trace;
  trace : Error.Trace.t;
  path_condition : Value.Typed.Expr.t list;
}

let execute runtime_event_policy fuel function_name filename =
  let fuel = Soteria.Symex.Fuel_gauge.Cli.validate_or_exit fuel in
  match Frontend.parse_file filename with
  | Error error ->
      Format.eprintf "Frontend error: %a@." Frontend.Error.pp error;
      2
  | Ok program -> (
      match
        match function_name with
        | None -> Ok ()
        | Some name ->
            Result.map (fun _ -> ()) (Interp.validate_entry_point program name)
      with
      | Error message ->
          Format.eprintf "Entry point error: %s@." message;
          2
      | Ok _ ->
          let entry_point = Option.value ~default:filename function_name in
          let results =
            Interp.run ?function_name program
            |> Phpsymex.Result.run_with_trace ~fuel
                 ~mode:Soteria.Symex.Approx.OX
          in
          let failures = ref [] in
          let runtime_event_failures = ref [] in
          let runtime_event_diagnostics = ref [] in
          let incomplete = ref false in
          let expect_failure = ref false in
          List.iter
            (fun (result, path_condition) ->
              match result with
              | Soteria.Soteria_std.Compo_res.Ok (state, trace) ->
                  expect_failure :=
                    trace.Error.Trace.expect_failure || !expect_failure;
                  print_string (State.output state);
                  State.runtime_events state
                  |> List.iter (fun event ->
                      match
                        Config.runtime_event_disposition runtime_event_policy
                          event.Error.Runtime_event.severity
                      with
                      | Config.Ignore_event -> ()
                      | Diagnostic ->
                          runtime_event_diagnostics :=
                            event :: !runtime_event_diagnostics
                      | Bug ->
                          runtime_event_failures :=
                            (event, path_condition) :: !runtime_event_failures)
              | Error (Soteria.Symex.Or_gave_up.E (error, trace)) ->
                  expect_failure :=
                    trace.Error.Trace.expect_failure || !expect_failure;
                  failures := { error; trace; path_condition } :: !failures
              | Error (Soteria.Symex.Or_gave_up.Gave_up reason) ->
                  incomplete := true;
                  Format.eprintf "%s@." reason
              | Missing _ ->
                  incomplete := true;
                  Format.eprintf "Incomplete symbolic execution@.")
            results;
          let failures = List.rev !failures in
          let runtime_event_failures = List.rev !runtime_event_failures in
          let runtime_event_diagnostics = List.rev !runtime_event_diagnostics in
          List.iter Error.Runtime_event_diagnostic.print
            runtime_event_diagnostics;
          if !expect_failure then
            if !incomplete then 3
            else if failures <> [] || runtime_event_failures <> [] then 0
            else (
              Soteria.Terminal.Diagnostic.print_diagnostic_simple
                ~severity:Error
                (Printf.sprintf
                   "Expected failure in entry point %s, but none was found"
                   entry_point);
              1)
          else (
            List.iter
              (fun { error; trace; path_condition } ->
                Format.eprintf "Entry point: %s@." entry_point;
                Error.Diagnostic.print error;
                Counterexample.bindings
                  ~inputs:trace.Error.Trace.symbolic_inputs_rev ~path_condition
                |> Option.iter Counterexample.print)
              failures;
            List.iter
              (fun (event, path_condition) ->
                Format.eprintf "Entry point: %s@." entry_point;
                Error.Runtime_event_diagnostic.print ~as_bug:true event;
                Counterexample.bindings
                  ~inputs:event.Error.Runtime_event.trace.symbolic_inputs_rev
                  ~path_condition
                |> Option.iter Counterexample.print)
              runtime_event_failures;
            if failures <> [] || runtime_event_failures <> [] then 1
            else if !incomplete then 3
            else 0))
