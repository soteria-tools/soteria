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

let default_fuel =
  let open Soteria.Symex.Fuel_gauge in
  { steps = Finite 1_000; branching = Finite 100 }

type failure = {
  error : Error.with_trace;
  trace : Error.Trace.t;
  path_condition : Value.Typed.Expr.t list;
}

let execute fuel function_name filename =
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
          let incomplete = ref false in
          let expect_failure = ref false in
          List.iter
            (fun (result, path_condition) ->
              match result with
              | Soteria.Soteria_std.Compo_res.Ok (state, trace) ->
                  expect_failure :=
                    trace.Error.Trace.expect_failure || !expect_failure;
                  print_string (State.output state)
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
          if !expect_failure then
            if !incomplete then 3
            else if failures <> [] then 0
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
            if failures <> [] then 1 else if !incomplete then 3 else 0))
