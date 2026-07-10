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

let execute fuel filename =
  let fuel = Soteria.Symex.Fuel_gauge.Cli.validate_or_exit fuel in
  match Frontend.parse_file filename with
  | Error error ->
      Format.eprintf "Frontend error: %a@." Frontend.Error.pp error;
      2
  | Ok program ->
      let results =
        Interp.run program
        |> Phpsymex.Result.run ~fuel ~mode:Soteria.Symex.Approx.OX
      in
      let failed = ref false in
      let incomplete = ref false in
      List.iter
        (fun (result, _) ->
          match result with
          | Soteria.Soteria_std.Compo_res.Ok state ->
              print_string (State.output state)
          | Error (Soteria.Symex.Or_gave_up.E error) ->
              failed := true;
              Error.Diagnostic.print error
          | Error (Soteria.Symex.Or_gave_up.Gave_up reason) ->
              incomplete := true;
              Format.eprintf "%s@." reason
          | Missing _ ->
              incomplete := true;
              Format.eprintf "Incomplete symbolic execution@.")
        results;
      if !failed then 1 else if !incomplete then 3 else 0
