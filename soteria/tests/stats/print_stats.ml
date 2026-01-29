open
  Soteria.Symex.Make
    (Soteria.Symex.Meta.Dummy)
    (Soteria.Tiny_values.Tiny_solver.Z3_solver)

open Syntax
open Soteria.Tiny_values.Typed

let process : (int, int, unit) Result.t =
  let* b1 = nondet t_bool in
  let* b2 = nondet t_bool in
  let* b3 = nondet t_bool in
  let* b4 = nondet t_bool in
  if%sat b1 then give_up ~loc:() "give up reason"
  else if%sat b2 then Result.miss_no_fix ~reason:"miss no fix" ()
  else if%sat b3 then Result.miss_no_fix ~reason:"other miss no fix" ()
  else if%sat b4 then Result.ok 1
  else Result.error 2

let () =
  Soteria.Logs.Config.(set_and_lock (make ~hide_unstable:true ()));
  let { stats; _ } : 'a Soteria.Stats.with_stats =
    run_with_stats ~mode:UX process
  in
  Soteria.Stats.pp Fmt.stdout stats;
  Fmt.pr "@.";
  let as_yojson = Soteria.Stats.to_yojson stats in
  let as_yojson_stable =
    Yojson.Safe.Util.to_assoc as_yojson
    |> List.map (function
      | k, `Float _ -> (k, `String "<float>")
      | k, v -> (k, v))
    |> fun lst -> `Assoc lst
  in
  Yojson.Safe.pretty_to_channel stdout as_yojson_stable;
  Fmt.pr "@.@.";
  match Soteria.Stats.of_yojson as_yojson with
  | Ok stats' ->
      let as_yojson' = Soteria.Stats.to_yojson stats' in
      if Yojson.Safe.equal as_yojson as_yojson' then
        Fmt.pr "Serialization round-trip successful.@."
      else Fmt.pr "Serialization round-trip failed.@."
  | Error err -> Fmt.pr "Deserialization failed: %s@." err
