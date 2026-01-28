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
  if%sat b1 then give_up ~loc:() "give up reason"
  else if%sat b2 then Result.miss_no_fix ~reason:"miss no fix" ()
  else if%sat b3 then Result.ok 1
  else Result.error 2

let () =
  Soteria.Logs.Config.(with_config_raw ~config:(make ~hide_unstable:true ()))
  @@ fun () ->
  let { stats; _ } : 'a Stats.with_stats = run_with_stats ~mode:UX process in
  Stats.pp Fmt.stdout stats
