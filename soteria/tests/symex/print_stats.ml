open
  Soteria.Symex.Make
    (Soteria.Symex.Meta.Dummy)
    (Soteria.C_values.C_solver.Z3_solver)

open Syntax
open Soteria.C_values.Typed

let process : (int, int, unit) Result.t =
  let* b1 = nondet t_bool in
  let* b2 = nondet t_bool in
  let* b3 = nondet t_bool in
  if%sat b1 then give_up ~loc:() "give up reason"
  else
    if%sat b2 then Result.miss_no_fix ~reason:"miss no fix" ()
    else if%sat b3 then Result.ok 1 else Result.error 2

let () =
  Soteria.Terminal.Config.(set (make ~hide_unstable:true ()));
  let { stats; _ } : 'a Stats.with_stats = run_with_stats ~mode:UX process in
  Stats.pp Fmt.stdout stats
