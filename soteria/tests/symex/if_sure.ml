open
  Soteria.Symex.Make (Soteria.Symex.Meta.Dummy) (Soteria.Symex.Mut.Dummy)
    (Soteria.C_values.C_solver.Z3_solver)

open Syntax
open Soteria.C_values.Typed
open Infix

let if_true = if%sure v_true then return 42 else return (-1)
let if_false = if%sure v_false then return (-1) else return 42

let if_maybe =
  let* b = nondet t_bool in
  if%sure b then return (-1) else return 42

let if_guranteed =
  let* x = nondet t_int in
  let* () = assume [ x >@ one ] in
  if%sure x >@ zero then return 42 else return (-1)

let () =
  let pp = Fmt.(Dump.list int) in
  Fmt.pr "if_true: %a@\n" pp (List.map fst @@ run ~mode:OX if_true);
  Fmt.pr "if_false: %a@\n" pp (List.map fst @@ run ~mode:OX if_false);
  Fmt.pr "if_maybe: %a@\n" pp (List.map fst @@ run ~mode:OX if_maybe);
  Fmt.pr "if_guranteed: %a@\n" pp (List.map fst @@ run ~mode:OX if_guranteed)
