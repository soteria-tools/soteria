open Soteria.Symex.Make (Soteria.Symex.Meta.Dummy) (Tiny_solver.Z3_solver)
open Syntax

let branching_process =
  let* b = nondet Typed.t_bool in
  if%sat b then
    (* At this stage there is no available solver, they're both used.
       This branch should return 0! *)
    return (Solver_pool.total_available ())
  else
    return
      (* We created two solvers, so this branch returns 1 *)
      (Solver_pool.total_created () - 1)

let interrupted_process =
  let* b = nondet Typed.t_bool in
  if%sat b then
    (* We have run our first query.
       Therefore, we have one solver created and zero available
       (used in the current execution.) *)
    let one = Solver_pool.total_created () in
    let zero = Solver_pool.total_available () in
    return (one + 1, [ zero ])
  else
    let v = run ~mode:OX branching_process |> List.map fst in
    (* At this stage, we have created one more solver, but released it now. *)
    let two = Solver_pool.total_created () in
    let one = Solver_pool.total_available () in
    return (one + two, v)

let test_case () =
  let results = run ~mode:OX interrupted_process |> List.map fst in
  Alcotest.(check (list (pair int (list int))))
    "The first branch returns 2 with nothing, the second returns 3 with [0;1] \
     (the result of the interrupting process)"
    [ (2, [ 0 ]); (3, [ 0; 1 ]) ]
    results;
  Alcotest.(check int)
    "Two solvers were created" 2
    (Solver_pool.total_created ());
  Alcotest.(check int)
    "Both solvers are now available" 2
    (Solver_pool.total_available ())

let () =
  Alcotest.run "Interrupt"
    [
      ( "Interrupt",
        [
          Alcotest.test_case
            "interrupting a symbolic execution with another works well" `Quick
            test_case;
        ] );
    ]
