open Soteria.Symex.Make (Tiny_solver.Z3_solver)
open Syntax

(* ============================================================================
   Test Helpers
   ============================================================================ *)

let fuel = Soteria.Symex.Fuel_gauge.infinite
let count_outcomes process = run ~fuel ~mode:UX process |> List.length

(* ============================================================================
   Basic Outcome Tests
   ============================================================================ *)

let process_one = assume [ Typed.v_false ]
let process_two = assume [ Typed.v_true ]

let process_with_false_assumption_has_no_outcomes () =
  let outcomes = count_outcomes process_one in
  Alcotest.(check int) "outcomes" 0 outcomes

let process_with_true_assumption_has_one_outcome () =
  let outcomes = count_outcomes process_two in
  Alcotest.(check int) "outcomes" 1 outcomes

(* ============================================================================
   Determinism Tests
   ============================================================================ *)

let fn () =
  let open Typed.Infix in
  let* v = nondet Typed.t_int in
  let* () = assume [ v ==@ Typed.zero ] in
  if%sat Typed.not (v ==@ Typed.one) then return true else return false

let branches_are_deterministic () =
  let pp =
    let open Fmt in
    list ~sep:semi (braces (pair ~sep:comma bool (Dump.list Typed.ppa)))
  in
  let b1 = run ~fuel ~mode:OX (fn ()) in
  let b2 = run ~fuel ~mode:OX (fn ()) in
  let s1 = Fmt.str "%a" pp b1 in
  let s2 = Fmt.str "%a" pp b2 in
  Alcotest.(check string) "branches are identical" s1 s2

(* ============================================================================
   Solver Pool Tests
   ============================================================================ *)

let sequential_runs_reuse_single_solver () =
  (* Run the previous tests to ensure solver is used *)
  let _ = count_outcomes process_one in
  let _ = count_outcomes process_two in
  let _ = run ~fuel ~mode:OX (fn ()) in
  let _ = run ~fuel ~mode:OX (fn ()) in
  let created = Solver_pool.total_created () in
  let available = Solver_pool.total_available () in
  Alcotest.(check int) "created solvers" 1 created;
  Alcotest.(check int) "available solvers" 1 available

(* ============================================================================
   Test Runner
   ============================================================================ *)

let () =
  Alcotest.run "Run_twice"
    [
      ( "Run_twice",
        [
          Alcotest.test_case "process_with_false_assumption_has_no_outcomes"
            `Quick process_with_false_assumption_has_no_outcomes;
          Alcotest.test_case "process_with_true_assumption_has_one_outcome"
            `Quick process_with_true_assumption_has_one_outcome;
          Alcotest.test_case "branches_are_deterministic" `Quick
            branches_are_deterministic;
          Alcotest.test_case "sequential_runs_reuse_single_solver" `Quick
            sequential_runs_reuse_single_solver;
        ] );
    ]
