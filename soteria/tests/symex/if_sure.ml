open Soteria.Symex.Make (Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values.Typed
open Infix

(* ============================================================================
   Test Processes
   ============================================================================ *)

let if_true = if%sure v_true then return 42 else return (-1)
let if_false = if%sure v_false then return (-1) else return 42

let if_maybe =
  let* b = nondet t_bool in
  if%sure b then return (-1) else return 42

let if_guaranteed =
  let* x = nondet t_int in
  let* () = assume [ x >@ one ] in
  if%sure x >@ zero then return 42 else return (-1)

let if_sure_runs_then_with_consistent_pc =
  let* x = nondet t_int in
  let* y = nondet t_int in
  let* () = assume [ (x *@ x) +@ (y *@ y) ==@ zero ] in
  if%sure x ==@ zero then return 42 else return (-1)

let if_sure_else_preserves_both_arms =
  let* x = nondet t_int in
  if%sure x ==@ zero then return 0
  else if%sat x ==@ zero then return 1
  else return 2

(* ============================================================================
   Helper
   ============================================================================ *)

let get_results process = List.map fst (run ~mode:OX process)

(* ============================================================================
   Tests
   ============================================================================ *)

let if_true_produces_42 () =
  let results = get_results if_true in
  Alcotest.(check (list int)) "result" [ 42 ] results

let if_false_produces_42 () =
  let results = get_results if_false in
  Alcotest.(check (list int)) "result" [ 42 ] results

let if_maybe_produces_42 () =
  let results = get_results if_maybe in
  Alcotest.(check (list int)) "result" [ 42 ] results

let if_guaranteed_produces_42 () =
  let results = get_results if_guaranteed in
  Alcotest.(check (list int)) "result" [ 42 ] results

let if_sure_runs_then_with_consistent_pc_test () =
  let results = get_results if_sure_runs_then_with_consistent_pc in
  Alcotest.(check (list int)) "result" [ 42 ] results

let if_sure_else_preserves_both_arms_test () =
  let results =
    get_results if_sure_else_preserves_both_arms |> List.sort Stdlib.compare
  in
  Alcotest.(check (list int)) "result" [ 1; 2 ] results

(* ============================================================================
   Test Runner
   ============================================================================ *)

let () =
  Alcotest.run "If_sure"
    [
      ( "If_sure",
        [
          Alcotest.test_case "if_true_produces_42" `Quick if_true_produces_42;
          Alcotest.test_case "if_false_produces_42" `Quick if_false_produces_42;
          Alcotest.test_case "if_maybe_produces_42" `Quick if_maybe_produces_42;
          Alcotest.test_case "if_guaranteed_produces_42" `Quick
            if_guaranteed_produces_42;
          Alcotest.test_case "if_sure_runs_then_with_consistent_pc" `Quick
            if_sure_runs_then_with_consistent_pc_test;
          Alcotest.test_case "if_sure_else_preserves_both_arms" `Quick
            if_sure_else_preserves_both_arms_test;
        ] );
    ]
