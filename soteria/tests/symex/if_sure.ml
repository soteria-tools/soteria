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
        ] );
    ]
