open Soteria.Symex.Make (Soteria.Symex.Meta.Dummy) (Tiny_solver.Z3_solver)
open Syntax
open Soteria.Tiny_values

(* ============================================================================
   Test Process
   ============================================================================ *)

let complex_process () =
  let open Typed.Infix in
  let* x = nondet Typed.t_int in
  let* y = nondet Typed.t_int in
  if%sat x >@ y then
    if%sat x >@ Typed.zero then Result.ok (x +@ y) else Result.ok (x -@ y)
  else if%sat x ==@ y then give_up ~loc:() "x == y"
  else
    let** () = assert_or_error (Typed.not (x ==@ y)) "x != y" in
    Result.error "okkk"

(* ============================================================================
   Helper Functions
   ============================================================================ *)

let count_oks results =
  List.filter (fun (res, _) -> Soteria.Symex.Compo_res.is_ok res) results
  |> List.length

let count_errors results =
  List.filter (fun (res, _) -> Soteria.Symex.Compo_res.is_error res) results
  |> List.length

let get_gave_up_reasons results =
  List.filter_map
    (fun (res, _) ->
      match res with
      | Soteria.Symex.Compo_res.Error (Soteria.Symex.Or_gave_up.Gave_up reason)
        ->
          Some reason
      | _ -> None)
    results

(* ============================================================================
   UX Mode Tests
   ============================================================================ *)

let ux_mode_returns_three_branches () =
  let results = run ~mode:UX (complex_process ()) in
  Alcotest.(check int) "number of branches" 3 (List.length results);
  Alcotest.(check int) "ok branches" 2 (count_oks results);
  Alcotest.(check int) "error branches" 1 (count_errors results)

let ux_mode_result_returns_three_branches () =
  let results = Result.run ~mode:UX (complex_process ()) in
  Alcotest.(check int) "number of branches" 3 (List.length results);
  Alcotest.(check int) "ok branches" 2 (count_oks results);
  Alcotest.(check int) "error branches" 1 (count_errors results)

(* ============================================================================
   OX Mode Tests
   ============================================================================ *)

let ox_mode_raises_gave_up () =
  let raised = ref false in
  let reason_msg = ref "" in
  (try
     let _ = run ~mode:OX (complex_process ()) in
     ()
   with Soteria.Symex.Gave_up reason ->
     raised := true;
     reason_msg := reason);
  Alcotest.(check bool) "raised Gave_up" true !raised;
  Alcotest.(check string) "gave up reason" "x == y" !reason_msg

let ox_mode_result_returns_four_branches () =
  let results = Result.run ~mode:OX (complex_process ()) in
  Alcotest.(check int) "number of branches" 4 (List.length results);
  Alcotest.(check int) "ok branches" 2 (count_oks results);
  Alcotest.(check int) "error branches" 2 (count_errors results);
  let gave_up_reasons = get_gave_up_reasons results in
  Alcotest.(check int) "gave up branches" 1 (List.length gave_up_reasons);
  Alcotest.(check string) "gave up reason" "x == y" (List.hd gave_up_reasons)

(* ============================================================================
   Test Runner
   ============================================================================ *)

let () =
  Alcotest.run "Deep_give_up"
    [
      ( "Deep_give_up",
        [
          Alcotest.test_case "ux_mode_returns_three_branches" `Quick
            ux_mode_returns_three_branches;
          Alcotest.test_case "ux_mode_result_returns_three_branches" `Quick
            ux_mode_result_returns_three_branches;
          Alcotest.test_case "ox_mode_raises_gave_up" `Quick
            ox_mode_raises_gave_up;
          Alcotest.test_case "ox_mode_result_returns_four_branches" `Quick
            ox_mode_result_returns_four_branches;
        ] );
    ]
