open Soteria.Soteria_std
module Typed = Soteria.Tiny_values.Typed
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)

module S_int = struct
  include Typed

  type t = Typed.T.sint Typed.t
  type syn = Typed.Expr.t

  let simplify = Symex.simplify
  let fresh () = Symex.nondet Typed.t_int
  let pp = Typed.ppa
  let show x = (Fmt.to_to_string pp) x
  let pp_syn = Symex.Value.Expr.pp
  let show_syn x = (Fmt.to_to_string pp_syn) x
  let learn_eq (s : syn) (t : t) = Symex.Consumer.learn_eq s t
  let to_syn (x : t) = Symex.Value.Expr.of_value x
  let exprs_syn (x : syn) = [ x ]
  let subst = Symex.Value.Expr.subst
end

module Excl_int = Soteria.Sym_states.Excl.Make (Symex) (S_int)
module Heap = Soteria.Sym_states.Pmap.Make (Symex) (S_int) (Excl_int)
module Logic = Soteria.Logic.Make (Symex)
module Asrt = Logic.Asrt
module Execute = Asrt.Execute (Heap)

let x, y, z, t =
  let open Soteria.Symex.Var in
  let mk_int id = Svalue.mk_var (of_int id) Svalue.TInt in
  (mk_int 42, mk_int 67, mk_int 1337, mk_int 1996)

let ( @-> ) (x : Svalue.t) (y : Svalue.t) : Heap.syn Asrt.t =
  [ Asrt.Spatial (x, y) ]

let p (x : Svalue.t) : 'a Asrt.t = [ Asrt.Pure x ]
let ( * ) x y = x @ y
let zero = Svalue.int 0
let one = Svalue.int 1

let asrt_success =
  let open Svalue.Infix in
  (x @-> y) * (y @-> z) * p (y ==@ zero) * p (z ==@ one)

let expected_subst = {|V|1337| -> V|1|
V|67| -> V|2|
V|42| -> V|3||}

let asrt_lfail =
  let open Svalue.Infix in
  (x @-> y) * (y @-> z) * p (y ==@ zero) * p (z ==@ zero)

let expected_lfail_reason = "(0 == V|1|)"
let asrt_missing = (x @-> y) * (y @-> z) * (y @-> t)

let first_and_heap () =
  let open Heap.SM.Syntax in
  let* c = Heap.SM.nondet Typed.t_int in
  let** b = Heap.alloc ~new_codom:c in
  let** a = Heap.alloc ~new_codom:b in
  let+ () = Heap.SM.assume Typed.Infix.[ b ==@ Typed.zero; c ==@ Typed.one ] in
  Compo_res.Ok a

let consumer asrt a heap =
  let open Symex.Consumer in
  let open Syntax in
  let* () = learn_eq x a in
  Execute.consume asrt heap

let run_asrt asrt =
  let open Symex.Syntax in
  let* a_res, heap = first_and_heap () None in
  let** a = Symex.return a_res in
  let++ heap, subst =
    Symex.Consumer.run ~subst:Typed.Expr.Subst.empty (consumer asrt a heap)
  in
  let heap_is_empty = Option.is_none heap in
  let str = Fmt.str "%a" Typed.Expr.Subst.pp subst in
  (heap_is_empty, str)

(* ============================================================================
   Tests
   ============================================================================ *)

let test_consume_succeeds () =
  let open Compo_res in
  let res = Symex.run ~mode:OX (run_asrt asrt_success) in
  let res, _ =
    match res with
    | [ one_result ] -> one_result
    | _ -> Alcotest.fail "Expected exactly one result"
  in
  match res with
  | Ok (heap_is_empty, subst_str) ->
      Alcotest.(check bool) "heap is empty after consumption" true heap_is_empty;
      Alcotest.(check string)
        "substitution matches expected" expected_subst subst_str
  | Error _ -> Alcotest.fail "Consumption should be successful!"
  | Missing _ -> Alcotest.fail "This test shouldn't have a missing error"

let test_consume_lfail () =
  let open Compo_res in
  let res = Symex.run ~mode:OX (run_asrt asrt_lfail) in
  let res, _ =
    match res with
    | [ one_result ] -> one_result
    | _ -> Alcotest.fail "Expected exactly one result"
  in
  match res with
  | Ok _ -> Alcotest.fail "Consumption should fail!"
  | Error (`Lfail v) ->
      let lfail_reason = Fmt.str "%a" Typed.ppa v in
      Alcotest.(check string)
        "lfail reason matches expected" expected_lfail_reason lfail_reason
  | Error _ -> Alcotest.fail "Consumption should fail with an Lfail error!"
  | Missing _ -> Alcotest.fail "This test shouldn't have a missing error"

let test_consume_missing () =
  let open Compo_res in
  let res = Symex.run ~mode:OX (run_asrt asrt_missing) in
  let res, _ =
    match res with
    | [ one_result ] -> one_result
    | _ -> Alcotest.fail "Expected exactly one result"
  in
  match res with
  | Ok _ -> Alcotest.fail "Consumption should fail!"
  | Error _ -> Alcotest.fail "Consumption should hit a miss"
  | Missing _ -> () (* Expected outcome *)

(* ============================================================================
   Test Runner
   ============================================================================ *)

let () =
  Alcotest.run "Consume"
    [
      ( "Consume",
        [
          Alcotest.test_case
            "consume succeeds and produces expected substitution" `Quick
            test_consume_succeeds;
          Alcotest.test_case "consume fails with Lfail error" `Quick
            test_consume_lfail;
          Alcotest.test_case "consume fails with Missing error" `Quick
            test_consume_missing;
        ] );
    ]
