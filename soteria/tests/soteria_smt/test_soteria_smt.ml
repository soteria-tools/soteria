open Soteria_smt

let sexp = Alcotest.testable pp_sexp ( = )

(* {1 Serializer} *)

let test_serialize () =
  Alcotest.(check string) "atom" "foo" (to_string (atom "foo"));
  Alcotest.(check string) "empty list" "()" (to_string (list []));
  Alcotest.(check string) "flat list" "(a b c)"
    (to_string (list [ atom "a"; atom "b"; atom "c" ]));
  Alcotest.(check string) "nested" "(= (bvadd x y) #x0f)"
    (to_string (eq (bv_add (atom "x") (atom "y")) (bv_k 8 (Z.of_int 15))));
  Alcotest.(check string) "bv binary (non-mult-of-4)" "#b101"
    (to_string (bv_k 3 (Z.of_int 5)));
  Alcotest.(check string) "indexed sort" "(_ BitVec 32)"
    (to_string (t_bits 32));
  Alcotest.(check string) "assert" "(assert (and p q))"
    (to_string (assume (bool_and (atom "p") (atom "q"))))

(* {1 Reader} *)

let parse_one s =
  let tmp = Filename.temp_file "soteria_smt" ".in" in
  let oc = open_out tmp in
  output_string oc s;
  close_out oc;
  let ic = open_in tmp in
  let err = open_in tmp in
  Fun.protect
    ~finally:(fun () ->
      close_in ic;
      close_in err;
      Sys.remove tmp)
    (fun () -> Reader.read_sexp (Reader.create ic err))

let test_reader () =
  Alcotest.check sexp "atom" (atom "success") (parse_one "success\n");
  Alcotest.check sexp "sat" (atom "sat") (parse_one "  sat ");
  Alcotest.check sexp "bv hex" (atom "#xff") (parse_one "#xff");
  Alcotest.check sexp "bv bin" (atom "#b1010") (parse_one "#b1010");
  Alcotest.check sexp "nested list"
    (list [ atom "a"; list [ atom "b"; atom "c" ] ])
    (parse_one "(a (b c))");
  Alcotest.check sexp "empty list" (list []) (parse_one "()");
  Alcotest.check sexp "quoted symbol" (atom "|a b|") (parse_one "|a b|");
  Alcotest.check sexp "string literal" (atom "\"hi\"") (parse_one "\"hi\"");
  Alcotest.check sexp "comment skipped" (atom "x")
    (parse_one "; a comment\n  x");
  Alcotest.check sexp "model-ish"
    (list [ list [ atom "define-fun"; atom "x"; list []; atom "Int"; atom "3" ] ])
    (parse_one "((define-fun x () Int 3))")

let test_reader_interns () =
  (* The hot tokens must be the very same shared values used by check/ack. *)
  Alcotest.(check bool) "success interned" true
    (parse_one "success" == Reader.a_success);
  Alcotest.(check bool) "sat interned" true (parse_one "sat" == Reader.a_sat)

(* {1 ack_command / check} via a fake solver *)

let fake resp =
  {
    command = (fun _ -> resp);
    stop = (fun () -> ());
    force_stop = (fun () -> ());
    config = z3;
  }

let test_ack () =
  ack_command (fake (atom "success")) (atom "anything");
  Alcotest.check_raises "non-success raises"
    (UnexpectedSolverResponse (atom "boom")) (fun () ->
      ack_command (fake (atom "boom")) (atom "x"))

let test_check () =
  Alcotest.(check string) "sat" "Sat" (show_result (check (fake (atom "sat"))));
  Alcotest.(check string) "unsat" "Unsat"
    (show_result (check (fake (atom "unsat"))));
  Alcotest.(check string) "unknown" "Unknown"
    (show_result (check (fake (atom "unknown"))));
  Alcotest.check_raises "garbage raises"
    (UnexpectedSolverResponse (atom "weird")) (fun () ->
      ignore (check (fake (atom "weird"))))

(* {1 Live z3 smoke test (skipped if z3 is absent)} *)

let z3_available () =
  Sys.command "command -v z3 >/dev/null 2>&1" = 0

let test_z3_smoke () =
  if not (z3_available ()) then
    Alcotest.skip ()
  else begin
    let s = new_solver z3 in
    ack_command s (declare "x" (t_bits 8));
    ack_command s (assume (bv_ult (atom "x") (bv_k 8 (Z.of_int 5))));
    Alcotest.(check string) "sat" "Sat" (show_result (check s));
    let m = get_model s in
    Alcotest.(check bool) "model is a list" true
      (match m with List _ -> true | Atom _ -> false);
    ack_command s (assume (bv_ult (bv_k 8 (Z.of_int 5)) (atom "x")));
    Alcotest.(check string) "unsat" "Unsat" (show_result (check s));
    s.stop ()
  end

let () =
  Alcotest.run "soteria_smt"
    [
      ("serialize", [ Alcotest.test_case "builders" `Quick test_serialize ]);
      ( "reader",
        [
          Alcotest.test_case "round-trip" `Quick test_reader;
          Alcotest.test_case "interning" `Quick test_reader_interns;
        ] );
      ( "protocol",
        [
          Alcotest.test_case "ack_command" `Quick test_ack;
          Alcotest.test_case "check" `Quick test_check;
        ] );
      ("z3", [ Alcotest.test_case "smoke" `Slow test_z3_smoke ]);
    ]
