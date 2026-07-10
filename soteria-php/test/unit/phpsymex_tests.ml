module Builtins = Soteria_php.Builtins
module Counterexample = Soteria_php.Counterexample
module Error = Soteria_php.Error
module Php_ir = Soteria_php.Php_ir
module Phpsymex = Soteria_php.Phpsymex
module Value = Soteria_php.Value
module Compo_res = Soteria.Soteria_std.Compo_res
module Or_gave_up = Soteria.Symex.Or_gave_up

let run process = Phpsymex.Result.run ~mode:Soteria.Symex.Approx.OX process

let run_with_trace process =
  Phpsymex.Result.run_with_trace ~mode:Soteria.Symex.Approx.OX process

let expect_single_ok label results =
  match results with
  | [ (Compo_res.Ok value, _) ] -> value
  | _ -> Alcotest.failf "%s: expected one successful path" label

let creates_symbolic_scalars () =
  let boolean =
    run (Builtins.call "Soteria\\symbolic_bool" [])
    |> expect_single_ok "symbolic bool"
  in
  Alcotest.(check string) "bool type" "bool" (Value.type_name boolean);
  Alcotest.(check (option bool))
    "symbolic bool payload" None (Value.bool_value boolean);
  let integer =
    run (Builtins.call "Soteria\\symbolic_int" [])
    |> expect_single_ok "symbolic int"
  in
  Alcotest.(check string) "int type" "int" (Value.type_name integer);
  Alcotest.(check (option int64))
    "symbolic int payload" None (Value.int_value integer);
  let float =
    run (Builtins.call "Soteria\\symbolic_float" [])
    |> expect_single_ok "symbolic float"
  in
  Alcotest.(check string) "float type" "float" (Value.type_name float);
  Alcotest.(check bool)
    "symbolic float payload" true
    (Option.is_none (Value.float_value float))

let symbolic_assertion_branches () =
  let process =
    let open Phpsymex.Syntax in
    let** condition = Builtins.call "Soteria\\symbolic_bool" [] in
    Builtins.call "Soteria\\assert" [ condition ]
  in
  match run process with
  | [
   (Compo_res.Error (Or_gave_up.E (Error.Failed_assertion, [])), failure_path);
   (Compo_res.Ok Value.Null, success_path);
  ] ->
      Alcotest.(check int) "failure path condition" 1 (List.length failure_path);
      Alcotest.(check int) "success path condition" 1 (List.length success_path)
  | _ ->
      Alcotest.fail
        "a symbolic assertion should produce one failing and one successful \
         path"

let assume_restricts_assertion () =
  let process =
    let open Phpsymex.Syntax in
    let** condition = Builtins.call "Soteria\\symbolic_bool" [] in
    let** _ = Builtins.call "Soteria\\assume" [ condition ] in
    Builtins.call "Soteria\\assert" [ condition ]
  in
  let result = run process |> expect_single_ok "assume followed by assert" in
  Alcotest.(check string) "assert return type" "null" (Value.type_name result)

let records_and_concretizes_symbolic_inputs () =
  let process =
    let open Phpsymex.Syntax in
    let** boolean = Builtins.call "Soteria\\symbolic_bool" [] in
    let** integer = Builtins.call "Soteria\\symbolic_int" [] in
    let** float = Builtins.call "Soteria\\symbolic_float" [] in
    let boolean =
      match boolean with Value.Bool value -> value | _ -> assert false
    in
    let integer =
      match integer with Value.Int value -> value | _ -> assert false
    in
    let float =
      match float with Value.Float value -> value | _ -> assert false
    in
    let expected_integer =
      match Value.int 42L with Value.Int value -> value | _ -> assert false
    in
    let expected_float =
      match Value.float 1.5 with
      | Value.Float value -> value
      | _ -> assert false
    in
    let* () =
      Phpsymex.assume
        [
          boolean;
          Value.Typed.sem_eq integer expected_integer;
          Value.Typed.Float.eq float expected_float;
        ]
    in
    Builtins.call "Soteria\\assert" [ Value.bool false ]
  in
  match run_with_trace process with
  | [ (Compo_res.Error (Or_gave_up.E (_, trace)), path_condition) ] -> (
      match
        Counterexample.bindings ~inputs:trace.Error.Trace.symbolic_inputs_rev
          ~path_condition
      with
      | Some
          [
            { name = "input0"; value = Bool true };
            { name = "input1"; value = Int 42L };
            { name = "input2"; value = Float value };
          ] ->
          Alcotest.(check (float 0.0)) "float model" 1.5 value
      | _ -> Alcotest.fail "counterexample did not contain the expected model")
  | _ -> Alcotest.fail "expected one failing constrained path"

let marks_expected_failures_in_the_trace () =
  match run_with_trace (Builtins.call "Soteria\\expect_fail" []) with
  | [ (Compo_res.Ok (Value.Null, trace), _) ] ->
      Alcotest.(check bool)
        "expect failure marker" true trace.Error.Trace.expect_failure
  | _ -> Alcotest.fail "expect_fail should return null and mark the entry point"

let position line column offset : Php_ir.position = { line; column; offset }

let location file start_line start_column start_offset end_line end_column
    end_offset : Php_ir.location =
  {
    file;
    start = position start_line start_column start_offset;
    end_ = position end_line end_column end_offset;
  }

let attaches_source_and_call_trace () =
  let call_location = location "trace.php" 2 1 6 2 12 17 in
  let assertion_location = location "trace.php" 4 3 25 4 19 41 in
  let process =
    Builtins.call "Soteria\\assert" [ Value.bool false ]
    |> Phpsymex.with_location ~location:assertion_location
    |> Phpsymex.with_call ~location:call_location ~message:"Calling test()"
  in
  match run process with
  | [
   ( Compo_res.Error
       (Or_gave_up.E
          ( Error.Failed_assertion,
            [
              { Soteria.Terminal.Call_trace.loc = actual_call; msg = call_msg };
              { loc = actual_assertion; msg = assertion_msg };
            ] )),
     _ );
  ] ->
      Alcotest.(check string) "call message" "Calling test()" call_msg;
      Alcotest.(check string)
        "assertion message" "Triggering operation" assertion_msg;
      Alcotest.(check string)
        "call location" "trace.php:2:1"
        (Format.asprintf "%a" Error.pp_location actual_call);
      Alcotest.(check string)
        "assertion location" "trace.php:4:3"
        (Format.asprintf "%a" Error.pp_location actual_assertion)
  | _ -> Alcotest.fail "assertion error did not retain its source trace"

let validates_builtin_calls () =
  (match run (Builtins.call "\\SOTERIA\\ASSERT" [ Value.int 1L ]) with
  | [
   ( Compo_res.Error
       (Or_gave_up.E
          ( Error.Invalid_argument_type
              {
                function_name = "Soteria\\assert()";
                position = 1;
                expected = "bool";
                actual = `Integer;
              },
            [] )),
     _ );
  ] ->
      ()
  | _ -> Alcotest.fail "assert should reject a non-boolean argument");
  (match run (Builtins.call "Soteria\\symbolic_int" [ Value.null ]) with
  | [
   ( Compo_res.Error
       (Or_gave_up.E
          ( Error.Invalid_argument_count
              {
                function_name = "Soteria\\symbolic_int()";
                expected = 0;
                actual = 1;
              },
            [] )),
     _ );
  ] ->
      ()
  | _ -> Alcotest.fail "symbolic_int should reject arguments");
  match run (Builtins.call "Soteria\\missing" []) with
  | [
   ( Compo_res.Error (Or_gave_up.Gave_up "Unsupported: builtin Soteria\\missing"),
     _ );
  ] ->
      ()
  | _ -> Alcotest.fail "an unknown builtin should explicitly give up"

let () =
  Alcotest.run "PHP symbolic runtime"
    [
      ( "builtins",
        [
          Alcotest.test_case "symbolic scalar inputs" `Quick
            creates_symbolic_scalars;
          Alcotest.test_case "symbolic assertion" `Quick
            symbolic_assertion_branches;
          Alcotest.test_case "assumption" `Quick assume_restricts_assertion;
          Alcotest.test_case "counterexample models" `Quick
            records_and_concretizes_symbolic_inputs;
          Alcotest.test_case "expected failure marker" `Quick
            marks_expected_failures_in_the_trace;
          Alcotest.test_case "argument validation" `Quick
            validates_builtin_calls;
        ] );
      ( "diagnostics",
        [
          Alcotest.test_case "source and call trace" `Quick
            attaches_source_and_call_trace;
        ] );
    ]
