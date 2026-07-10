module Error = Soteria_php.Error
module Interp = Soteria_php.Interp
module Php_ir = Soteria_php.Php_ir
module Phpsymex = Soteria_php.Phpsymex
module State = Soteria_php.State
module Value = Soteria_php.Value
module Compo_res = Soteria.Soteria_std.Compo_res
module Or_gave_up = Soteria.Symex.Or_gave_up

let position line column offset : Php_ir.position = { line; column; offset }

let location : Php_ir.location =
  { file = "test.php"; start = position 1 1 0; end_ = position 1 2 1 }

let expression desc : Php_ir.expression = { desc; location }
let literal value = expression (Literal value)
let variable name = expression (Variable name)
let assign name value = expression (Assign (name, value))
let call name arguments = expression (Call (name, arguments))
let binary left operator right = expression (Binary (left, operator, right))
let expression_statement expression = Php_ir.Expression (expression, location)
let parameter name : Php_ir.parameter = { name; location }

let function_ name parameters body : Php_ir.function_decl =
  { name; parameters = List.map parameter parameters; body; location }

let program ?(functions = []) statements : Php_ir.t =
  {
    target_php_version = Php_ir.target_php_version;
    source_file = location.file;
    functions;
    statements;
  }

let run ?fuel ?functions statements =
  Interp.run (program ?functions statements)
  |> Phpsymex.Result.run ?fuel ~mode:Soteria.Symex.Approx.OX

let expect_single_ok label = function
  | [ (Compo_res.Ok state, _) ] -> state
  | _ -> Alcotest.failf "%s: expected one successful path" label

let evaluates_assignments_and_division () =
  let statements =
    [
      expression_statement (assign "x" (literal (Int 4L)));
      Php_ir.Echo
        ( [
            binary (variable "x") Divide (literal (Int 2L));
            literal (String ":");
            binary (literal (Int 5L)) Divide (literal (Int 2L));
          ],
          location );
    ]
  in
  let state = run statements |> expect_single_ok "concrete execution" in
  Alcotest.(check string) "output" "2:2.5" (State.output state);
  let x = Option.bind (State.find_variable "x" state) Value.int_value in
  Alcotest.(check (option int64)) "variable" (Some 4L) x

let isolates_symbolic_branches () =
  let statements =
    [
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.If
        ( variable "condition",
          [ expression_statement (assign "value" (literal (Int 1L))) ],
          [ expression_statement (assign "value" (literal (Int 2L))) ],
          location );
    ]
  in
  let values =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind (State.find_variable "value" state) Value.int_value
      | _ -> None)
    |> List.sort Int64.compare
  in
  Alcotest.(check (list int64)) "branch-local values" [ 1L; 2L ] values

let short_circuits_calls () =
  let missing = call "Soteria\\missing" [] in
  let statements =
    [
      expression_statement (binary (literal (Bool false)) Boolean_and missing);
      expression_statement (binary (literal (Bool true)) Boolean_or missing);
    ]
  in
  ignore (run statements |> expect_single_ok "short-circuit evaluation")

let reports_division_by_zero_at_expression () =
  let division = binary (literal (Int 1L)) Divide (literal (Int 0L)) in
  match run [ expression_statement division ] with
  | [
   ( Compo_res.Error
       (Or_gave_up.E
          ( Error.Division_by_zero,
            [ { Soteria.Terminal.Call_trace.loc = actual; _ } ] )),
     _ );
  ] ->
      Alcotest.(check string)
        "location" "test.php:1:1"
        (Format.asprintf "%a" Error.pp_location actual)
  | _ -> Alcotest.fail "division by zero did not produce a located error"

let exhausts_loop_fuel () =
  let open Soteria.Symex.Fuel_gauge in
  let fuel = { steps = Finite 5; branching = Finite 5 } in
  let loop = Php_ir.While (literal (Bool true), [], location) in
  match run ~fuel [ loop ] with
  | [ (Compo_res.Error (Or_gave_up.Gave_up _), _) ] -> ()
  | _ -> Alcotest.fail "an unbounded loop should give up when fuel is exhausted"

let reports_partial_branch_exhaustion () =
  let open Soteria.Symex.Fuel_gauge in
  let fuel = { steps = Infinite; branching = Finite 1 } in
  let statements =
    [
      expression_statement (assign "a" (call "Soteria\\symbolic_bool" []));
      expression_statement (assign "b" (call "Soteria\\symbolic_bool" []));
      Php_ir.If
        ( variable "a",
          [ Php_ir.If (variable "b", [], [], location) ],
          [],
          location );
    ]
  in
  let results = run ~fuel statements in
  let has_success =
    List.exists (function Compo_res.Ok _, _ -> true | _ -> false) results
  in
  let has_give_up =
    List.exists
      (function
        | Compo_res.Error (Or_gave_up.Gave_up _), _ -> true | _ -> false)
      results
  in
  Alcotest.(check bool) "completed branch" true has_success;
  Alcotest.(check bool) "exhausted branch" true has_give_up

let calls_functions_with_local_scopes () =
  let add =
    function_ "Add" [ "left"; "right" ]
      [
        expression_statement (assign "outside" (literal (Int 99L)));
        Php_ir.Echo ([ literal (String "inside:") ], location);
        Php_ir.If
          ( binary (variable "left") Greater_than (literal (Int 0L)),
            [
              Php_ir.Return
                ( Some (binary (variable "left") Add (variable "right")),
                  location );
            ],
            [ Php_ir.Return (None, location) ],
            location );
        expression_statement (call "Soteria\\assert" [ literal (Bool false) ]);
      ]
  in
  let statements =
    [
      expression_statement (assign "outside" (literal (Int 7L)));
      expression_statement
        (assign "result"
           (call "aDd" [ literal (Int 2L); literal (Int 3L); literal (Int 4L) ]));
    ]
  in
  let state =
    run ~functions:[ add ] statements |> expect_single_ok "function call"
  in
  Alcotest.(check string) "function output" "inside:" (State.output state);
  let result =
    Option.bind (State.find_variable "result" state) Value.int_value
  in
  let outside =
    Option.bind (State.find_variable "outside" state) Value.int_value
  in
  Alcotest.(check (option int64)) "return value" (Some 5L) result;
  Alcotest.(check (option int64)) "caller variable" (Some 7L) outside;
  Alcotest.(check bool)
    "parameter does not leak" true
    (Option.is_none (State.find_variable "left" state))

let defaults_to_null_on_fallthrough () =
  let no_return =
    function_ "no_return" []
      [ expression_statement (assign "local" (literal (Int 1L))) ]
  in
  let state =
    run ~functions:[ no_return ]
      [ expression_statement (assign "result" (call "no_return" [])) ]
    |> expect_single_ok "function fallthrough"
  in
  match State.find_variable "result" state with
  | Some Value.Null -> ()
  | _ -> Alcotest.fail "function fallthrough did not return null"

let isolates_symbolic_function_returns () =
  let choose =
    function_ "choose" [ "condition" ]
      [
        Php_ir.If
          ( variable "condition",
            [ Php_ir.Return (Some (literal (Int 1L)), location) ],
            [ Php_ir.Return (Some (literal (Int 2L)), location) ],
            location );
      ]
  in
  let statements =
    [
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      expression_statement
        (assign "result" (call "choose" [ variable "condition" ]));
    ]
  in
  let values =
    run ~functions:[ choose ] statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind (State.find_variable "result" state) Value.int_value
      | _ -> None)
    |> List.sort Int64.compare
  in
  Alcotest.(check (list int64)) "return values" [ 1L; 2L ] values

let reports_missing_function_arguments () =
  let function_ =
    function_ "needs_two" [ "first"; "second" ]
      [ Php_ir.Return (None, location) ]
  in
  match
    run ~functions:[ function_ ]
      [ expression_statement (call "needs_two" [ literal (Int 1L) ]) ]
  with
  | [
   ( Compo_res.Error
       (Or_gave_up.E
          ( Error.Invalid_argument_count
              { function_name = "needs_two()"; expected = 2; actual = 1 },
            _ )),
     _ );
  ] ->
      ()
  | _ -> Alcotest.fail "missing function argument did not produce an error"

let supports_recursive_calls () =
  let count_down =
    function_ "count_down" [ "value" ]
      [
        Php_ir.If
          ( binary (variable "value") Identical (literal (Int 0L)),
            [ Php_ir.Return (Some (literal (Int 0L)), location) ],
            [
              Php_ir.Return
                ( Some
                    (binary
                       (call "count_down"
                          [
                            binary (variable "value") Subtract
                              (literal (Int 1L));
                          ])
                       Add (literal (Int 1L))),
                  location );
            ],
            location );
      ]
  in
  let state =
    run ~functions:[ count_down ]
      [
        expression_statement
          (assign "result" (call "count_down" [ literal (Int 3L) ]));
      ]
    |> expect_single_ok "recursive function"
  in
  let result =
    Option.bind (State.find_variable "result" state) Value.int_value
  in
  Alcotest.(check (option int64)) "recursive result" (Some 3L) result

let () =
  Alcotest.run "PHP interpreter"
    [
      ( "execution",
        [
          Alcotest.test_case "assignments and division" `Quick
            evaluates_assignments_and_division;
          Alcotest.test_case "symbolic branch isolation" `Quick
            isolates_symbolic_branches;
          Alcotest.test_case "short-circuit calls" `Quick short_circuits_calls;
          Alcotest.test_case "division by zero" `Quick
            reports_division_by_zero_at_expression;
          Alcotest.test_case "loop fuel" `Quick exhausts_loop_fuel;
          Alcotest.test_case "branch fuel" `Quick
            reports_partial_branch_exhaustion;
          Alcotest.test_case "function calls and local scopes" `Quick
            calls_functions_with_local_scopes;
          Alcotest.test_case "function fallthrough" `Quick
            defaults_to_null_on_fallthrough;
          Alcotest.test_case "symbolic function returns" `Quick
            isolates_symbolic_function_returns;
          Alcotest.test_case "missing function arguments" `Quick
            reports_missing_function_arguments;
          Alcotest.test_case "recursive functions" `Quick
            supports_recursive_calls;
        ] );
    ]
