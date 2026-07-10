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

let program statements : Php_ir.t =
  {
    target_php_version = Php_ir.target_php_version;
    source_file = location.file;
    statements;
  }

let run ?fuel statements =
  Interp.run (program statements)
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
        ] );
    ]
