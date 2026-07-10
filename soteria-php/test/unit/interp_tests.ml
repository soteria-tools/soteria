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

let variable_lvalue name : Php_ir.lvalue =
  { desc = Variable_lvalue name; location }

let assign name value = expression (Assign (variable_lvalue name, value))
let assign_lvalue target value = expression (Assign (target, value))

let assign_reference target source =
  expression (Assign_reference (target, source))

let array_element ?key array : Php_ir.lvalue =
  { desc = Array_element_lvalue (array, key); location }

let array_get target = expression (Array_get target)
let array_item ?key value : Php_ir.array_item = { key; value; location }
let array items = expression (Array items)
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

let preserves_array_order_and_append_keys () =
  let statements =
    [
      expression_statement
        (assign "array"
           (array
              [
                array_item ~key:(literal (Int 2L)) (literal (Int 10L));
                array_item (literal (Int 20L));
                array_item ~key:(literal (Int 1L)) (literal (Int 30L));
                array_item (literal (Int 40L));
                array_item ~key:(literal (Int 2L)) (literal (Int 50L));
              ]));
    ]
  in
  let state = run statements |> expect_single_ok "ordered array" in
  let bindings =
    Option.bind (State.find_variable "array" state) Value.array_value
    |> Option.get
    |> Value.array_bindings
  in
  let keys =
    List.map
      (function
        | Value.Integer_key key, _ -> Int64.to_string key
        | String_key key, _ -> key)
      bindings
  in
  let values =
    List.map
      (function
        | _, Value.Inline value -> Option.get (Value.int_value value)
        | _, Reference _ -> Alcotest.fail "unexpected reference")
      bindings
  in
  Alcotest.(check (list string)) "insertion order" [ "2"; "3"; "1"; "4" ] keys;
  Alcotest.(check (list int64))
    "overwritten values" [ 50L; 20L; 30L; 40L ] values

let writes_nested_arrays_and_preserves_copies () =
  let original_item =
    array_element ~key:(literal (String "item")) (variable_lvalue "original")
  in
  let copy_item =
    array_element ~key:(literal (String "item")) (variable_lvalue "copy")
  in
  let statements =
    [
      expression_statement (assign "original" (array []));
      expression_statement
        (assign_lvalue (array_element original_item) (literal (Int 7L)));
      expression_statement (assign "copy" (variable "original"));
      expression_statement
        (assign_lvalue
           (array_element ~key:(literal (Int 0L)) copy_item)
           (literal (Int 8L)));
    ]
  in
  let state = run statements |> expect_single_ok "nested array writes" in
  let nested_value name =
    Option.bind (State.find_variable name state) Value.array_value
    |> fun array ->
    Option.bind array (fun array ->
        State.find_array_value (Value.String_key "item") array state)
    |> fun item ->
    Option.bind item Value.array_value |> fun item ->
    Option.bind item (fun array ->
        State.find_array_value (Value.Integer_key 0L) array state)
    |> fun value -> Option.bind value Value.int_value
  in
  Alcotest.(check (option int64))
    "original remains unchanged" (Some 7L) (nested_value "original");
  Alcotest.(check (option int64))
    "copy is updated" (Some 8L) (nested_value "copy")

let isolates_array_copies_across_symbolic_branches () =
  let copy_zero =
    array_element ~key:(literal (Int 0L)) (variable_lvalue "copy")
  in
  let statements =
    [
      expression_statement
        (assign "original" (array [ array_item (literal (Int 1L)) ]));
      expression_statement (assign "copy" (variable "original"));
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.If
        ( variable "condition",
          [ expression_statement (assign_lvalue copy_zero (literal (Int 2L))) ],
          [ expression_statement (assign_lvalue copy_zero (literal (Int 3L))) ],
          location );
    ]
  in
  let values =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          let element name =
            Option.bind (State.find_variable name state) Value.array_value
            |> fun array ->
            Option.bind array (fun array ->
                State.find_array_value (Value.Integer_key 0L) array state)
            |> fun value -> Option.bind value Value.int_value
          in
          Option.bind (element "original") (fun original ->
              Option.map (fun copy -> (original, copy)) (element "copy"))
      | _ -> None)
    |> List.sort Stdlib.compare
  in
  Alcotest.(check (list (pair int64 int64)))
    "branch-local array copies"
    [ (1L, 2L); (1L, 3L) ]
    values

let evaluates_array_keys_before_assignment_values () =
  let key =
    function_ "key" []
      [
        Php_ir.Echo ([ literal (String "k") ], location);
        Php_ir.Return (Some (literal (Int 0L)), location);
      ]
  in
  let value =
    function_ "value" []
      [
        Php_ir.Echo ([ literal (String "v") ], location);
        Php_ir.Return (Some (literal (Int 1L)), location);
      ]
  in
  let target = array_element ~key:(call "key" []) (variable_lvalue "array") in
  let statements =
    [
      expression_statement (assign "array" (array []));
      expression_statement (assign_lvalue target (call "value" []));
    ]
  in
  let state =
    run ~functions:[ key; value ] statements
    |> expect_single_ok "array assignment evaluation order"
  in
  Alcotest.(check string) "key before value" "kv" (State.output state)

let reads_existing_symbolic_array_keys () =
  let index = variable "index" in
  let in_bounds =
    binary
      (binary index Greater_than_or_equal (literal (Int 0L)))
      Boolean_and
      (binary index Less_than_or_equal (literal (Int 1L)))
  in
  let target = array_element ~key:index (variable_lvalue "array") in
  let statements =
    [
      expression_statement
        (assign "array"
           (array
              [ array_item (literal (Int 10L)); array_item (literal (Int 20L)) ]));
      expression_statement (assign "index" (call "Soteria\\symbolic_int" []));
      expression_statement (call "Soteria\\assume" [ in_bounds ]);
      expression_statement (assign "result" (array_get target));
    ]
  in
  let values =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind (State.find_variable "result" state) Value.int_value
      | _ -> None)
    |> List.sort Int64.compare
  in
  Alcotest.(check (list int64)) "symbolic offset values" [ 10L; 20L ] values

let reports_array_append_overflow () =
  let statements =
    [
      expression_statement
        (assign "array"
           (array
              [
                array_item ~key:(literal (Int Int64.max_int)) (literal (Int 1L));
              ]));
      expression_statement
        (assign_lvalue
           (array_element (variable_lvalue "array"))
           (literal (Int 2L)));
    ]
  in
  match run statements with
  | [ (Compo_res.Error (Or_gave_up.E (Error.Array_append_overflow, _)), _) ] ->
      ()
  | _ -> Alcotest.fail "append after PHP_INT_MAX did not produce an error"

let reserves_append_keys_before_values () =
  let append = array_element (variable_lvalue "array") in
  let statements =
    [
      expression_statement (assign "array" (array []));
      expression_statement
        (assign_lvalue append (assign_lvalue append (literal (Int 1L))));
    ]
  in
  let state = run statements |> expect_single_ok "nested append assignment" in
  let bindings =
    Option.bind (State.find_variable "array" state) Value.array_value
    |> Option.get
    |> Value.array_bindings
  in
  let keys = List.map fst bindings in
  let values =
    List.filter_map
      (function _, Value.Inline value -> Value.int_value value | _ -> None)
      bindings
  in
  Alcotest.(check bool)
    "reserved key order" true
    (keys = [ Value.Integer_key 0L; Value.Integer_key 1L ]);
  Alcotest.(check (list int64)) "nested append values" [ 1L; 1L ] values

let creates_and_rebinds_variable_references () =
  let statements =
    [
      expression_statement (assign "value" (literal (Int 1L)));
      expression_statement
        (assign_reference (variable_lvalue "alias") (variable_lvalue "value"));
      expression_statement (assign "alias" (literal (Int 2L)));
      expression_statement (assign "other" (literal (Int 3L)));
      expression_statement
        (assign_reference (variable_lvalue "alias") (variable_lvalue "other"));
      expression_statement (assign "alias" (literal (Int 4L)));
    ]
  in
  let state = run statements |> expect_single_ok "variable references" in
  let integer name =
    Option.bind (State.find_variable name state) Value.int_value
  in
  Alcotest.(check (option int64)) "old binding" (Some 2L) (integer "value");
  Alcotest.(check (option int64)) "rebound alias" (Some 4L) (integer "alias");
  Alcotest.(check (option int64)) "new binding" (Some 4L) (integer "other")

let unsets_bindings_without_destroying_aliased_cells () =
  let element =
    array_element ~key:(literal (Int 0L)) (variable_lvalue "values")
  in
  let statements =
    [
      expression_statement (assign "value" (literal (Int 1L)));
      expression_statement
        (assign_reference (variable_lvalue "alias") (variable_lvalue "value"));
      Php_ir.Unset ([ variable_lvalue "alias" ], location);
      expression_statement (assign "alias" (literal (Int 2L)));
      expression_statement (assign "values" (array []));
      expression_statement (assign_reference element (variable_lvalue "value"));
      expression_statement (assign "copy" (variable "values"));
      Php_ir.Unset ([ element ], location);
      expression_statement (assign "value" (literal (Int 3L)));
    ]
  in
  let state = run statements |> expect_single_ok "unset references" in
  let integer name =
    Option.bind (State.find_variable name state) Value.int_value
  in
  let array_integer name =
    Option.bind (State.find_variable name state) Value.array_value
    |> fun array ->
    Option.bind array (fun array ->
        State.find_array_value (Value.Integer_key 0L) array state)
    |> fun value -> Option.bind value Value.int_value
  in
  Alcotest.(check (option int64))
    "detached variable" (Some 2L) (integer "alias");
  Alcotest.(check (option int64)) "surviving source" (Some 3L) (integer "value");
  Alcotest.(check (option int64))
    "removed array binding" None (array_integer "values");
  Alcotest.(check (option int64))
    "copied reference survives" (Some 3L) (array_integer "copy")

let preserves_reference_timing_across_array_copies () =
  let original_element =
    array_element ~key:(literal (Int 0L)) (variable_lvalue "original")
  in
  let after_element =
    array_element ~key:(literal (Int 0L)) (variable_lvalue "after")
  in
  let statements =
    [
      expression_statement
        (assign "original" (array [ array_item (literal (Int 1L)) ]));
      expression_statement (assign "before" (variable "original"));
      expression_statement
        (assign_reference (variable_lvalue "alias") original_element);
      expression_statement (assign "alias" (literal (Int 2L)));
      expression_statement (assign "after" (variable "original"));
      expression_statement (assign_lvalue after_element (literal (Int 3L)));
    ]
  in
  let state = run statements |> expect_single_ok "array reference copies" in
  let element name =
    Option.bind (State.find_variable name state) Value.array_value
    |> fun array ->
    Option.bind array (fun array ->
        State.find_array_value (Value.Integer_key 0L) array state)
    |> fun value -> Option.bind value Value.int_value
  in
  Alcotest.(check (option int64))
    "copy before promotion" (Some 1L) (element "before");
  Alcotest.(check (option int64))
    "promoted original" (Some 3L) (element "original");
  Alcotest.(check (option int64))
    "copy after promotion" (Some 3L) (element "after");
  Alcotest.(check (option int64))
    "element alias" (Some 3L)
    (Option.bind (State.find_variable "alias" state) Value.int_value)

let isolates_aliased_cells_across_symbolic_branches () =
  let statements =
    [
      expression_statement (assign "value" (literal (Int 1L)));
      expression_statement
        (assign_reference (variable_lvalue "alias") (variable_lvalue "value"));
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.If
        ( variable "condition",
          [ expression_statement (assign "alias" (literal (Int 2L))) ],
          [ expression_statement (assign "alias" (literal (Int 3L))) ],
          location );
    ]
  in
  let values =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind
            (Option.bind (State.find_variable "value" state) Value.int_value)
            (fun value ->
              Option.map
                (fun alias -> (value, alias))
                (Option.bind
                   (State.find_variable "alias" state)
                   Value.int_value))
      | _ -> None)
    |> List.sort Stdlib.compare
  in
  Alcotest.(check (list (pair int64 int64)))
    "branch-local aliases"
    [ (2L, 2L); (3L, 3L) ]
    values

let compares_self_referential_arrays () =
  let element =
    array_element ~key:(literal (Int 0L)) (variable_lvalue "array")
  in
  let statements =
    [
      expression_statement (assign "array" (array []));
      expression_statement (assign_reference element (variable_lvalue "array"));
      expression_statement
        (assign "equal"
           (binary (variable "array") Identical (variable "array")));
    ]
  in
  let state = run statements |> expect_single_ok "recursive reference" in
  Alcotest.(check (option bool))
    "self equality" (Some true)
    (Option.bind (State.find_variable "equal" state) Value.bool_value)

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
          Alcotest.test_case "array order and append keys" `Quick
            preserves_array_order_and_append_keys;
          Alcotest.test_case "nested array writes and copies" `Quick
            writes_nested_arrays_and_preserves_copies;
          Alcotest.test_case "array branch isolation" `Quick
            isolates_array_copies_across_symbolic_branches;
          Alcotest.test_case "array assignment evaluation order" `Quick
            evaluates_array_keys_before_assignment_values;
          Alcotest.test_case "symbolic array keys" `Quick
            reads_existing_symbolic_array_keys;
          Alcotest.test_case "array append overflow" `Quick
            reports_array_append_overflow;
          Alcotest.test_case "array append reservation" `Quick
            reserves_append_keys_before_values;
          Alcotest.test_case "variable reference rebinding" `Quick
            creates_and_rebinds_variable_references;
          Alcotest.test_case "unset reference bindings" `Quick
            unsets_bindings_without_destroying_aliased_cells;
          Alcotest.test_case "array copies with references" `Quick
            preserves_reference_timing_across_array_copies;
          Alcotest.test_case "reference branch isolation" `Quick
            isolates_aliased_cells_across_symbolic_branches;
          Alcotest.test_case "self-referential array equality" `Quick
            compares_self_referential_arrays;
        ] );
    ]
