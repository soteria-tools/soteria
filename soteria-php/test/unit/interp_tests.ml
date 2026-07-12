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

let object_property object_ name : Php_ir.lvalue =
  { desc = Object_property_lvalue (object_, name); location }

let array_get target = expression (Array_get target)
let property_get target = expression (Property_get target)
let array_item ?key value : Php_ir.array_item = { key; value; location }
let array items = expression (Array items)
let call name arguments = expression (Call (name, arguments))

let method_call object_ name arguments =
  expression (Method_call (object_, name, arguments))

let new_ name arguments = expression (New (name, arguments))
let throw value = expression (Throw value)
let binary left operator right = expression (Binary (left, operator, right))
let unary operator operand = expression (Unary (operator, operand))
let expression_statement expression = Php_ir.Expression (expression, location)
let parameter name : Php_ir.parameter = { name; location }

let function_ name parameters body : Php_ir.function_decl =
  { name; parameters = List.map parameter parameters; body; location }

let property ?default ?(visibility = Php_ir.Public) name : Php_ir.property_decl
    =
  { name; default; modifiers = [ visibility ]; location }

let method_ ?(visibility = Php_ir.Public) name parameters body :
    Php_ir.method_decl =
  {
    name;
    parameters = List.map parameter parameters;
    body;
    modifiers = [ visibility ];
    location;
  }

let class_ ?(methods = []) name properties : Php_ir.class_decl =
  { name; properties; methods; location }

let catch ?variable types body : Php_ir.catch_clause =
  { types; variable; body; location }

let program ?(functions = []) ?(classes = []) statements : Php_ir.t =
  {
    target_php_version = Php_ir.target_php_version;
    source_file = location.file;
    functions;
    classes;
    statements;
  }

let run ?fuel ?function_name ?functions ?classes statements =
  Interp.run ?function_name (program ?functions ?classes statements)
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

let completes_scalar_coercion_and_comparison () =
  let statements =
    [
      expression_statement
        (assign "sum" (binary (literal Null) Add (literal (String "2"))));
      expression_statement
        (assign "product"
           (binary (literal (Bool true)) Multiply (literal (String "2.5"))));
      expression_statement
        (assign "division"
           (binary (literal (String "4")) Divide (literal (Int 2L))));
      expression_statement
        (assign "overflow"
           (binary (literal (Int Int64.max_int)) Add (literal (Int 1L))));
      expression_statement
        (assign "negated_minimum"
           (unary Numeric_negation (literal (Int Int64.min_int))));
      expression_statement
        (assign "numeric_string_equal"
           (binary (literal (String " 12 \n")) Equal (literal (Int 12L))));
      expression_statement
        (assign "ordinary_string_order"
           (binary (literal (Int 0L)) Less_than (literal (String "abc"))));
      expression_statement
        (assign "nan_equal"
           (binary (literal (Float nan)) Equal (literal (Float nan))));
      expression_statement
        (assign "nan_order"
           (binary (literal (Float nan)) Less_than (literal (Int 1L))));
    ]
  in
  let state = run statements |> expect_single_ok "scalar semantics" in
  let find name project =
    Option.bind (State.find_variable name state) project
  in
  Alcotest.(check (option int64))
    "weak integer addition" (Some 2L)
    (find "sum" Value.int_value);
  Alcotest.(check (option (float 0.0)))
    "weak float multiplication" (Some 2.5)
    (find "product" Value.float_value);
  Alcotest.(check (option (float 0.0)))
    "division result type" (Some 2.0)
    (find "division" Value.float_value);
  Alcotest.(check bool)
    "integer overflow promotion" true
    (Option.exists
       (fun value -> Value.kind value = `Float)
       (State.find_variable "overflow" state));
  Alcotest.(check bool)
    "unary overflow promotion" true
    (Option.exists
       (fun value -> Value.kind value = `Float)
       (State.find_variable "negated_minimum" state));
  Alcotest.(check (option bool))
    "numeric string equality" (Some true)
    (find "numeric_string_equal" Value.bool_value);
  Alcotest.(check (option bool))
    "ordinary string ordering" (Some true)
    (find "ordinary_string_order" Value.bool_value);
  Alcotest.(check (option bool))
    "NAN equality" (Some false)
    (find "nan_equal" Value.bool_value);
  Alcotest.(check (option bool))
    "NAN ordering" (Some false)
    (find "nan_order" Value.bool_value)

let branches_on_symbolic_numeric_string_comparison () =
  let statements =
    [
      expression_statement (assign "input" (call "Soteria\\symbolic_int" []));
      Php_ir.If
        ( binary (variable "input") Equal (literal (String "0")),
          [ expression_statement (assign "result" (literal (Int 1L))) ],
          [ expression_statement (assign "result" (literal (Int 2L))) ],
          location );
    ]
  in
  let results =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind (State.find_variable "result" state) Value.int_value
      | _ -> None)
    |> List.sort Int64.compare
  in
  Alcotest.(check (list int64)) "comparison branches" [ 1L; 2L ] results

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
          ( Error.Uncaught_exception
              {
                class_name = "DivisionByZeroError";
                message = "Division by zero";
              },
            [ { Soteria.Terminal.Call_trace.loc = actual; _ } ] )),
     _ );
  ] ->
      Alcotest.(check string)
        "location" "test.php:1:1"
        (Format.asprintf "%a" Error.pp_location actual)
  | _ -> Alcotest.fail "division by zero did not produce a located throwable"

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

let executes_a_selected_function_entry_point () =
  let selected =
    function_ "Selected" []
      [ Php_ir.Echo ([ literal (String "selected") ], location) ]
  in
  let state =
    run ~function_name:"sElEcTeD" ~functions:[ selected ]
      [ expression_statement (call "Soteria\\assert" [ literal (Bool false) ]) ]
    |> expect_single_ok "selected function entry point"
  in
  Alcotest.(check string) "selected output" "selected" (State.output state);
  (match
     Interp.validate_entry_point (program ~functions:[ selected ] []) "missing"
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "a missing entry point should be rejected");
  let parameterized = function_ "parameterized" [ "argument" ] [] in
  match
    Interp.validate_entry_point
      (program ~functions:[ parameterized ] [])
      "parameterized"
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "a parameterized entry point should be rejected"

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
          ( Error.Uncaught_exception
              {
                class_name = "ArgumentCountError";
                message = "needs_two() expects exactly 2 arguments, 1 given";
              },
            _ )),
     _ );
  ] ->
      ()
  | _ -> Alcotest.fail "missing function argument did not produce a throwable"

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
        | String_key key, _ -> key
        | Symbolic_integer_key _, _ -> Alcotest.fail "unexpected symbolic key")
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

let iterates_arrays_by_value_over_a_snapshot () =
  let source = variable_lvalue "source" in
  let source_key key = array_element ~key source in
  let statements =
    [
      expression_statement
        (assign "source"
           (array
              [
                array_item ~key:(literal (Int 2L)) (literal (String "two"));
                array_item ~key:(literal (String "name"))
                  (literal (String "value"));
                array_item ~key:(literal (Int 5L)) (literal (String "five"));
              ]));
      Php_ir.Foreach
        ( variable "source",
          Some (variable_lvalue "key"),
          variable_lvalue "value",
          false,
          [
            Php_ir.Echo
              ( [ variable "key"; literal (String "="); variable "value" ],
                location );
            Php_ir.If
              ( binary (variable "key") Identical (literal (Int 2L)),
                [
                  expression_statement
                    (assign_lvalue
                       (source_key (literal (String "name")))
                       (literal (String "changed")));
                  expression_statement
                    (assign_lvalue (array_element source)
                       (literal (String "new")));
                ],
                [],
                location );
          ],
          location );
    ]
  in
  let state = run statements |> expect_single_ok "foreach snapshot" in
  Alcotest.(check string)
    "insertion order and snapshot values" "2=twoname=value5=five"
    (State.output state);
  Alcotest.(check (option string))
    "loop value remains assigned" (Some "five")
    (Option.bind (State.find_variable "value" state) Value.string_value)

let isolates_foreach_progress_across_symbolic_branches () =
  let statements =
    [
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.Foreach
        ( array [ array_item (literal (Int 1L)); array_item (literal (Int 2L)) ],
          None,
          variable_lvalue "value",
          false,
          [
            Php_ir.If
              ( binary
                  (binary (variable "value") Identical (literal (Int 1L)))
                  Boolean_and (variable "condition"),
                [ Php_ir.Break (1, location) ],
                [],
                location );
            Php_ir.Echo ([ variable "value" ], location);
          ],
          location );
    ]
  in
  let outputs =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ -> Some (State.output state)
      | _ -> None)
    |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "branch-local iterator progress" [ ""; "12" ] outputs

let iterates_arrays_by_reference_and_preserves_lingering_alias () =
  let source = variable_lvalue "source" in
  let statements =
    [
      expression_statement
        (assign "source"
           (array
              [ array_item (literal (Int 1L)); array_item (literal (Int 2L)) ]));
      Php_ir.Foreach
        ( variable "source",
          Some (variable_lvalue "key"),
          variable_lvalue "value",
          true,
          [
            Php_ir.Echo ([ variable "value" ], location);
            Php_ir.If
              ( binary (variable "key") Identical (literal (Int 0L)),
                [
                  expression_statement
                    (assign_lvalue (array_element source) (literal (Int 3L)));
                ],
                [],
                location );
          ],
          location );
      expression_statement (assign "copy" (variable "source"));
      expression_statement (assign "value" (literal (Int 9L)));
    ]
  in
  let state = run statements |> expect_single_ok "foreach references" in
  let array_values name =
    Option.bind (State.find_variable name state) Value.array_value
    |> Option.to_list
    |> List.concat_map Value.array_bindings
    |> List.map (fun (_, entry) ->
        Option.bind (State.value_of_array_entry entry state) Value.int_value
        |> Option.get)
  in
  Alcotest.(check string) "appended entry is visited" "123" (State.output state);
  Alcotest.(check (list int64))
    "lingering alias updates source" [ 1L; 2L; 9L ] (array_values "source");
  Alcotest.(check (list int64))
    "copied references retain aliases" [ 1L; 2L; 9L ] (array_values "copy")

let isolates_foreach_reference_promotions_across_symbolic_branches () =
  let statements =
    [
      expression_statement
        (assign "source"
           (array
              [ array_item (literal (Int 1L)); array_item (literal (Int 1L)) ]));
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.Foreach
        ( variable "source",
          None,
          variable_lvalue "value",
          true,
          [
            Php_ir.If
              ( variable "condition",
                [
                  expression_statement (assign "value" (literal (Int 2L)));
                  Php_ir.Break (1, location);
                ],
                [ expression_statement (assign "value" (literal (Int 3L))) ],
                location );
            Php_ir.Echo ([ variable "value" ], location);
          ],
          location );
    ]
  in
  let outcomes =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind (State.find_variable "source" state) Value.array_value
          |> Option.map (fun array ->
              ( State.output state,
                Value.array_bindings array
                |> List.map (fun (_, entry) ->
                    Option.bind
                      (State.value_of_array_entry entry state)
                      Value.int_value
                    |> Option.get) ))
      | _ -> None)
    |> List.sort Stdlib.compare
  in
  Alcotest.(check (list (pair string (list int64))))
    "branch-local iterator and promoted cells"
    [ ("", [ 2L; 1L ]); ("33", [ 3L; 3L ]) ]
    outcomes

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

let inserts_fresh_symbolic_array_keys_persistently () =
  let target =
    array_element ~key:(variable "index") (variable_lvalue "array")
  in
  let statements =
    [
      expression_statement
        (assign "array" (array [ array_item (literal (Int 1L)) ]));
      expression_statement (assign "index" (call "Soteria\\symbolic_int" []));
      expression_statement (assign_lvalue target (literal (Int 2L)));
      expression_statement (assign "result" (array_get target));
    ]
  in
  let outcomes =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          let length =
            Option.bind (State.find_variable "array" state) Value.array_value
            |> Option.map Value.array_length
          in
          Option.bind length (fun length ->
              Option.bind (State.find_variable "result" state) Value.int_value
              |> Option.map (fun result -> (length, result)))
      | _ -> None)
    |> List.sort Stdlib.compare
  in
  Alcotest.(check (list (pair int int64)))
    "existing and fresh-key paths"
    [ (1, 2L); (2, 2L) ]
    outcomes

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
  | [
   ( Compo_res.Error
       (Or_gave_up.E
          ( Error.Uncaught_exception
              {
                class_name = "Error";
                message =
                  "Cannot add element to the array as the next element is \
                   already occupied";
              },
            _ )),
     _ );
  ] ->
      ()
  | _ -> Alcotest.fail "append after PHP_INT_MAX did not produce a throwable"

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

let shares_object_identity_and_property_cells () =
  let box = class_ "Box" [ property ~default:(literal (Int 1L)) "value" ] in
  let first_value = object_property (variable_lvalue "first") "value" in
  let alias_value = object_property (variable_lvalue "alias") "value" in
  let other_value = object_property (variable_lvalue "other") "value" in
  let statements =
    [
      expression_statement (assign "first" (new_ "Box" []));
      expression_statement (assign "alias" (variable "first"));
      expression_statement (assign "other" (new_ "box" []));
      expression_statement (assign_lvalue alias_value (literal (Int 2L)));
      expression_statement
        (assign_reference (variable_lvalue "reference") first_value);
      expression_statement (assign "reference" (literal (Int 3L)));
      expression_statement
        (assign "same" (binary (variable "first") Identical (variable "alias")));
      expression_statement
        (assign "different"
           (binary (variable "first") Not_identical (variable "other")));
      Php_ir.Unset ([ alias_value ], location);
      expression_statement (assign_lvalue first_value (literal (Int 4L)));
      expression_statement (assign "result" (property_get alias_value));
      expression_statement (assign "untouched" (property_get other_value));
    ]
  in
  let state =
    run ~classes:[ box ] statements |> expect_single_ok "object properties"
  in
  let boolean name =
    Option.bind (State.find_variable name state) Value.bool_value
  in
  let integer name =
    Option.bind (State.find_variable name state) Value.int_value
  in
  Alcotest.(check (option bool)) "shared handle" (Some true) (boolean "same");
  Alcotest.(check (option bool))
    "distinct allocation" (Some true) (boolean "different");
  Alcotest.(check (option int64))
    "property restored through alias" (Some 4L) (integer "result");
  Alcotest.(check (option int64))
    "separate property store" (Some 1L) (integer "untouched");
  Alcotest.(check (option int64))
    "detached property reference" (Some 3L) (integer "reference")

let keeps_declaring_class_in_property_identity () =
  let parent_property =
    State.declared_property ~declaring_class:"Parent" "value"
  in
  let child_property =
    State.declared_property ~declaring_class:"Child" "value"
  in
  let object_id, state =
    State.allocate_object
      ~properties:
        [
          (parent_property, Value.of_literal (Int 1L));
          (child_property, Value.of_literal (Int 2L));
        ]
      "Child" "" State.empty
  in
  let integer property =
    Option.bind
      (State.find_object_property object_id property state)
      Value.int_value
  in
  Alcotest.(check (option int64))
    "parent private property" (Some 1L) (integer parent_property);
  Alcotest.(check (option int64))
    "child private property" (Some 2L) (integer child_property)

let isolates_object_properties_across_symbolic_branches () =
  let box = class_ "Box" [ property ~default:(literal (Int 1L)) "value" ] in
  let value = object_property (variable_lvalue "box") "value" in
  let statements =
    [
      expression_statement (assign "box" (new_ "Box" []));
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.If
        ( variable "condition",
          [ expression_statement (assign_lvalue value (literal (Int 2L))) ],
          [ expression_statement (assign_lvalue value (literal (Int 3L))) ],
          location );
      expression_statement (assign "result" (property_get value));
    ]
  in
  let values =
    run ~classes:[ box ] statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind (State.find_variable "result" state) Value.int_value
      | _ -> None)
    |> List.sort Int64.compare
  in
  Alcotest.(check (list int64)) "branch-local property stores" [ 2L; 3L ] values

let runs_constructors_instance_methods_and_recursive_calls () =
  let this_value = object_property (variable_lvalue "this") "value" in
  let constructor =
    method_ "__construct" [ "value" ]
      [ expression_statement (assign_lvalue this_value (variable "value")) ]
  in
  let set =
    method_ "set" [ "value" ]
      [
        expression_statement (assign_lvalue this_value (variable "value"));
        Php_ir.Return (Some (property_get this_value), location);
      ]
  in
  let recurse =
    method_ "recurse" [ "depth" ]
      [
        Php_ir.If
          ( binary (variable "depth") Identical (literal (Int 0L)),
            [ Php_ir.Return (Some (property_get this_value), location) ],
            [
              Php_ir.Return
                ( Some
                    (method_call (variable "this") "recurse"
                       [ binary (variable "depth") Subtract (literal (Int 1L)) ]),
                  location );
            ],
            location );
      ]
  in
  let box =
    class_
      ~methods:[ constructor; set; recurse ]
      "Box"
      [ property ~default:(literal (Int 1L)) "value" ]
  in
  let statements =
    [
      expression_statement (assign "box" (new_ "Box" [ literal (Int 4L) ]));
      expression_statement (assign "alias" (variable "box"));
      expression_statement
        (assign "set_result"
           (method_call (variable "alias") "SET" [ literal (Int 7L) ]));
      expression_statement
        (assign "recursive_result"
           (method_call (variable "box") "recurse" [ literal (Int 3L) ]));
      expression_statement
        (assign "property_result"
           (property_get (object_property (variable_lvalue "box") "value")));
    ]
  in
  let state =
    run ~classes:[ box ] statements |> expect_single_ok "object methods"
  in
  let integer name =
    Option.bind (State.find_variable name state) Value.int_value
  in
  Alcotest.(check (option int64))
    "method return" (Some 7L) (integer "set_result");
  Alcotest.(check (option int64))
    "recursive return" (Some 7L)
    (integer "recursive_result");
  Alcotest.(check (option int64))
    "aliased mutation" (Some 7L)
    (integer "property_result")

let propagates_constructor_throws_and_isolates_method_branches () =
  let throwing =
    class_
      ~methods:
        [
          method_ "__construct" []
            [
              expression_statement
                (throw (new_ "RuntimeException" [ literal (String "boom") ]));
            ];
        ]
      "Throwing" []
  in
  let this_value = object_property (variable_lvalue "this") "value" in
  let box =
    class_
      ~methods:
        [
          method_ "set" [ "value" ]
            [
              expression_statement (assign_lvalue this_value (variable "value"));
            ];
        ]
      "Box"
      [ property ~default:(literal (Int 1L)) "value" ]
  in
  let statements =
    [
      Php_ir.Try
        ( [ expression_statement (new_ "Throwing" []) ],
          [
            catch [ "RuntimeException" ]
              [ expression_statement (assign "caught" (literal (Bool true))) ];
          ],
          None,
          location );
      expression_statement (assign "box" (new_ "Box" []));
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.If
        ( variable "condition",
          [
            expression_statement
              (method_call (variable "box") "set" [ literal (Int 2L) ]);
          ],
          [
            expression_statement
              (method_call (variable "box") "set" [ literal (Int 3L) ]);
          ],
          location );
      expression_statement
        (assign "result"
           (property_get (object_property (variable_lvalue "box") "value")));
    ]
  in
  let values =
    run ~classes:[ throwing; box ] statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Option.bind
            (Option.bind (State.find_variable "caught" state) Value.bool_value)
            (fun caught ->
              if caught then
                Option.bind (State.find_variable "result" state) Value.int_value
              else None)
      | _ -> None)
    |> List.sort Int64.compare
  in
  Alcotest.(check (list int64)) "branch-local method mutation" [ 2L; 3L ] values

let propagates_nested_loop_control_through_finally () =
  let increment =
    expression_statement
      (assign "count" (binary (variable "count") Add (literal (Int 1L))))
  in
  let inner =
    Php_ir.While
      ( literal (Bool true),
        [
          Php_ir.Try
            ( [
                increment;
                Php_ir.If
                  ( binary (variable "count") Identical (literal (Int 1L)),
                    [ Php_ir.Continue (2, location) ],
                    [ Php_ir.Break (2, location) ],
                    location );
              ],
              [],
              Some [ Php_ir.Echo ([ literal (String "f") ], location) ],
              location );
        ],
        location )
  in
  let statements =
    [
      expression_statement (assign "count" (literal (Int 0L)));
      Php_ir.While
        ( binary (variable "count") Less_than (literal (Int 3L)),
          [ inner ],
          location );
      Php_ir.Echo ([ literal (String "done") ], location);
    ]
  in
  let state = run statements |> expect_single_ok "structured loop control" in
  Alcotest.(check string) "finally output" "ffdone" (State.output state);
  Alcotest.(check (option int64))
    "loop count" (Some 2L)
    (Option.bind (State.find_variable "count" state) Value.int_value)

let models_undefined_reads_as_warnings_and_null () =
  let missing_key =
    array_element ~key:(literal (String "missing")) (variable_lvalue "array")
  in
  let property_target = object_property (variable_lvalue "object") "property" in
  let missing_property = object_property (variable_lvalue "object") "missing" in
  let statements =
    [
      expression_statement (assign "variable" (variable "undefined"));
      expression_statement (assign "array" (array []));
      expression_statement (assign "offset" (array_get missing_key));
      expression_statement (assign "object" (new_ "Container" []));
      Php_ir.Unset ([ property_target ], location);
      expression_statement (assign "property" (property_get property_target));
      expression_statement
        (assign "missing_property" (property_get missing_property));
    ]
  in
  let state =
    run ~classes:[ class_ "Container" [ property "property" ] ] statements
    |> expect_single_ok "undefined reads"
  in
  List.iter
    (fun name ->
      match State.find_variable name state with
      | Some Value.Null -> ()
      | _ -> Alcotest.failf "$%s did not receive null" name)
    [ "variable"; "offset"; "property"; "missing_property" ];
  let messages =
    State.runtime_events state
    |> List.map (fun event -> event.Error.Runtime_event.message)
  in
  Alcotest.(check (list string))
    "warning messages"
    [
      "Undefined variable $undefined";
      "Undefined array key \"missing\"";
      "Undefined property: Container::$property";
      "Undefined property: Container::$missing";
    ]
    messages;
  let warn =
    function_ "warn" [] [ expression_statement (variable "missing") ]
  in
  let state =
    run ~functions:[ warn ] [ expression_statement (call "warn" []) ]
    |> expect_single_ok "runtime event call trace"
  in
  match State.runtime_events state with
  | [
   {
     Error.Runtime_event.trace = { location = Some _; call_trace = [ _ ]; _ };
     _;
   };
  ] ->
      ()
  | _ -> Alcotest.fail "runtime event did not retain its source and call trace"

let records_leading_numeric_warnings_persistently () =
  let statements =
    [
      expression_statement
        (assign "condition" (call "Soteria\\symbolic_bool" []));
      Php_ir.If
        ( variable "condition",
          [
            expression_statement
              (assign "value"
                 (binary (literal (String "12x")) Add (literal (Int 1L))));
          ],
          [ expression_statement (assign "value" (literal (Int 0L))) ],
          location );
    ]
  in
  let paths =
    run statements
    |> List.filter_map (function
      | Compo_res.Ok state, _ ->
          Some
            ( Option.bind (State.find_variable "value" state) Value.int_value,
              List.map
                (fun event -> event.Error.Runtime_event.message)
                (State.runtime_events state) )
      | _ -> None)
    |> List.sort Stdlib.compare
  in
  Alcotest.(check (list (pair (option int64) (list string))))
    "branch-local warnings"
    [ (Some 0L, []); (Some 13L, [ "A non-numeric value encountered" ]) ]
    paths;
  let state =
    run
      [
        Php_ir.Try
          ( [
              expression_statement
                (binary (literal (String "12x")) Divide (literal (Int 0L)));
            ],
            [ catch [ "DivisionByZeroError" ] [] ],
            None,
            location );
      ]
    |> expect_single_ok "warning before caught error"
  in
  let messages =
    State.runtime_events state
    |> List.map (fun event -> event.Error.Runtime_event.message)
  in
  Alcotest.(check (list string))
    "warning retained before throw"
    [ "A non-numeric value encountered" ]
    messages

let records_array_and_dynamic_property_deprecations () =
  let float_key =
    array_element ~key:(literal (Float 1.25)) (variable_lvalue "array")
  in
  let false_element =
    array_element ~key:(literal (Int 0L)) (variable_lvalue "false_value")
  in
  let dynamic_property = object_property (variable_lvalue "object") "dynamic" in
  let statements =
    [
      expression_statement (assign "array" (array []));
      expression_statement (array_get float_key);
      expression_statement (assign "false_value" (literal (Bool false)));
      expression_statement (assign_lvalue false_element (literal (Int 1L)));
      expression_statement (assign "object" (new_ "Container" []));
      expression_statement (assign_lvalue dynamic_property (literal (Int 2L)));
    ]
  in
  let state =
    run ~classes:[ class_ "Container" [] ] statements
    |> expect_single_ok "runtime deprecations"
  in
  let events =
    State.runtime_events state
    |> List.map (fun event ->
        (event.Error.Runtime_event.severity, event.message))
  in
  Alcotest.(check (list (pair string string)))
    "deprecation events"
    [
      ( "deprecation",
        "Implicit conversion from float 1.25 to int loses precision" );
      ("warning", "Undefined array key 1");
      ("deprecation", "Automatic conversion of false to array is deprecated");
      ( "deprecation",
        "Creation of dynamic property Container::$dynamic is deprecated" );
    ]
    (List.map
       (fun (severity, message) ->
         let severity =
           match severity with
           | Error.Runtime_event.Notice -> "notice"
           | Warning -> "warning"
           | Deprecation -> "deprecation"
           | Error -> "error"
         in
         (severity, message))
       events)

let catches_runtime_errors_and_runs_finally () =
  let needs_argument = function_ "needs_argument" [ "argument" ] [] in
  let divide =
    function_ "divide" []
      [
        Php_ir.Return
          (Some (binary (literal (Int 1L)) Divide (literal (Int 0L))), location);
      ]
  in
  let invalid_offset =
    array_element ~key:(array []) (variable_lvalue "array")
  in
  let cases =
    [
      (call "divide" [], "DivisionByZeroError", "division");
      ( binary (literal (String "not numeric")) Add (literal (Int 1L)),
        "TypeError",
        "operand" );
      (call "needs_argument" [], "ArgumentCountError", "argument");
      (array_get invalid_offset, "TypeError", "offset");
      ( assign_lvalue
          (object_property (variable_lvalue "scalar") "property")
          (literal (Int 1L)),
        "Error",
        "property" );
    ]
  in
  let loop_error =
    Php_ir.Try
      ( [
          Php_ir.While
            ( literal (Bool true),
              [
                expression_statement
                  (binary (literal (Int 1L)) Divide (literal (Int 0L)));
              ],
              location );
        ],
        [
          catch [ "DivisionByZeroError" ]
            [ Php_ir.Echo ([ literal (String "loop") ], location) ];
        ],
        Some [ Php_ir.Echo ([ literal (String ":finally;") ], location) ],
        location )
  in
  let statements =
    expression_statement (assign "array" (array []))
    :: expression_statement (assign "scalar" (literal (Int 1L)))
    :: (List.map
          (fun (expression, class_name, output) ->
            Php_ir.Try
              ( [ expression_statement expression ],
                [
                  catch [ class_name ]
                    [ Php_ir.Echo ([ literal (String output) ], location) ];
                ],
                Some
                  [ Php_ir.Echo ([ literal (String ":finally;") ], location) ],
                location ))
          cases
       @ [ loop_error ])
  in
  let state =
    run ~functions:[ needs_argument; divide ] statements
    |> expect_single_ok "caught runtime errors"
  in
  Alcotest.(check string)
    "catch and finally output"
    "division:finally;operand:finally;argument:finally;offset:finally;property:finally;loop:finally;"
    (State.output state)

let catches_function_exceptions_and_preserves_identity () =
  let fail =
    function_ "fail" []
      [
        expression_statement
          (throw (new_ "RuntimeException" [ literal (String "boom") ]));
      ]
  in
  let statements =
    [
      Php_ir.Try
        ( [ expression_statement (call "fail" []) ],
          [
            catch [ "LogicException" ]
              [ Php_ir.Echo ([ literal (String "wrong") ], location) ];
            catch ~variable:"exception" [ "Exception" ]
              [
                expression_statement
                  (assign "same"
                     (binary (variable "exception") Identical
                        (variable "exception")));
                Php_ir.Echo ([ literal (String "caught") ], location);
              ];
          ],
          Some [ Php_ir.Echo ([ literal (String ":finally") ], location) ],
          location );
    ]
  in
  let state =
    run ~functions:[ fail ] statements |> expect_single_ok "caught exception"
  in
  Alcotest.(check string)
    "catch and finally output" "caught:finally" (State.output state);
  Alcotest.(check (option bool))
    "object identity" (Some true)
    (Option.bind (State.find_variable "same" state) Value.bool_value);
  match State.find_variable "exception" state with
  | Some (Value.Object id) -> (
      match State.find_object id state with
      | Some { class_name = "RuntimeException"; message = "boom" } -> ()
      | _ -> Alcotest.fail "caught object has unexpected exception metadata")
  | _ -> Alcotest.fail "catch variable does not contain an object"

let lets_finally_override_a_pending_throw () =
  let override =
    function_ "override" []
      [
        Php_ir.Try
          ( [ expression_statement (throw (literal (Int 1L))) ],
            [],
            Some [ Php_ir.Return (Some (literal (Int 9L)), location) ],
            location );
      ]
  in
  let state =
    run ~functions:[ override ]
      [ expression_statement (assign "result" (call "override" [])) ]
    |> expect_single_ok "finally override"
  in
  Alcotest.(check (option int64))
    "return from finally" (Some 9L)
    (Option.bind (State.find_variable "result" state) Value.int_value)

let reports_uncaught_exceptions_at_the_throw () =
  let fail =
    function_ "fail" []
      [
        expression_statement
          (throw (new_ "RuntimeException" [ literal (String "boom") ]));
      ]
  in
  match run ~functions:[ fail ] [ expression_statement (call "fail" []) ] with
  | [
   ( Compo_res.Error
       (Or_gave_up.E
          ( Error.Uncaught_exception
              { class_name = "RuntimeException"; message = "boom" },
            [ _throw_location; _call_location ] )),
     _ );
  ] ->
      ()
  | _ -> Alcotest.fail "uncaught exception did not retain its throw trace"

let () =
  Alcotest.run "PHP interpreter"
    [
      ( "execution",
        [
          Alcotest.test_case "assignments and division" `Quick
            evaluates_assignments_and_division;
          Alcotest.test_case "scalar coercion and comparison" `Quick
            completes_scalar_coercion_and_comparison;
          Alcotest.test_case "symbolic numeric string comparison" `Quick
            branches_on_symbolic_numeric_string_comparison;
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
          Alcotest.test_case "selected function entry point" `Quick
            executes_a_selected_function_entry_point;
          Alcotest.test_case "symbolic function returns" `Quick
            isolates_symbolic_function_returns;
          Alcotest.test_case "missing function arguments" `Quick
            reports_missing_function_arguments;
          Alcotest.test_case "recursive functions" `Quick
            supports_recursive_calls;
          Alcotest.test_case "array order and append keys" `Quick
            preserves_array_order_and_append_keys;
          Alcotest.test_case "foreach by-value snapshot" `Quick
            iterates_arrays_by_value_over_a_snapshot;
          Alcotest.test_case "foreach branch isolation" `Quick
            isolates_foreach_progress_across_symbolic_branches;
          Alcotest.test_case "foreach by-reference" `Quick
            iterates_arrays_by_reference_and_preserves_lingering_alias;
          Alcotest.test_case "foreach reference branch isolation" `Quick
            isolates_foreach_reference_promotions_across_symbolic_branches;
          Alcotest.test_case "nested array writes and copies" `Quick
            writes_nested_arrays_and_preserves_copies;
          Alcotest.test_case "array branch isolation" `Quick
            isolates_array_copies_across_symbolic_branches;
          Alcotest.test_case "array assignment evaluation order" `Quick
            evaluates_array_keys_before_assignment_values;
          Alcotest.test_case "symbolic array keys" `Quick
            reads_existing_symbolic_array_keys;
          Alcotest.test_case "fresh symbolic array keys" `Quick
            inserts_fresh_symbolic_array_keys_persistently;
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
          Alcotest.test_case "object identity and property cells" `Quick
            shares_object_identity_and_property_cells;
          Alcotest.test_case "declaring-class property identity" `Quick
            keeps_declaring_class_in_property_identity;
          Alcotest.test_case "object property branch isolation" `Quick
            isolates_object_properties_across_symbolic_branches;
          Alcotest.test_case "constructors and recursive methods" `Quick
            runs_constructors_instance_methods_and_recursive_calls;
          Alcotest.test_case "constructor throws and method branch isolation"
            `Quick propagates_constructor_throws_and_isolates_method_branches;
          Alcotest.test_case "loop control through finally" `Quick
            propagates_nested_loop_control_through_finally;
          Alcotest.test_case "undefined read warnings" `Quick
            models_undefined_reads_as_warnings_and_null;
          Alcotest.test_case "persistent runtime warnings" `Quick
            records_leading_numeric_warnings_persistently;
          Alcotest.test_case "runtime deprecations" `Quick
            records_array_and_dynamic_property_deprecations;
          Alcotest.test_case "catchable runtime errors" `Quick
            catches_runtime_errors_and_runs_finally;
          Alcotest.test_case "exception catches" `Quick
            catches_function_exceptions_and_preserves_identity;
          Alcotest.test_case "finally overrides throw" `Quick
            lets_finally_override_a_pending_throw;
          Alcotest.test_case "uncaught exception trace" `Quick
            reports_uncaught_exceptions_at_the_throw;
        ] );
    ]
