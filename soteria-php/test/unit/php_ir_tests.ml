let position line column offset =
  `Assoc
    [ ("line", `Int line); ("column", `Int column); ("offset", `Int offset) ]

let location =
  `Assoc
    [
      ("file", `String "test.php");
      ("start", position 1 1 0);
      ("end", position 1 2 1);
    ]

let program ?(schema_version = 10) ?(functions = []) ?(classes = []) statement =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("target_php_version", `String "8.4.19");
      ("source_file", `String "test.php");
      ("functions", `List functions);
      ("classes", `List classes);
      ("statements", `List [ statement ]);
    ]

let decodes_supported_ir () =
  let open Soteria_php.Php_ir in
  let expression =
    `Assoc
      [
        ("kind", `String "int");
        ("value", `String "9223372036854775807");
        ("location", location);
      ]
  in
  let statement =
    `Assoc
      [
        ("kind", `String "echo");
        ("expressions", `List [ expression ]);
        ("location", location);
      ]
  in
  match Soteria_php.Php_ir.of_yojson (program statement) with
  | Ok { statements = [ Echo ([ { desc = Literal (Int value); _ } ], _) ]; _ }
    ->
      Alcotest.(check int64) "integer value" Int64.max_int value
  | Ok _ -> Alcotest.fail "decoded an unexpected IR shape"
  | Error error -> Alcotest.fail error

let rejects_unknown_schema () =
  let statement = `Assoc [ ("kind", `String "nop"); ("location", location) ] in
  match Soteria_php.Php_ir.of_yojson (program ~schema_version:11 statement) with
  | Error error ->
      Alcotest.(check string)
        "error" "$.schema_version: unsupported schema version 11 (expected 10)"
        error
  | Ok _ -> Alcotest.fail "accepted an incompatible schema"

let rejects_unknown_fields () =
  let statement =
    `Assoc
      [
        ("kind", `String "nop"); ("location", location); ("ignored", `Bool true);
      ]
  in
  match Soteria_php.Php_ir.of_yojson (program statement) with
  | Error error ->
      Alcotest.(check string)
        "error" "$.statements[0]: unknown field ignored" error
  | Ok _ -> Alcotest.fail "accepted an unknown field"

let decodes_functions_and_returns () =
  let variable =
    `Assoc
      [
        ("kind", `String "variable");
        ("name", `String "value");
        ("location", location);
      ]
  in
  let return =
    `Assoc
      [
        ("kind", `String "return");
        ("expression", variable);
        ("location", location);
      ]
  in
  let parameter =
    `Assoc [ ("name", `String "value"); ("location", location) ]
  in
  let function_ =
    `Assoc
      [
        ("name", `String "identity");
        ("parameters", `List [ parameter ]);
        ("body", `List [ return ]);
        ("location", location);
      ]
  in
  let statement = `Assoc [ ("kind", `String "nop"); ("location", location) ] in
  match
    Soteria_php.Php_ir.of_yojson (program ~functions:[ function_ ] statement)
  with
  | Ok
      {
        functions =
          [
            {
              name = "identity";
              parameters = [ { name = "value"; _ } ];
              body = [ Return (Some { desc = Variable "value"; _ }, _) ];
              _;
            };
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "decoded an unexpected function shape"
  | Error error -> Alcotest.fail error

let rejects_top_level_returns () =
  let statement =
    `Assoc
      [
        ("kind", `String "return"); ("expression", `Null); ("location", location);
      ]
  in
  match Soteria_php.Php_ir.of_yojson (program statement) with
  | Error error ->
      Alcotest.(check string)
        "error" "$.statements[0]: return is only valid in a function body" error
  | Ok _ -> Alcotest.fail "accepted a top-level return"

let decodes_arrays_and_lvalues () =
  let int value =
    `Assoc
      [
        ("kind", `String "int"); ("value", `String value); ("location", location);
      ]
  in
  let variable =
    `Assoc
      [
        ("kind", `String "variable");
        ("name", `String "array");
        ("location", location);
      ]
  in
  let target =
    `Assoc
      [
        ("kind", `String "array_element");
        ("array", variable);
        ("key", `Null);
        ("location", location);
      ]
  in
  let array =
    `Assoc
      [
        ("kind", `String "array");
        ( "items",
          `List
            [
              `Assoc
                [ ("key", int "2"); ("value", int "3"); ("location", location) ];
            ] );
        ("location", location);
      ]
  in
  let assign =
    `Assoc
      [
        ("kind", `String "assign");
        ("target", target);
        ("value", array);
        ("location", location);
      ]
  in
  let statement =
    `Assoc
      [
        ("kind", `String "expression");
        ("expression", assign);
        ("location", location);
      ]
  in
  match Soteria_php.Php_ir.of_yojson (program statement) with
  | Ok
      {
        statements =
          [
            Expression
              ( {
                  desc =
                    Assign
                      ( { desc = Array_element_lvalue (_, None); _ },
                        {
                          desc =
                            Array
                              [
                                {
                                  key = Some { desc = Literal (Int 2L); _ };
                                  value = { desc = Literal (Int 3L); _ };
                                  _;
                                };
                              ];
                          _;
                        } );
                  _;
                },
                _ );
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "decoded an unexpected array or lvalue shape"
  | Error error -> Alcotest.fail error

let rejects_array_append_reads () =
  let variable =
    `Assoc
      [
        ("kind", `String "variable");
        ("name", `String "array");
        ("location", location);
      ]
  in
  let target =
    `Assoc
      [
        ("kind", `String "array_element");
        ("array", variable);
        ("key", `Null);
        ("location", location);
      ]
  in
  let get =
    `Assoc
      [
        ("kind", `String "array_get"); ("target", target); ("location", location);
      ]
  in
  let statement =
    `Assoc
      [
        ("kind", `String "expression");
        ("expression", get);
        ("location", location);
      ]
  in
  match Soteria_php.Php_ir.of_yojson (program statement) with
  | Error error ->
      Alcotest.(check string)
        "error" "$.statements[0].expression.target.key: append cannot be read"
        error
  | Ok _ -> Alcotest.fail "accepted an array append read"

let decodes_references_and_unset () =
  let lvalue name =
    `Assoc
      [
        ("kind", `String "variable");
        ("name", `String name);
        ("location", location);
      ]
  in
  let assignment =
    `Assoc
      [
        ("kind", `String "assign_reference");
        ("target", lvalue "alias");
        ("source", lvalue "value");
        ("location", location);
      ]
  in
  let expression_statement =
    `Assoc
      [
        ("kind", `String "expression");
        ("expression", assignment);
        ("location", location);
      ]
  in
  let unset =
    `Assoc
      [
        ("kind", `String "unset");
        ("targets", `List [ lvalue "alias" ]);
        ("location", location);
      ]
  in
  let json =
    match program expression_statement with
    | `Assoc fields ->
        `Assoc
          (("statements", `List [ expression_statement; unset ])
          :: List.remove_assoc "statements" fields)
    | _ -> assert false
  in
  match Soteria_php.Php_ir.of_yojson json with
  | Ok
      {
        statements =
          [
            Expression
              ( {
                  desc =
                    Assign_reference
                      ( { desc = Variable_lvalue "alias"; _ },
                        { desc = Variable_lvalue "value"; _ } );
                  _;
                },
                _ );
            Unset ([ { desc = Variable_lvalue "alias"; _ } ], _);
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "decoded an unexpected reference or unset shape"
  | Error error -> Alcotest.fail error

let decodes_exceptions_and_structured_control () =
  let string value =
    `Assoc
      [
        ("kind", `String "string");
        ("value", `String value);
        ("location", location);
      ]
  in
  let new_exception =
    `Assoc
      [
        ("kind", `String "new");
        ("class", `String "RuntimeException");
        ("arguments", `List [ string "boom" ]);
        ("location", location);
      ]
  in
  let throw =
    `Assoc
      [
        ("kind", `String "throw");
        ("expression", new_exception);
        ("location", location);
      ]
  in
  let expression =
    `Assoc
      [
        ("kind", `String "expression");
        ("expression", throw);
        ("location", location);
      ]
  in
  let nop = `Assoc [ ("kind", `String "nop"); ("location", location) ] in
  let catch =
    `Assoc
      [
        ("types", `List [ `String "LogicException"; `String "Exception" ]);
        ("variable", `String "exception");
        ("body", `List [ nop ]);
        ("location", location);
      ]
  in
  let try_ =
    `Assoc
      [
        ("kind", `String "try");
        ("body", `List [ expression ]);
        ("catches", `List [ catch ]);
        ("finally", `List [ nop ]);
        ("location", location);
      ]
  in
  match Soteria_php.Php_ir.of_yojson (program try_) with
  | Ok
      {
        statements =
          [
            Try
              ( [
                  Expression
                    ( {
                        desc =
                          Throw
                            {
                              desc =
                                New
                                  ( "RuntimeException",
                                    [ { desc = Literal (String "boom"); _ } ] );
                              _;
                            };
                        _;
                      },
                      _ );
                ],
                [
                  {
                    types = [ "LogicException"; "Exception" ];
                    variable = Some "exception";
                    _;
                  };
                ],
                Some [ Nop _ ],
                _ );
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "decoded an unexpected exception shape"
  | Error error -> Alcotest.fail error

let decodes_classes_and_object_properties () =
  let int =
    `Assoc
      [
        ("kind", `String "int"); ("value", `String "1"); ("location", location);
      ]
  in
  let property =
    `Assoc
      [ ("name", `String "value"); ("default", int); ("location", location) ]
  in
  let class_ =
    `Assoc
      [
        ("name", `String "Box");
        ("properties", `List [ property ]);
        ("methods", `List []);
        ("location", location);
      ]
  in
  let variable =
    `Assoc
      [
        ("kind", `String "variable");
        ("name", `String "box");
        ("location", location);
      ]
  in
  let property_lvalue =
    `Assoc
      [
        ("kind", `String "object_property");
        ("object", variable);
        ("name", `String "value");
        ("location", location);
      ]
  in
  let get =
    `Assoc
      [
        ("kind", `String "property_get");
        ("target", property_lvalue);
        ("location", location);
      ]
  in
  let statement =
    `Assoc
      [
        ("kind", `String "expression");
        ("expression", get);
        ("location", location);
      ]
  in
  match
    Soteria_php.Php_ir.of_yojson (program ~classes:[ class_ ] statement)
  with
  | Ok
      {
        classes =
          [
            {
              name = "Box";
              properties =
                [
                  {
                    name = "value";
                    default = Some { desc = Literal (Int 1L); _ };
                    _;
                  };
                ];
              _;
            };
          ];
        statements =
          [
            Expression
              ( {
                  desc =
                    Property_get
                      {
                        desc =
                          Object_property_lvalue
                            ({ desc = Variable_lvalue "box"; _ }, "value");
                        _;
                      };
                  _;
                },
                _ );
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "decoded an unexpected class or property shape"
  | Error error -> Alcotest.fail error

let decodes_methods_and_method_calls () =
  let variable name =
    `Assoc
      [
        ("kind", `String "variable");
        ("name", `String name);
        ("location", location);
      ]
  in
  let parameter =
    `Assoc [ ("name", `String "value"); ("location", location) ]
  in
  let return =
    `Assoc
      [
        ("kind", `String "return");
        ("expression", variable "value");
        ("location", location);
      ]
  in
  let method_ =
    `Assoc
      [
        ("name", `String "identity");
        ("parameters", `List [ parameter ]);
        ("body", `List [ return ]);
        ("modifiers", `List [ `String "public" ]);
        ("location", location);
      ]
  in
  let class_ =
    `Assoc
      [
        ("name", `String "Box");
        ("properties", `List []);
        ("methods", `List [ method_ ]);
        ("location", location);
      ]
  in
  let call =
    `Assoc
      [
        ("kind", `String "method_call");
        ("object", variable "box");
        ("method", `String "identity");
        ("arguments", `List [ variable "argument" ]);
        ("location", location);
      ]
  in
  let statement =
    `Assoc
      [
        ("kind", `String "expression");
        ("expression", call);
        ("location", location);
      ]
  in
  match
    Soteria_php.Php_ir.of_yojson (program ~classes:[ class_ ] statement)
  with
  | Ok
      {
        classes =
          [
            {
              methods =
                [
                  {
                    name = "identity";
                    parameters = [ { name = "value"; _ } ];
                    body = [ Return (Some { desc = Variable "value"; _ }, _) ];
                    modifiers = [ Public ];
                    _;
                  };
                ];
              _;
            };
          ];
        statements =
          [
            Expression
              ( {
                  desc =
                    Method_call
                      ( { desc = Variable "box"; _ },
                        "identity",
                        [ { desc = Variable "argument"; _ } ] );
                  _;
                },
                _ );
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "decoded an unexpected method shape"
  | Error error -> Alcotest.fail error

let decodes_foreach_by_reference () =
  let variable name =
    `Assoc
      [
        ("kind", `String "variable");
        ("name", `String name);
        ("location", location);
      ]
  in
  let foreach =
    `Assoc
      [
        ("kind", `String "foreach");
        ("iterable", variable "items");
        ("key", variable "key");
        ("value", variable "value");
        ("by_reference", `Bool true);
        ( "body",
          `List
            [
              `Assoc
                [
                  ("kind", `String "continue");
                  ("depth", `Int 1);
                  ("location", location);
                ];
            ] );
        ("location", location);
      ]
  in
  match Soteria_php.Php_ir.of_yojson (program foreach) with
  | Ok
      {
        statements =
          [
            Foreach
              ( { desc = Variable "items"; _ },
                Some { desc = Variable_lvalue "key"; _ },
                { desc = Variable_lvalue "value"; _ },
                true,
                [ Continue (1, _) ],
                _ );
          ];
        _;
      } ->
      ()
  | Ok _ -> Alcotest.fail "decoded an unexpected foreach shape"
  | Error error -> Alcotest.fail error

let rejects_invalid_loop_control () =
  let break =
    `Assoc
      [ ("kind", `String "break"); ("depth", `Int 1); ("location", location) ]
  in
  match Soteria_php.Php_ir.of_yojson (program break) with
  | Error error ->
      Alcotest.(check string)
        "error"
        "$.statements[0].depth: break depth 1 exceeds enclosing loop depth 0"
        error
  | Ok _ -> Alcotest.fail "accepted break outside a loop"

let () =
  Alcotest.run "PHP IR"
    [
      ( "decoder",
        [
          Alcotest.test_case "supported IR" `Quick decodes_supported_ir;
          Alcotest.test_case "schema version" `Quick rejects_unknown_schema;
          Alcotest.test_case "unknown field" `Quick rejects_unknown_fields;
          Alcotest.test_case "functions and returns" `Quick
            decodes_functions_and_returns;
          Alcotest.test_case "top-level return" `Quick rejects_top_level_returns;
          Alcotest.test_case "arrays and lvalues" `Quick
            decodes_arrays_and_lvalues;
          Alcotest.test_case "array append reads" `Quick
            rejects_array_append_reads;
          Alcotest.test_case "references and unset" `Quick
            decodes_references_and_unset;
          Alcotest.test_case "exceptions and structured control" `Quick
            decodes_exceptions_and_structured_control;
          Alcotest.test_case "classes and object properties" `Quick
            decodes_classes_and_object_properties;
          Alcotest.test_case "methods and method calls" `Quick
            decodes_methods_and_method_calls;
          Alcotest.test_case "foreach by reference" `Quick
            decodes_foreach_by_reference;
          Alcotest.test_case "loop-control validation" `Quick
            rejects_invalid_loop_control;
        ] );
    ]
