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

let program ?(schema_version = 3) ?(functions = []) statement =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("target_php_version", `String "8.4.19");
      ("source_file", `String "test.php");
      ("functions", `List functions);
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
  match Soteria_php.Php_ir.of_yojson (program ~schema_version:4 statement) with
  | Error error ->
      Alcotest.(check string)
        "error" "$.schema_version: unsupported schema version 4 (expected 3)"
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
        ] );
    ]
