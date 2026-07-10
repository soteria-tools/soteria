module Value = Soteria_php.Value

let check_int label expected value =
  Alcotest.(check (option int64)) label (Some expected) (Value.int_value value)

let constructs_scalars () =
  Alcotest.(check string)
    "undefined type" "undefined"
    (Value.type_name Value.undef);
  Alcotest.(check string) "null type" "null" (Value.type_name Value.null);
  Alcotest.(check (option bool))
    "boolean" (Some true)
    (Value.bool_value (Value.bool true));
  check_int "minimum integer" Int64.min_int (Value.int Int64.min_int);
  check_int "maximum integer" Int64.max_int (Value.int Int64.max_int);
  let negative_zero = Option.get (Value.float_value (Value.float (-0.0))) in
  Alcotest.(check int64)
    "negative zero"
    (Int64.bits_of_float (-0.0))
    (Int64.bits_of_float negative_zero);
  Alcotest.(check (option string))
    "string" (Some "soteria")
    (Value.string_value (Value.string "soteria"))

let converts_ir_literals () =
  check_int "IR integer" 42L (Value.of_literal (Soteria_php.Php_ir.Int 42L));
  Alcotest.(check (option string))
    "IR string" (Some "php")
    (Value.of_literal (Soteria_php.Php_ir.String "php") |> Value.string_value)

let prints_signed_integers () =
  Alcotest.(check string)
    "signed integer" "-1"
    (Format.asprintf "%a" Value.pp (Value.int (-1L)));
  Alcotest.(check string)
    "quoted string" "\"php\""
    (Format.asprintf "%a" Value.pp (Value.string "php"))

let () =
  Alcotest.run "PHP values"
    [
      ( "scalars",
        [
          Alcotest.test_case "construction" `Quick constructs_scalars;
          Alcotest.test_case "IR literals" `Quick converts_ir_literals;
          Alcotest.test_case "printing" `Quick prints_signed_integers;
        ] );
    ]
