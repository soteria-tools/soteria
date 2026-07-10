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

let constructs_persistent_ordered_arrays () =
  let original =
    Value.empty_array
    |> Value.array_set (Integer_key 2L) (Value.int 1L)
    |> Value.array_set (String_key "name") (Value.string "php")
    |> Value.array_set (Integer_key 2L) (Value.int 3L)
  in
  let copy = Value.array_set (Integer_key 3L) (Value.int 4L) original in
  let original_keys = List.map fst (Value.array_bindings original) in
  let copy_keys = List.map fst (Value.array_bindings copy) in
  Alcotest.(check int) "original length" 2 (Value.array_length original);
  Alcotest.(check int) "copy length" 3 (Value.array_length copy);
  Alcotest.(check bool)
    "overwrite preserves insertion order" true
    (original_keys = [ Integer_key 2L; String_key "name" ]);
  Alcotest.(check bool)
    "copy gets a new final key" true
    (copy_keys = [ Integer_key 2L; String_key "name"; Integer_key 3L ]);
  check_int "original overwritten value" 3L
    (Option.get (Value.array_find (Integer_key 2L) original));
  Alcotest.(check bool)
    "original is unchanged by copy update" true
    (Option.is_none (Value.array_find (Integer_key 3L) original))

let tracks_negative_append_keys () =
  let array =
    Value.array_set (Value.Integer_key (-5L)) (Value.string "value")
      Value.empty_array
  in
  Alcotest.(check bool)
    "negative successor" true
    (Value.array_next_key array = Some (Value.Integer_key (-4L)));
  let exhausted =
    Value.array_set (Value.Integer_key Int64.max_int) Value.null
      Value.empty_array
  in
  Alcotest.(check bool)
    "maximum key exhausts append" true
    (Option.is_none (Value.array_next_key exhausted))

let () =
  Alcotest.run "PHP values"
    [
      ( "scalars",
        [
          Alcotest.test_case "construction" `Quick constructs_scalars;
          Alcotest.test_case "IR literals" `Quick converts_ir_literals;
          Alcotest.test_case "printing" `Quick prints_signed_integers;
          Alcotest.test_case "persistent ordered arrays" `Quick
            constructs_persistent_ordered_arrays;
          Alcotest.test_case "array append keys" `Quick
            tracks_negative_append_keys;
        ] );
    ]
