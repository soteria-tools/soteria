module Coercion = Soteria_php.Coercion
module Value = Soteria_php.Value

let fail path message = Alcotest.failf "%s: %s" path message

let member path name json =
  match Yojson.Safe.Util.member name json with
  | `Null -> fail path ("missing field " ^ name)
  | value -> value

let string path json =
  try Yojson.Safe.Util.to_string json
  with Yojson.Safe.Util.Type_error _ -> fail path "expected a string"

let bool path json =
  try Yojson.Safe.Util.to_bool json
  with Yojson.Safe.Util.Type_error _ -> fail path "expected a boolean"

let list path json =
  try Yojson.Safe.Util.to_list json
  with Yojson.Safe.Util.Type_error _ -> fail path "expected an array"

let find_file name =
  let executable_directory = Filename.dirname Sys.argv.(0) in
  let candidates =
    [
      Filename.concat "soteria-php/test/unit" name;
      Filename.concat executable_directory name;
      name;
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> fail name "test dependency not found"

let read_all channel =
  let buffer = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buffer channel 4096
     done
   with End_of_file -> ());
  Buffer.contents buffer

let run_oracle () =
  let php = Option.value ~default:"php" (Sys.getenv_opt "SOTERIA_PHP_ORACLE") in
  let oracle = find_file "coercion_oracle.php" in
  let cases = find_file "coercion_cases.json" in
  let channel = Unix.open_process_args_in php [| php; oracle; cases |] in
  let output = read_all channel in
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 -> Yojson.Safe.from_string output
  | Unix.WEXITED status ->
      fail "PHP oracle" (Printf.sprintf "exited with status %d" status)
  | Unix.WSIGNALED signal ->
      fail "PHP oracle" (Printf.sprintf "was killed by signal %d" signal)
  | Unix.WSTOPPED signal ->
      fail "PHP oracle" (Printf.sprintf "was stopped by signal %d" signal)

let decode_float = function
  | "INF" -> infinity
  | "-INF" -> neg_infinity
  | "NAN" -> nan
  | value -> float_of_string value

let decode_input path json =
  match member path "type" json |> string (path ^ ".type") with
  | "null" -> Value.null
  | "bool" -> Value.bool (member path "value" json |> bool (path ^ ".value"))
  | "int" ->
      member path "value" json
      |> string (path ^ ".value")
      |> Int64.of_string
      |> Value.int
  | "float" ->
      member path "value" json
      |> string (path ^ ".value")
      |> decode_float
      |> Value.float
  | "string" ->
      member path "value" json |> string (path ^ ".value") |> Value.string
  | type_ -> fail path ("unknown value type " ^ type_)

let target = function
  | "bool" -> Coercion.Boolean
  | "int" -> Coercion.Integer
  | "float" -> Coercion.Float
  | "string" -> Coercion.String
  | target -> fail target "unknown coercion target"

let check_result path expected actual =
  let warnings = member path "warnings" expected |> list (path ^ ".warnings") in
  Alcotest.(check int) (path ^ " warning count") 0 (List.length warnings);
  if Yojson.Safe.Util.member "error" expected <> `Null then
    fail path "the PHP oracle raised an error";
  let expected = member path "value" expected in
  match member path "type" expected |> string (path ^ ".type") with
  | "bool" ->
      let expected = member path "value" expected |> bool (path ^ ".value") in
      Alcotest.(check (option bool))
        path (Some expected) (Value.bool_value actual)
  | "int" ->
      let expected =
        member path "value" expected
        |> string (path ^ ".value")
        |> Int64.of_string
      in
      Alcotest.(check (option int64))
        path (Some expected) (Value.int_value actual)
  | "float" ->
      let expected =
        member path "value" expected |> string (path ^ ".value") |> decode_float
      in
      let actual =
        match Value.float_value actual with
        | Some value -> value
        | None -> fail path "Soteria returned a non-concrete float"
      in
      if Float.is_nan expected then
        Alcotest.(check bool) path true (Float.is_nan actual)
      else
        Alcotest.(check int64)
          path
          (Int64.bits_of_float expected)
          (Int64.bits_of_float actual)
  | "string" ->
      let expected = member path "value" expected |> string (path ^ ".value") in
      Alcotest.(check (option string))
        path (Some expected)
        (Value.string_value actual)
  | type_ -> fail path ("unknown oracle result type " ^ type_)

let differential_scalar_coercions () =
  let oracle = run_oracle () in
  Alcotest.(check string)
    "oracle PHP version" Soteria_php.Php_ir.target_php_version
    (member "$" "php_version" oracle |> string "$.php_version");
  member "$" "results" oracle
  |> list "$.results"
  |> List.iteri (fun index result ->
      let path = Printf.sprintf "$.results[%d]" index in
      let input = member path "input" result in
      let label = member path "label" input |> string (path ^ ".input.label") in
      let value = decode_input (path ^ ".input") input in
      let casts = member path "casts" result in
      List.iter
        (fun target_name ->
          let test_path = label ^ " to " ^ target_name in
          match Coercion.coerce (target target_name) value with
          | Ok actual ->
              check_result test_path
                (member (path ^ ".casts") target_name casts)
                actual
          | Error error ->
              fail test_path (Format.asprintf "%a" Coercion.pp_error error))
        [ "bool"; "int"; "float"; "string" ])

let differential_numeric_operands () =
  let oracle = run_oracle () in
  member "$" "results" oracle
  |> list "$.results"
  |> List.iteri (fun index result ->
      let path = Printf.sprintf "$.results[%d]" index in
      let input = member path "input" result in
      let label = member path "label" input |> string (path ^ ".input.label") in
      let value = decode_input (path ^ ".input") input in
      let expected = member path "numeric" result in
      let warnings = member path "warnings" expected |> list path in
      let has_error = Yojson.Safe.Util.member "error" expected <> `Null in
      match (warnings, has_error, Coercion.to_number value) with
      | [], false, Ok actual ->
          check_result (label ^ " numeric") expected actual
      | _ :: _, false, Error (Leading_numeric_string _) -> ()
      | [], true, Error (Invalid_numeric_operand `String) -> ()
      | _, _, Error error ->
          fail label
            (Format.asprintf "unexpected error: %a" Coercion.pp_error error)
      | _ -> fail label "numeric operand result disagrees with PHP")

let comparison_operator = function
  | "equal" -> Coercion.Equal
  | "less_than" -> Coercion.Less_than
  | "less_than_or_equal" -> Coercion.Less_than_or_equal
  | "greater_than" -> Coercion.Greater_than
  | "greater_than_or_equal" -> Coercion.Greater_than_or_equal
  | operator -> fail operator "unknown comparison operator"

let differential_scalar_comparisons () =
  let oracle = run_oracle () in
  member "$" "comparisons" oracle
  |> list "$.comparisons"
  |> List.iteri (fun index result ->
      let path = Printf.sprintf "$.comparisons[%d]" index in
      let left_json = member path "left" result in
      let right_json = member path "right" result in
      let left = decode_input (path ^ ".left") left_json in
      let right = decode_input (path ^ ".right") right_json in
      let left_label = member path "label" left_json |> string path in
      let right_label = member path "label" right_json |> string path in
      List.iter
        (fun operator_name ->
          let label = left_label ^ " " ^ operator_name ^ " " ^ right_label in
          let expected = member path operator_name result |> bool path in
          match
            Coercion.compare_scalar
              (comparison_operator operator_name)
              left right
          with
          | Ok actual ->
              let actual =
                actual
                |> Value.Typed.untyped
                |> Value.Typed.Eval.eval ~force:true
                |> Value.Typed.type_
              in
              Alcotest.(check (option bool))
                label (Some expected)
                (Value.Typed.Bool.to_bool actual)
          | Error error ->
              fail label (Format.asprintf "%a" Coercion.pp_error error))
        [
          "equal";
          "less_than";
          "less_than_or_equal";
          "greater_than";
          "greater_than_or_equal";
        ])

let symbolic_coercions () =
  let open Value in
  let integer =
    Typed.mk_var (Soteria.Symex.Var.of_int 0) (Typed.t_int integer_bits)
  in
  let boolean = Typed.mk_var (Soteria.Symex.Var.of_int 1) Typed.t_bool in
  let float = Typed.mk_var (Soteria.Symex.Var.of_int 2) Typed.t_f64 in
  (match Coercion.to_bool (Int integer) with
  | Ok (Bool actual) ->
      Alcotest.(check bool)
        "symbolic integer to bool" true
        (Typed.equal actual (Typed.BitVec.to_bool integer))
  | Ok _ -> Alcotest.fail "integer-to-bool returned the wrong PHP type"
  | Error error -> Alcotest.fail (Format.asprintf "%a" Coercion.pp_error error));
  (match Coercion.to_string (Bool boolean) with
  | Error (Symbolic_conversion { target = String; _ }) -> ()
  | Error error -> Alcotest.fail (Format.asprintf "%a" Coercion.pp_error error)
  | Ok _ -> Alcotest.fail "symbolic bool-to-string should be unsupported");
  (match Coercion.to_int (Float float) with
  | Error (Symbolic_conversion { target = Integer; _ }) -> ()
  | Error error -> Alcotest.fail (Format.asprintf "%a" Coercion.pp_error error)
  | Ok _ -> Alcotest.fail "symbolic float-to-int should be unsupported");
  match Coercion.to_bool Undef with
  | Error (Undefined_value Boolean) -> ()
  | Error error -> Alcotest.fail (Format.asprintf "%a" Coercion.pp_error error)
  | Ok _ -> Alcotest.fail "undefined-to-bool should be unsupported"

let array_key_coercions () =
  let check label expected value =
    match Coercion.to_array_key value with
    | Ok (Concrete_key actual) ->
        Alcotest.(check bool) label true (actual = expected)
    | Ok (Symbolic_integer_key _) ->
        Alcotest.failf "%s: expected a concrete key" label
    | Error error ->
        Alcotest.fail (Format.asprintf "%a" Coercion.pp_error error)
  in
  check "canonical integer string" (Value.Integer_key 12L) (Value.string "12");
  check "leading-zero string" (Value.String_key "012") (Value.string "012");
  check "negative zero string" (Value.String_key "-0") (Value.string "-0");
  check "out-of-range integer string" (Value.String_key "9223372036854775808")
    (Value.string "9223372036854775808");
  check "true" (Value.Integer_key 1L) (Value.bool true);
  check "null" (Value.String_key "") Value.null;
  check "float truncation" (Value.Integer_key (-1L)) (Value.float (-1.9));
  match Coercion.to_array_key (Value.array Value.empty_array) with
  | Error (Invalid_array_key `Array) -> ()
  | Error error -> Alcotest.fail (Format.asprintf "%a" Coercion.pp_error error)
  | Ok _ -> Alcotest.fail "an array was accepted as an array key"

let () =
  Alcotest.run "PHP coercions"
    [
      ( "scalar casts",
        [
          Alcotest.test_case "PHP differential oracle" `Quick
            differential_scalar_coercions;
          Alcotest.test_case "numeric operands" `Quick
            differential_numeric_operands;
          Alcotest.test_case "scalar comparisons" `Quick
            differential_scalar_comparisons;
          Alcotest.test_case "symbolic values" `Quick symbolic_coercions;
          Alcotest.test_case "array keys" `Quick array_key_coercions;
        ] );
    ]
