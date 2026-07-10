type position = { line : int; column : int; offset : int }
type location = { file : string; start : position; end_ : position }

type literal =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string

type expression = { literal : literal; location : location }

type statement =
  | Expression of expression * location
  | Echo of expression list * location
  | Nop of location

type t = {
  target_php_version : string;
  source_file : string;
  statements : statement list;
}

let schema_version = 1

(* [versionsync: PHP_VERSION=8.4.19] *)
let target_php_version = "8.4.19"

exception Decode_error of string

let decode_error path message = raise (Decode_error (path ^ ": " ^ message))

let as_assoc path = function
  | `Assoc fields -> fields
  | _ -> decode_error path "expected an object"

let as_list path = function
  | `List values -> values
  | _ -> decode_error path "expected an array"

let as_string path = function
  | `String value -> value
  | _ -> decode_error path "expected a string"

let as_bool path = function
  | `Bool value -> value
  | _ -> decode_error path "expected a boolean"

let as_int path = function
  | `Int value -> value
  | _ -> decode_error path "expected an integer"

let check_fields path allowed fields =
  let rec check_seen seen = function
    | [] -> ()
    | (name, _) :: rest ->
        if List.mem name seen then decode_error path ("duplicate field " ^ name)
        else if not (List.mem name allowed) then
          decode_error path ("unknown field " ^ name)
        else check_seen (name :: seen) rest
  in
  check_seen [] fields

let field path name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> decode_error path ("missing field " ^ name)

let decode_position path json =
  let fields = as_assoc path json in
  check_fields path [ "line"; "column"; "offset" ] fields;
  let line = field path "line" fields |> as_int (path ^ ".line") in
  let column = field path "column" fields |> as_int (path ^ ".column") in
  let offset = field path "offset" fields |> as_int (path ^ ".offset") in
  if line < 1 then decode_error (path ^ ".line") "must be positive";
  if column < 1 then decode_error (path ^ ".column") "must be positive";
  if offset < 0 then decode_error (path ^ ".offset") "must not be negative";
  { line; column; offset }

let decode_location path json =
  let fields = as_assoc path json in
  check_fields path [ "file"; "start"; "end" ] fields;
  let file = field path "file" fields |> as_string (path ^ ".file") in
  let start = field path "start" fields |> decode_position (path ^ ".start") in
  let end_ = field path "end" fields |> decode_position (path ^ ".end") in
  if end_.offset < start.offset then
    decode_error path "end offset precedes start offset";
  { file; start; end_ }

let decode_int path value =
  let length = String.length value in
  let first_digit = if length > 0 && value.[0] = '-' then 1 else 0 in
  let rec all_digits index =
    if index = length then true
    else
      match value.[index] with
      | '0' .. '9' -> all_digits (index + 1)
      | _ -> false
  in
  if first_digit = length || not (all_digits first_digit) then
    decode_error path "expected a signed 64-bit decimal integer";
  try Int64.of_string value
  with Failure _ ->
    decode_error path "expected a signed 64-bit decimal integer"

let decode_float path value =
  let value =
    try float_of_string value
    with Failure _ -> decode_error path "expected a decimal float"
  in
  match classify_float value with
  | FP_normal | FP_subnormal | FP_zero -> value
  | FP_infinite | FP_nan -> decode_error path "float must be finite"

let decode_expression path json =
  let fields = as_assoc path json in
  let kind = field path "kind" fields |> as_string (path ^ ".kind") in
  let literal =
    match kind with
    | "null" ->
        check_fields path [ "kind"; "location" ] fields;
        Null
    | "bool" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        Bool (field path "value" fields |> as_bool (path ^ ".value"))
    | "int" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        let value = field path "value" fields |> as_string (path ^ ".value") in
        Int (decode_int (path ^ ".value") value)
    | "float" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        let value = field path "value" fields |> as_string (path ^ ".value") in
        Float (decode_float (path ^ ".value") value)
    | "string" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        String (field path "value" fields |> as_string (path ^ ".value"))
    | kind -> decode_error (path ^ ".kind") ("unknown expression kind " ^ kind)
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { literal; location }

let decode_statement path json =
  let fields = as_assoc path json in
  let kind = field path "kind" fields |> as_string (path ^ ".kind") in
  match kind with
  | "expression" ->
      check_fields path [ "kind"; "expression"; "location" ] fields;
      let expression =
        field path "expression" fields
        |> decode_expression (path ^ ".expression")
      in
      let location =
        field path "location" fields |> decode_location (path ^ ".location")
      in
      Expression (expression, location)
  | "echo" ->
      check_fields path [ "kind"; "expressions"; "location" ] fields;
      let expressions =
        field path "expressions" fields
        |> as_list (path ^ ".expressions")
        |> List.mapi (fun index ->
            decode_expression (Printf.sprintf "%s.expressions[%d]" path index))
      in
      let location =
        field path "location" fields |> decode_location (path ^ ".location")
      in
      Echo (expressions, location)
  | "nop" ->
      check_fields path [ "kind"; "location" ] fields;
      let location =
        field path "location" fields |> decode_location (path ^ ".location")
      in
      Nop location
  | kind -> decode_error (path ^ ".kind") ("unknown statement kind " ^ kind)

let locations_of_statement = function
  | Expression (expression, location) -> [ expression.location; location ]
  | Echo (expressions, location) ->
      location :: List.map (fun expression -> expression.location) expressions
  | Nop location -> [ location ]

let validate_source_file source_file statements =
  List.iter
    (fun statement ->
      List.iter
        (fun location ->
          if not (String.equal source_file location.file) then
            decode_error "$.statements" "location file differs from source_file")
        (locations_of_statement statement))
    statements

let of_yojson json =
  try
    let fields = as_assoc "$" json in
    check_fields "$"
      [ "schema_version"; "target_php_version"; "source_file"; "statements" ]
      fields;
    let actual_schema =
      field "$" "schema_version" fields |> as_int "$.schema_version"
    in
    if actual_schema <> schema_version then
      decode_error "$.schema_version"
        (Printf.sprintf "unsupported schema version %d (expected %d)"
           actual_schema schema_version);
    let actual_php_version =
      field "$" "target_php_version" fields |> as_string "$.target_php_version"
    in
    if not (String.equal actual_php_version target_php_version) then
      decode_error "$.target_php_version"
        (Printf.sprintf "unsupported PHP version %s (expected %s)"
           actual_php_version target_php_version);
    let source_file =
      field "$" "source_file" fields |> as_string "$.source_file"
    in
    let statements =
      field "$" "statements" fields
      |> as_list "$.statements"
      |> List.mapi (fun index ->
          decode_statement (Printf.sprintf "$.statements[%d]" index))
    in
    validate_source_file source_file statements;
    Ok { target_php_version = actual_php_version; source_file; statements }
  with Decode_error message -> Error message

let position_to_yojson position =
  `Assoc
    [
      ("line", `Int position.line);
      ("column", `Int position.column);
      ("offset", `Int position.offset);
    ]

let location_to_yojson location =
  `Assoc
    [
      ("file", `String location.file);
      ("start", position_to_yojson location.start);
      ("end", position_to_yojson location.end_);
    ]

let expression_to_yojson expression =
  let fields =
    match expression.literal with
    | Null -> [ ("kind", `String "null") ]
    | Bool value -> [ ("kind", `String "bool"); ("value", `Bool value) ]
    | Int value ->
        [ ("kind", `String "int"); ("value", `String (Int64.to_string value)) ]
    | Float value ->
        [
          ("kind", `String "float");
          ("value", `String (Printf.sprintf "%.17g" value));
        ]
    | String value -> [ ("kind", `String "string"); ("value", `String value) ]
  in
  `Assoc (fields @ [ ("location", location_to_yojson expression.location) ])

let statement_to_yojson = function
  | Expression (expression, location) ->
      `Assoc
        [
          ("kind", `String "expression");
          ("expression", expression_to_yojson expression);
          ("location", location_to_yojson location);
        ]
  | Echo (expressions, location) ->
      `Assoc
        [
          ("kind", `String "echo");
          ("expressions", `List (List.map expression_to_yojson expressions));
          ("location", location_to_yojson location);
        ]
  | Nop location ->
      `Assoc
        [ ("kind", `String "nop"); ("location", location_to_yojson location) ]

let to_yojson program =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("target_php_version", `String program.target_php_version);
      ("source_file", `String program.source_file);
      ("statements", `List (List.map statement_to_yojson program.statements));
    ]
