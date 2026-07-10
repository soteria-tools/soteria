type position = { line : int; column : int; offset : int }
type location = { file : string; start : position; end_ : position }

type literal =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | String of string

type unary_operator = Boolean_not | Numeric_identity | Numeric_negation

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Concat
  | Identical
  | Not_identical
  | Equal
  | Not_equal
  | Less_than
  | Less_than_or_equal
  | Greater_than
  | Greater_than_or_equal
  | Boolean_and
  | Boolean_or

type cast = To_boolean | To_integer | To_float | To_string

type expression = { desc : expression_desc; location : location }

and expression_desc =
  | Literal of literal
  | Variable of string
  | Assign of string * expression
  | Unary of unary_operator * expression
  | Binary of expression * binary_operator * expression
  | Cast of cast * expression
  | Call of string * expression list

type parameter = { name : string; location : location }

type statement =
  | Expression of expression * location
  | Echo of expression list * location
  | If of expression * statement list * statement list * location
  | While of expression * statement list * location
  | Return of expression option * location
  | Nop of location

type function_decl = {
  name : string;
  parameters : parameter list;
  body : statement list;
  location : location;
}

type t = {
  target_php_version : string;
  source_file : string;
  functions : function_decl list;
  statements : statement list;
}

let schema_version = 3

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

let decode_unary_operator path = function
  | "boolean_not" -> Boolean_not
  | "numeric_identity" -> Numeric_identity
  | "numeric_negation" -> Numeric_negation
  | operator -> decode_error path ("unknown unary operator " ^ operator)

let decode_binary_operator path = function
  | "add" -> Add
  | "subtract" -> Subtract
  | "multiply" -> Multiply
  | "divide" -> Divide
  | "concat" -> Concat
  | "identical" -> Identical
  | "not_identical" -> Not_identical
  | "equal" -> Equal
  | "not_equal" -> Not_equal
  | "less_than" -> Less_than
  | "less_than_or_equal" -> Less_than_or_equal
  | "greater_than" -> Greater_than
  | "greater_than_or_equal" -> Greater_than_or_equal
  | "boolean_and" -> Boolean_and
  | "boolean_or" -> Boolean_or
  | operator -> decode_error path ("unknown binary operator " ^ operator)

let decode_cast path = function
  | "bool" -> To_boolean
  | "int" -> To_integer
  | "float" -> To_float
  | "string" -> To_string
  | cast -> decode_error path ("unknown cast " ^ cast)

let rec decode_expression path json =
  let fields = as_assoc path json in
  let kind = field path "kind" fields |> as_string (path ^ ".kind") in
  let desc =
    match kind with
    | "null" ->
        check_fields path [ "kind"; "location" ] fields;
        Literal Null
    | "bool" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        Literal (Bool (field path "value" fields |> as_bool (path ^ ".value")))
    | "int" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        let value = field path "value" fields |> as_string (path ^ ".value") in
        Literal (Int (decode_int (path ^ ".value") value))
    | "float" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        let value = field path "value" fields |> as_string (path ^ ".value") in
        Literal (Float (decode_float (path ^ ".value") value))
    | "string" ->
        check_fields path [ "kind"; "value"; "location" ] fields;
        Literal
          (String (field path "value" fields |> as_string (path ^ ".value")))
    | "variable" ->
        check_fields path [ "kind"; "name"; "location" ] fields;
        Variable (field path "name" fields |> as_string (path ^ ".name"))
    | "assign" ->
        check_fields path [ "kind"; "variable"; "value"; "location" ] fields;
        let variable =
          field path "variable" fields |> as_string (path ^ ".variable")
        in
        let value =
          field path "value" fields |> decode_expression (path ^ ".value")
        in
        Assign (variable, value)
    | "unary" ->
        check_fields path [ "kind"; "operator"; "operand"; "location" ] fields;
        let operator =
          field path "operator" fields
          |> as_string (path ^ ".operator")
          |> decode_unary_operator (path ^ ".operator")
        in
        let operand =
          field path "operand" fields |> decode_expression (path ^ ".operand")
        in
        Unary (operator, operand)
    | "binary" ->
        check_fields path
          [ "kind"; "operator"; "left"; "right"; "location" ]
          fields;
        let left =
          field path "left" fields |> decode_expression (path ^ ".left")
        in
        let operator =
          field path "operator" fields
          |> as_string (path ^ ".operator")
          |> decode_binary_operator (path ^ ".operator")
        in
        let right =
          field path "right" fields |> decode_expression (path ^ ".right")
        in
        Binary (left, operator, right)
    | "cast" ->
        check_fields path [ "kind"; "type"; "expression"; "location" ] fields;
        let cast =
          field path "type" fields
          |> as_string (path ^ ".type")
          |> decode_cast (path ^ ".type")
        in
        let expression =
          field path "expression" fields
          |> decode_expression (path ^ ".expression")
        in
        Cast (cast, expression)
    | "call" ->
        check_fields path [ "kind"; "name"; "arguments"; "location" ] fields;
        let name = field path "name" fields |> as_string (path ^ ".name") in
        let arguments =
          field path "arguments" fields
          |> as_list (path ^ ".arguments")
          |> List.mapi (fun index ->
              decode_expression (Printf.sprintf "%s.arguments[%d]" path index))
        in
        Call (name, arguments)
    | kind -> decode_error (path ^ ".kind") ("unknown expression kind " ^ kind)
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { desc; location }

and decode_statement ~allow_return path json =
  let fields = as_assoc path json in
  let kind = field path "kind" fields |> as_string (path ^ ".kind") in
  let location () =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  let statements name =
    field path name fields
    |> as_list (path ^ "." ^ name)
    |> List.mapi (fun index ->
        decode_statement ~allow_return
          (Printf.sprintf "%s.%s[%d]" path name index))
  in
  match kind with
  | "expression" ->
      check_fields path [ "kind"; "expression"; "location" ] fields;
      let expression =
        field path "expression" fields
        |> decode_expression (path ^ ".expression")
      in
      Expression (expression, location ())
  | "echo" ->
      check_fields path [ "kind"; "expressions"; "location" ] fields;
      let expressions =
        field path "expressions" fields
        |> as_list (path ^ ".expressions")
        |> List.mapi (fun index ->
            decode_expression (Printf.sprintf "%s.expressions[%d]" path index))
      in
      Echo (expressions, location ())
  | "if" ->
      check_fields path
        [ "kind"; "condition"; "then"; "else"; "location" ]
        fields;
      let condition =
        field path "condition" fields |> decode_expression (path ^ ".condition")
      in
      If (condition, statements "then", statements "else", location ())
  | "while" ->
      check_fields path [ "kind"; "condition"; "body"; "location" ] fields;
      let condition =
        field path "condition" fields |> decode_expression (path ^ ".condition")
      in
      While (condition, statements "body", location ())
  | "return" ->
      check_fields path [ "kind"; "expression"; "location" ] fields;
      if not allow_return then
        decode_error path "return is only valid in a function body";
      let expression =
        match field path "expression" fields with
        | `Null -> None
        | json -> Some (decode_expression (path ^ ".expression") json)
      in
      Return (expression, location ())
  | "nop" ->
      check_fields path [ "kind"; "location" ] fields;
      Nop (location ())
  | kind -> decode_error (path ^ ".kind") ("unknown statement kind " ^ kind)

let decode_parameter path json =
  let fields = as_assoc path json in
  check_fields path [ "name"; "location" ] fields;
  let name = field path "name" fields |> as_string (path ^ ".name") in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { name; location }

let decode_function path json =
  let fields = as_assoc path json in
  check_fields path [ "name"; "parameters"; "body"; "location" ] fields;
  let name = field path "name" fields |> as_string (path ^ ".name") in
  let parameters =
    field path "parameters" fields
    |> as_list (path ^ ".parameters")
    |> List.mapi (fun index ->
        decode_parameter (Printf.sprintf "%s.parameters[%d]" path index))
  in
  let body =
    field path "body" fields
    |> as_list (path ^ ".body")
    |> List.mapi (fun index ->
        decode_statement ~allow_return:true
          (Printf.sprintf "%s.body[%d]" path index))
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { name; parameters; body; location }

let rec iter_expression_locations f (expression : expression) =
  f expression.location;
  match expression.desc with
  | Literal _ | Variable _ -> ()
  | Assign (_, value) | Unary (_, value) | Cast (_, value) ->
      iter_expression_locations f value
  | Binary (left, _, right) ->
      iter_expression_locations f left;
      iter_expression_locations f right
  | Call (_, arguments) -> List.iter (iter_expression_locations f) arguments

let rec iter_statement_locations f (statement : statement) =
  match statement with
  | Expression (expression, location) ->
      f location;
      iter_expression_locations f expression
  | Echo (expressions, location) ->
      f location;
      List.iter (iter_expression_locations f) expressions
  | If (condition, then_, else_, location) ->
      f location;
      iter_expression_locations f condition;
      List.iter (iter_statement_locations f) then_;
      List.iter (iter_statement_locations f) else_
  | While (condition, body, location) ->
      f location;
      iter_expression_locations f condition;
      List.iter (iter_statement_locations f) body
  | Return (expression, location) ->
      f location;
      Option.iter (iter_expression_locations f) expression
  | Nop location -> f location

let iter_function_locations f (function_ : function_decl) =
  f function_.location;
  List.iter
    (fun (parameter : parameter) -> f parameter.location)
    function_.parameters;
  List.iter (iter_statement_locations f) function_.body

let validate_source_file source_file functions statements =
  let validate location =
    if not (String.equal source_file location.file) then
      decode_error "$" "location file differs from source_file"
  in
  List.iter (iter_function_locations validate) functions;
  List.iter (iter_statement_locations validate) statements

let validate_function_names functions =
  let rec validate seen index = function
    | [] -> ()
    | (function_ : function_decl) :: functions ->
        let name = String.lowercase_ascii function_.name in
        if List.mem name seen then
          decode_error
            (Printf.sprintf "$.functions[%d].name" index)
            ("duplicate function " ^ function_.name);
        let rec validate_parameters seen parameter_index = function
          | [] -> ()
          | (parameter : parameter) :: parameters ->
              if List.mem parameter.name seen then
                decode_error
                  (Printf.sprintf "$.functions[%d].parameters[%d].name" index
                     parameter_index)
                  ("duplicate parameter " ^ parameter.name);
              validate_parameters (parameter.name :: seen) (parameter_index + 1)
                parameters
        in
        validate_parameters [] 0 function_.parameters;
        validate (name :: seen) (index + 1) functions
  in
  validate [] 0 functions

let of_yojson json =
  try
    let fields = as_assoc "$" json in
    check_fields "$"
      [
        "schema_version";
        "target_php_version";
        "source_file";
        "functions";
        "statements";
      ]
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
    let functions =
      field "$" "functions" fields
      |> as_list "$.functions"
      |> List.mapi (fun index ->
          decode_function (Printf.sprintf "$.functions[%d]" index))
    in
    let statements =
      field "$" "statements" fields
      |> as_list "$.statements"
      |> List.mapi (fun index ->
          decode_statement ~allow_return:false
            (Printf.sprintf "$.statements[%d]" index))
    in
    validate_function_names functions;
    validate_source_file source_file functions statements;
    Ok
      {
        target_php_version = actual_php_version;
        source_file;
        functions;
        statements;
      }
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

let unary_operator_to_string = function
  | Boolean_not -> "boolean_not"
  | Numeric_identity -> "numeric_identity"
  | Numeric_negation -> "numeric_negation"

let binary_operator_to_string = function
  | Add -> "add"
  | Subtract -> "subtract"
  | Multiply -> "multiply"
  | Divide -> "divide"
  | Concat -> "concat"
  | Identical -> "identical"
  | Not_identical -> "not_identical"
  | Equal -> "equal"
  | Not_equal -> "not_equal"
  | Less_than -> "less_than"
  | Less_than_or_equal -> "less_than_or_equal"
  | Greater_than -> "greater_than"
  | Greater_than_or_equal -> "greater_than_or_equal"
  | Boolean_and -> "boolean_and"
  | Boolean_or -> "boolean_or"

let cast_to_string = function
  | To_boolean -> "bool"
  | To_integer -> "int"
  | To_float -> "float"
  | To_string -> "string"

let rec expression_to_yojson expression =
  let fields =
    match expression.desc with
    | Literal Null -> [ ("kind", `String "null") ]
    | Literal (Bool value) ->
        [ ("kind", `String "bool"); ("value", `Bool value) ]
    | Literal (Int value) ->
        [ ("kind", `String "int"); ("value", `String (Int64.to_string value)) ]
    | Literal (Float value) ->
        [
          ("kind", `String "float");
          ("value", `String (Printf.sprintf "%.17g" value));
        ]
    | Literal (String value) ->
        [ ("kind", `String "string"); ("value", `String value) ]
    | Variable name -> [ ("kind", `String "variable"); ("name", `String name) ]
    | Assign (variable, value) ->
        [
          ("kind", `String "assign");
          ("variable", `String variable);
          ("value", expression_to_yojson value);
        ]
    | Unary (operator, operand) ->
        [
          ("kind", `String "unary");
          ("operator", `String (unary_operator_to_string operator));
          ("operand", expression_to_yojson operand);
        ]
    | Binary (left, operator, right) ->
        [
          ("kind", `String "binary");
          ("operator", `String (binary_operator_to_string operator));
          ("left", expression_to_yojson left);
          ("right", expression_to_yojson right);
        ]
    | Cast (cast, expression) ->
        [
          ("kind", `String "cast");
          ("type", `String (cast_to_string cast));
          ("expression", expression_to_yojson expression);
        ]
    | Call (name, arguments) ->
        [
          ("kind", `String "call");
          ("name", `String name);
          ("arguments", `List (List.map expression_to_yojson arguments));
        ]
  in
  `Assoc (fields @ [ ("location", location_to_yojson expression.location) ])

let rec statement_to_yojson = function
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
  | If (condition, then_, else_, location) ->
      `Assoc
        [
          ("kind", `String "if");
          ("condition", expression_to_yojson condition);
          ("then", `List (List.map statement_to_yojson then_));
          ("else", `List (List.map statement_to_yojson else_));
          ("location", location_to_yojson location);
        ]
  | While (condition, body, location) ->
      `Assoc
        [
          ("kind", `String "while");
          ("condition", expression_to_yojson condition);
          ("body", `List (List.map statement_to_yojson body));
          ("location", location_to_yojson location);
        ]
  | Return (expression, location) ->
      `Assoc
        [
          ("kind", `String "return");
          ( "expression",
            Option.fold ~none:`Null ~some:expression_to_yojson expression );
          ("location", location_to_yojson location);
        ]
  | Nop location ->
      `Assoc
        [ ("kind", `String "nop"); ("location", location_to_yojson location) ]

let parameter_to_yojson (parameter : parameter) =
  `Assoc
    [
      ("name", `String parameter.name);
      ("location", location_to_yojson parameter.location);
    ]

let function_to_yojson (function_ : function_decl) =
  `Assoc
    [
      ("name", `String function_.name);
      ("parameters", `List (List.map parameter_to_yojson function_.parameters));
      ("body", `List (List.map statement_to_yojson function_.body));
      ("location", location_to_yojson function_.location);
    ]

let to_yojson program =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("target_php_version", `String program.target_php_version);
      ("source_file", `String program.source_file);
      ("functions", `List (List.map function_to_yojson program.functions));
      ("statements", `List (List.map statement_to_yojson program.statements));
    ]
