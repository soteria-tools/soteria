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
  | Array of array_item list
  | Array_get of lvalue
  | Property_get of lvalue
  | Assign of lvalue * expression
  | Assign_reference of lvalue * lvalue
  | Unary of unary_operator * expression
  | Binary of expression * binary_operator * expression
  | Cast of cast * expression
  | Call of string * expression list
  | Invoke of expression * expression list
  | Function_callable of string
  | Method_call of expression * string * expression list
  | Object_method_callable of expression * string
  | Static_method_call of string * string * expression list
  | Static_method_callable of string * string
  | Parent_method_call of string * expression list
  | Closure of closure_decl
  | New of string * expression list
  | Throw of expression

and array_item = {
  key : expression option;
  value : expression;
  location : location;
}

and lvalue = { desc : lvalue_desc; location : location }

and lvalue_desc =
  | Variable_lvalue of string
  | Array_element_lvalue of lvalue * expression option
  | Object_property_lvalue of lvalue * string
  | Static_property_lvalue of string * string

and parameter = { name : string; location : location }

and closure_capture = {
  name : string;
  by_reference : bool;
  location : location;
}

and closure_decl = {
  parameters : parameter list;
  captures : closure_capture list;
  body : statement list;
  location : location;
}

and catch_clause = {
  types : string list;
  variable : string option;
  body : statement list;
  location : location;
}

and statement =
  | Expression of expression * location
  | Echo of expression list * location
  | If of expression * statement list * statement list * location
  | While of expression * statement list * location
  | Foreach of
      expression * lvalue option * lvalue * bool * statement list * location
  | Break of int * location
  | Continue of int * location
  | Return of expression option * location
  | Try of statement list * catch_clause list * statement list option * location
  | Unset of lvalue list * location
  | Nop of location

type function_decl = {
  name : string;
  parameters : parameter list;
  body : statement list;
  location : location;
}

type member_modifier = Public | Protected | Private | Static

type property_decl = {
  name : string;
  default : expression option;
  modifiers : member_modifier list;
  location : location;
}

type method_decl = {
  name : string;
  parameters : parameter list;
  body : statement list option;
  modifiers : member_modifier list;
  location : location;
}

type declaration_kind = Class | Interface | Trait

type trait_adaptation =
  | Trait_precedence of {
      trait : string;
      method_name : string;
      instead_of : string list;
      location : location;
    }
  | Trait_alias of {
      trait : string option;
      method_name : string;
      alias : string option;
      visibility : member_modifier option;
      location : location;
    }

type trait_use = {
  traits : string list;
  adaptations : trait_adaptation list;
  location : location;
}

type class_decl = {
  kind : declaration_kind;
  name : string;
  parent : string option;
  interfaces : string list;
  traits : trait_use list;
  properties : property_decl list;
  methods : method_decl list;
  location : location;
}

type t = {
  target_php_version : string;
  source_file : string;
  functions : function_decl list;
  classes : class_decl list;
  statements : statement list;
}

let schema_version = 13

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
    | "array" ->
        check_fields path [ "kind"; "items"; "location" ] fields;
        let items =
          field path "items" fields
          |> as_list (path ^ ".items")
          |> List.mapi (fun index ->
              decode_array_item (Printf.sprintf "%s.items[%d]" path index))
        in
        Array items
    | "array_get" ->
        check_fields path [ "kind"; "target"; "location" ] fields;
        let target =
          field path "target" fields
          |> decode_lvalue ~allow_append:false (path ^ ".target")
        in
        Array_get target
    | "property_get" -> (
        check_fields path [ "kind"; "target"; "location" ] fields;
        let target =
          field path "target" fields
          |> decode_lvalue ~allow_append:false (path ^ ".target")
        in
        match target.desc with
        | Object_property_lvalue _ | Static_property_lvalue _ ->
            Property_get target
        | _ -> decode_error (path ^ ".target") "expected a property")
    | "assign" ->
        check_fields path [ "kind"; "target"; "value"; "location" ] fields;
        let target =
          field path "target" fields
          |> decode_lvalue ~allow_append:true (path ^ ".target")
        in
        let value =
          field path "value" fields |> decode_expression (path ^ ".value")
        in
        Assign (target, value)
    | "assign_reference" ->
        check_fields path [ "kind"; "target"; "source"; "location" ] fields;
        let target =
          field path "target" fields
          |> decode_lvalue ~allow_append:true (path ^ ".target")
        in
        let source =
          field path "source" fields
          |> decode_lvalue ~allow_append:true (path ^ ".source")
        in
        Assign_reference (target, source)
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
    | "invoke" ->
        check_fields path [ "kind"; "callee"; "arguments"; "location" ] fields;
        let callee =
          field path "callee" fields |> decode_expression (path ^ ".callee")
        in
        let arguments =
          field path "arguments" fields
          |> as_list (path ^ ".arguments")
          |> List.mapi (fun index ->
              decode_expression (Printf.sprintf "%s.arguments[%d]" path index))
        in
        Invoke (callee, arguments)
    | "function_callable" ->
        check_fields path [ "kind"; "name"; "location" ] fields;
        Function_callable
          (field path "name" fields |> as_string (path ^ ".name"))
    | "method_call" ->
        check_fields path
          [ "kind"; "object"; "method"; "arguments"; "location" ]
          fields;
        let object_ =
          field path "object" fields |> decode_expression (path ^ ".object")
        in
        let method_name =
          field path "method" fields |> as_string (path ^ ".method")
        in
        let arguments =
          field path "arguments" fields
          |> as_list (path ^ ".arguments")
          |> List.mapi (fun index ->
              decode_expression (Printf.sprintf "%s.arguments[%d]" path index))
        in
        Method_call (object_, method_name, arguments)
    | "object_method_callable" ->
        check_fields path [ "kind"; "object"; "method"; "location" ] fields;
        let object_ =
          field path "object" fields |> decode_expression (path ^ ".object")
        in
        let method_name =
          field path "method" fields |> as_string (path ^ ".method")
        in
        Object_method_callable (object_, method_name)
    | "static_method_call" ->
        check_fields path
          [ "kind"; "class"; "method"; "arguments"; "location" ]
          fields;
        let class_name =
          field path "class" fields |> as_string (path ^ ".class")
        in
        let method_name =
          field path "method" fields |> as_string (path ^ ".method")
        in
        let arguments =
          field path "arguments" fields
          |> as_list (path ^ ".arguments")
          |> List.mapi (fun index ->
              decode_expression (Printf.sprintf "%s.arguments[%d]" path index))
        in
        Static_method_call (class_name, method_name, arguments)
    | "static_method_callable" ->
        check_fields path [ "kind"; "class"; "method"; "location" ] fields;
        let class_name =
          field path "class" fields |> as_string (path ^ ".class")
        in
        let method_name =
          field path "method" fields |> as_string (path ^ ".method")
        in
        Static_method_callable (class_name, method_name)
    | "parent_method_call" ->
        check_fields path [ "kind"; "method"; "arguments"; "location" ] fields;
        let method_name =
          field path "method" fields |> as_string (path ^ ".method")
        in
        let arguments =
          field path "arguments" fields
          |> as_list (path ^ ".arguments")
          |> List.mapi (fun index ->
              decode_expression (Printf.sprintf "%s.arguments[%d]" path index))
        in
        Parent_method_call (method_name, arguments)
    | "closure" ->
        check_fields path
          [ "kind"; "parameters"; "captures"; "body"; "location" ]
          fields;
        let parameters =
          field path "parameters" fields
          |> as_list (path ^ ".parameters")
          |> List.mapi (fun index ->
              decode_parameter (Printf.sprintf "%s.parameters[%d]" path index))
        in
        let captures =
          field path "captures" fields
          |> as_list (path ^ ".captures")
          |> List.mapi (fun index ->
              decode_closure_capture
                (Printf.sprintf "%s.captures[%d]" path index))
        in
        let rec validate_parameters seen index = function
          | [] -> seen
          | (parameter : parameter) :: parameters ->
              if String.equal parameter.name "this" then
                decode_error
                  (Printf.sprintf "%s.parameters[%d].name" path index)
                  "closure parameter cannot be named this";
              if List.mem parameter.name seen then
                decode_error
                  (Printf.sprintf "%s.parameters[%d].name" path index)
                  ("duplicate parameter " ^ parameter.name);
              validate_parameters (parameter.name :: seen) (index + 1)
                parameters
        in
        let parameter_names = validate_parameters [] 0 parameters in
        let rec validate_captures seen index = function
          | [] -> ()
          | (capture : closure_capture) :: captures ->
              if String.equal capture.name "this" then
                decode_error
                  (Printf.sprintf "%s.captures[%d].name" path index)
                  "closure cannot explicitly capture this";
              if List.mem capture.name parameter_names then
                decode_error
                  (Printf.sprintf "%s.captures[%d].name" path index)
                  ("capture duplicates parameter " ^ capture.name);
              if List.mem capture.name seen then
                decode_error
                  (Printf.sprintf "%s.captures[%d].name" path index)
                  ("duplicate capture " ^ capture.name);
              validate_captures (capture.name :: seen) (index + 1) captures
        in
        validate_captures [] 0 captures;
        let body =
          field path "body" fields
          |> as_list (path ^ ".body")
          |> List.mapi (fun index ->
              decode_statement ~allow_return:true ~loop_depth:0
                (Printf.sprintf "%s.body[%d]" path index))
        in
        let location =
          field path "location" fields |> decode_location (path ^ ".location")
        in
        Closure { parameters; captures; body; location }
    | "new" ->
        check_fields path [ "kind"; "class"; "arguments"; "location" ] fields;
        let class_name =
          field path "class" fields |> as_string (path ^ ".class")
        in
        let arguments =
          field path "arguments" fields
          |> as_list (path ^ ".arguments")
          |> List.mapi (fun index ->
              decode_expression (Printf.sprintf "%s.arguments[%d]" path index))
        in
        New (class_name, arguments)
    | "throw" ->
        check_fields path [ "kind"; "expression"; "location" ] fields;
        let expression =
          field path "expression" fields
          |> decode_expression (path ^ ".expression")
        in
        Throw expression
    | kind -> decode_error (path ^ ".kind") ("unknown expression kind " ^ kind)
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { desc; location }

and decode_array_item path json =
  let fields = as_assoc path json in
  check_fields path [ "key"; "value"; "location" ] fields;
  let key =
    match field path "key" fields with
    | `Null -> None
    | json -> Some (decode_expression (path ^ ".key") json)
  in
  let value =
    field path "value" fields |> decode_expression (path ^ ".value")
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { key; value; location }

and decode_lvalue ~allow_append path json =
  let fields = as_assoc path json in
  let kind = field path "kind" fields |> as_string (path ^ ".kind") in
  let desc =
    match kind with
    | "variable" ->
        check_fields path [ "kind"; "name"; "location" ] fields;
        Variable_lvalue (field path "name" fields |> as_string (path ^ ".name"))
    | "array_element" ->
        check_fields path [ "kind"; "array"; "key"; "location" ] fields;
        let array =
          field path "array" fields
          |> decode_lvalue ~allow_append (path ^ ".array")
        in
        let key =
          match field path "key" fields with
          | `Null when allow_append -> None
          | `Null -> decode_error (path ^ ".key") "append cannot be read"
          | json -> Some (decode_expression (path ^ ".key") json)
        in
        Array_element_lvalue (array, key)
    | "object_property" ->
        check_fields path [ "kind"; "object"; "name"; "location" ] fields;
        let object_ =
          field path "object" fields
          |> decode_lvalue ~allow_append (path ^ ".object")
        in
        let name = field path "name" fields |> as_string (path ^ ".name") in
        Object_property_lvalue (object_, name)
    | "static_property" ->
        check_fields path [ "kind"; "class"; "name"; "location" ] fields;
        let class_name =
          field path "class" fields |> as_string (path ^ ".class")
        in
        let name = field path "name" fields |> as_string (path ^ ".name") in
        Static_property_lvalue (class_name, name)
    | kind -> decode_error (path ^ ".kind") ("unknown lvalue kind " ^ kind)
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { desc; location }

and decode_parameter path json =
  let fields = as_assoc path json in
  check_fields path [ "name"; "location" ] fields;
  let name = field path "name" fields |> as_string (path ^ ".name") in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { name; location }

and decode_closure_capture path json =
  let fields = as_assoc path json in
  check_fields path [ "name"; "by_reference"; "location" ] fields;
  let name = field path "name" fields |> as_string (path ^ ".name") in
  let by_reference =
    field path "by_reference" fields |> as_bool (path ^ ".by_reference")
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { name; by_reference; location }

and decode_statement ~allow_return ~loop_depth path json =
  let fields = as_assoc path json in
  let kind = field path "kind" fields |> as_string (path ^ ".kind") in
  let location () =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  let statements ?(depth = loop_depth) name =
    field path name fields
    |> as_list (path ^ "." ^ name)
    |> List.mapi (fun index ->
        decode_statement ~allow_return ~loop_depth:depth
          (Printf.sprintf "%s.%s[%d]" path name index))
  in
  let loop_control kind constructor =
    check_fields path [ "kind"; "depth"; "location" ] fields;
    let depth = field path "depth" fields |> as_int (path ^ ".depth") in
    if depth < 1 then decode_error (path ^ ".depth") "must be positive";
    if depth > loop_depth then
      decode_error (path ^ ".depth")
        (Printf.sprintf "%s depth %d exceeds enclosing loop depth %d" kind depth
           loop_depth);
    constructor (depth, location ())
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
      While (condition, statements ~depth:(loop_depth + 1) "body", location ())
  | "foreach" ->
      check_fields path
        [
          "kind"; "iterable"; "key"; "value"; "by_reference"; "body"; "location";
        ]
        fields;
      let iterable =
        field path "iterable" fields |> decode_expression (path ^ ".iterable")
      in
      let key =
        match field path "key" fields with
        | `Null -> None
        | json -> Some (decode_lvalue ~allow_append:true (path ^ ".key") json)
      in
      let value =
        field path "value" fields
        |> decode_lvalue ~allow_append:true (path ^ ".value")
      in
      let by_reference =
        field path "by_reference" fields |> as_bool (path ^ ".by_reference")
      in
      Foreach
        ( iterable,
          key,
          value,
          by_reference,
          statements ~depth:(loop_depth + 1) "body",
          location () )
  | "break" ->
      loop_control "break" (fun (depth, location) -> Break (depth, location))
  | "continue" ->
      loop_control "continue" (fun (depth, location) ->
          Continue (depth, location))
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
  | "try" ->
      check_fields path
        [ "kind"; "body"; "catches"; "finally"; "location" ]
        fields;
      let catches =
        field path "catches" fields
        |> as_list (path ^ ".catches")
        |> List.mapi (fun index ->
            decode_catch_clause ~allow_return ~loop_depth
              (Printf.sprintf "%s.catches[%d]" path index))
      in
      let finally =
        match field path "finally" fields with
        | `Null -> None
        | json ->
            Some
              (json
              |> as_list (path ^ ".finally")
              |> List.mapi (fun index ->
                  decode_statement ~allow_return ~loop_depth
                    (Printf.sprintf "%s.finally[%d]" path index)))
      in
      if catches = [] && Option.is_none finally then
        decode_error path "try must have a catch or finally block";
      Try (statements "body", catches, finally, location ())
  | "unset" ->
      check_fields path [ "kind"; "targets"; "location" ] fields;
      let targets =
        field path "targets" fields
        |> as_list (path ^ ".targets")
        |> List.mapi (fun index ->
            decode_lvalue ~allow_append:false
              (Printf.sprintf "%s.targets[%d]" path index))
      in
      if targets = [] then decode_error (path ^ ".targets") "must not be empty";
      Unset (targets, location ())
  | "nop" ->
      check_fields path [ "kind"; "location" ] fields;
      Nop (location ())
  | kind -> decode_error (path ^ ".kind") ("unknown statement kind " ^ kind)

and decode_catch_clause ~allow_return ~loop_depth path json =
  let fields = as_assoc path json in
  check_fields path [ "types"; "variable"; "body"; "location" ] fields;
  let types =
    field path "types" fields
    |> as_list (path ^ ".types")
    |> List.mapi (fun index value ->
        as_string (Printf.sprintf "%s.types[%d]" path index) value)
  in
  if types = [] then decode_error (path ^ ".types") "must not be empty";
  let variable =
    match field path "variable" fields with
    | `Null -> None
    | json -> Some (as_string (path ^ ".variable") json)
  in
  let body =
    field path "body" fields
    |> as_list (path ^ ".body")
    |> List.mapi (fun index ->
        decode_statement ~allow_return ~loop_depth
          (Printf.sprintf "%s.body[%d]" path index))
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { types; variable; body; location }

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
        decode_statement ~allow_return:true ~loop_depth:0
          (Printf.sprintf "%s.body[%d]" path index))
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { name; parameters; body; location }

let rec validate_property_default path (expression : expression) =
  let rec numeric = function
    | { desc = Literal (Int _ | Float _); _ } -> true
    | { desc = Unary ((Numeric_identity | Numeric_negation), operand); _ } ->
        numeric operand
    | _ -> false
  in
  match expression.desc with
  | Literal _ -> ()
  | Unary ((Numeric_identity | Numeric_negation), _) when numeric expression ->
      ()
  | Array items ->
      List.iteri
        (fun index (item : array_item) ->
          let path = Printf.sprintf "%s.items[%d]" path index in
          Option.iter (validate_property_default (path ^ ".key")) item.key;
          validate_property_default (path ^ ".value") item.value)
        items
  | _ -> decode_error path "unsupported property default expression"

let decode_member_modifier path = function
  | "public" -> Public
  | "protected" -> Protected
  | "private" -> Private
  | "static" -> Static
  | modifier -> decode_error path ("unknown member modifier " ^ modifier)

let validate_visibility path = function
  | [ (Public | Protected | Private) ]
  | [ (Public | Protected | Private); Static ] ->
      ()
  | _ ->
      decode_error (path ^ ".modifiers")
        "expected one visibility modifier followed by optional static"

let decode_property path json =
  let fields = as_assoc path json in
  check_fields path [ "name"; "default"; "modifiers"; "location" ] fields;
  let name = field path "name" fields |> as_string (path ^ ".name") in
  let default =
    match field path "default" fields with
    | `Null -> None
    | json ->
        let expression = decode_expression (path ^ ".default") json in
        validate_property_default (path ^ ".default") expression;
        Some expression
  in
  let modifiers =
    field path "modifiers" fields
    |> as_list (path ^ ".modifiers")
    |> List.mapi (fun index json ->
        let path = Printf.sprintf "%s.modifiers[%d]" path index in
        json |> as_string path |> decode_member_modifier path)
  in
  validate_visibility path modifiers;
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { name; default; modifiers; location }

let decode_method path json =
  let fields = as_assoc path json in
  check_fields path
    [ "name"; "parameters"; "body"; "modifiers"; "location" ]
    fields;
  let name = field path "name" fields |> as_string (path ^ ".name") in
  let parameters =
    field path "parameters" fields
    |> as_list (path ^ ".parameters")
    |> List.mapi (fun index ->
        decode_parameter (Printf.sprintf "%s.parameters[%d]" path index))
  in
  let body =
    match field path "body" fields with
    | `Null -> None
    | json ->
        Some
          (json
          |> as_list (path ^ ".body")
          |> List.mapi (fun index ->
              decode_statement ~allow_return:true ~loop_depth:0
                (Printf.sprintf "%s.body[%d]" path index)))
  in
  let modifiers =
    field path "modifiers" fields
    |> as_list (path ^ ".modifiers")
    |> List.mapi (fun index json ->
        let path = Printf.sprintf "%s.modifiers[%d]" path index in
        json |> as_string path |> decode_member_modifier path)
  in
  validate_visibility path modifiers;
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { name; parameters; body; modifiers; location }

let decode_declaration_kind path = function
  | "class" -> Class
  | "interface" -> Interface
  | "trait" -> Trait
  | kind -> decode_error path ("unknown declaration kind " ^ kind)

let decode_optional_string path = function
  | `Null -> None
  | json -> Some (as_string path json)

let decode_trait_adaptation path json =
  let fields = as_assoc path json in
  let kind = field path "kind" fields |> as_string (path ^ ".kind") in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  match kind with
  | "precedence" ->
      check_fields path
        [ "kind"; "trait"; "method"; "instead_of"; "location" ]
        fields;
      let trait = field path "trait" fields |> as_string (path ^ ".trait") in
      let method_name =
        field path "method" fields |> as_string (path ^ ".method")
      in
      let instead_of =
        field path "instead_of" fields
        |> as_list (path ^ ".instead_of")
        |> List.mapi (fun index ->
            as_string (Printf.sprintf "%s.instead_of[%d]" path index))
      in
      Trait_precedence { trait; method_name; instead_of; location }
  | "alias" ->
      check_fields path
        [ "kind"; "trait"; "method"; "alias"; "visibility"; "location" ]
        fields;
      let trait =
        field path "trait" fields |> decode_optional_string (path ^ ".trait")
      in
      let method_name =
        field path "method" fields |> as_string (path ^ ".method")
      in
      let alias =
        field path "alias" fields |> decode_optional_string (path ^ ".alias")
      in
      let visibility =
        match field path "visibility" fields with
        | `Null -> None
        | json ->
            Some
              (json
              |> as_string (path ^ ".visibility")
              |> decode_member_modifier (path ^ ".visibility"))
      in
      if Option.is_none alias && Option.is_none visibility then
        decode_error path "trait alias must change its name or visibility";
      Trait_alias { trait; method_name; alias; visibility; location }
  | kind -> decode_error (path ^ ".kind") ("unknown trait adaptation " ^ kind)

let decode_trait_use path json =
  let fields = as_assoc path json in
  check_fields path [ "traits"; "adaptations"; "location" ] fields;
  let traits =
    field path "traits" fields
    |> as_list (path ^ ".traits")
    |> List.mapi (fun index ->
        as_string (Printf.sprintf "%s.traits[%d]" path index))
  in
  let adaptations =
    field path "adaptations" fields
    |> as_list (path ^ ".adaptations")
    |> List.mapi (fun index ->
        decode_trait_adaptation (Printf.sprintf "%s.adaptations[%d]" path index))
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { traits; adaptations; location }

let decode_class path json =
  let fields = as_assoc path json in
  check_fields path
    [
      "kind";
      "name";
      "parent";
      "interfaces";
      "traits";
      "properties";
      "methods";
      "location";
    ]
    fields;
  let kind =
    field path "kind" fields
    |> as_string (path ^ ".kind")
    |> decode_declaration_kind (path ^ ".kind")
  in
  let name = field path "name" fields |> as_string (path ^ ".name") in
  let parent =
    field path "parent" fields |> decode_optional_string (path ^ ".parent")
  in
  let interfaces =
    field path "interfaces" fields
    |> as_list (path ^ ".interfaces")
    |> List.mapi (fun index ->
        as_string (Printf.sprintf "%s.interfaces[%d]" path index))
  in
  let traits =
    field path "traits" fields
    |> as_list (path ^ ".traits")
    |> List.mapi (fun index ->
        decode_trait_use (Printf.sprintf "%s.traits[%d]" path index))
  in
  let properties =
    field path "properties" fields
    |> as_list (path ^ ".properties")
    |> List.mapi (fun index ->
        decode_property (Printf.sprintf "%s.properties[%d]" path index))
  in
  let methods =
    field path "methods" fields
    |> as_list (path ^ ".methods")
    |> List.mapi (fun index ->
        decode_method (Printf.sprintf "%s.methods[%d]" path index))
  in
  let location =
    field path "location" fields |> decode_location (path ^ ".location")
  in
  { kind; name; parent; interfaces; traits; properties; methods; location }

let rec iter_expression_locations f (expression : expression) =
  f expression.location;
  match expression.desc with
  | Literal _ | Variable _ | Function_callable _ | Static_method_callable _ ->
      ()
  | Array items -> List.iter (iter_array_item_locations f) items
  | Array_get target | Property_get target -> iter_lvalue_locations f target
  | Assign (target, value) ->
      iter_lvalue_locations f target;
      iter_expression_locations f value
  | Assign_reference (target, source) ->
      iter_lvalue_locations f target;
      iter_lvalue_locations f source
  | Unary (_, value) | Cast (_, value) -> iter_expression_locations f value
  | Binary (left, _, right) ->
      iter_expression_locations f left;
      iter_expression_locations f right
  | Call (_, arguments)
  | New (_, arguments)
  | Parent_method_call (_, arguments)
  | Static_method_call (_, _, arguments) ->
      List.iter (iter_expression_locations f) arguments
  | Invoke (callee, arguments) ->
      iter_expression_locations f callee;
      List.iter (iter_expression_locations f) arguments
  | Method_call (object_, _, arguments) ->
      iter_expression_locations f object_;
      List.iter (iter_expression_locations f) arguments
  | Object_method_callable (object_, _) -> iter_expression_locations f object_
  | Closure closure ->
      f closure.location;
      List.iter
        (fun (parameter : parameter) -> f parameter.location)
        closure.parameters;
      List.iter
        (fun (capture : closure_capture) -> f capture.location)
        closure.captures;
      List.iter (iter_statement_locations f) closure.body
  | Throw expression -> iter_expression_locations f expression

and iter_array_item_locations f (item : array_item) =
  f item.location;
  Option.iter (iter_expression_locations f) item.key;
  iter_expression_locations f item.value

and iter_lvalue_locations f (lvalue : lvalue) =
  f lvalue.location;
  match lvalue.desc with
  | Variable_lvalue _ | Static_property_lvalue _ -> ()
  | Array_element_lvalue (array, key) ->
      iter_lvalue_locations f array;
      Option.iter (iter_expression_locations f) key
  | Object_property_lvalue (object_, _) -> iter_lvalue_locations f object_

and iter_statement_locations f (statement : statement) =
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
  | Foreach (iterable, key, value, _, body, location) ->
      f location;
      iter_expression_locations f iterable;
      Option.iter (iter_lvalue_locations f) key;
      iter_lvalue_locations f value;
      List.iter (iter_statement_locations f) body
  | Break (_, location) | Continue (_, location) -> f location
  | Return (expression, location) ->
      f location;
      Option.iter (iter_expression_locations f) expression
  | Try (body, catches, finally, location) ->
      f location;
      List.iter (iter_statement_locations f) body;
      List.iter (iter_catch_clause_locations f) catches;
      Option.iter (List.iter (iter_statement_locations f)) finally
  | Unset (targets, location) ->
      f location;
      List.iter (iter_lvalue_locations f) targets
  | Nop location -> f location

and iter_catch_clause_locations f (catch : catch_clause) =
  f catch.location;
  List.iter (iter_statement_locations f) catch.body

let iter_function_locations f (function_ : function_decl) =
  f function_.location;
  List.iter
    (fun (parameter : parameter) -> f parameter.location)
    function_.parameters;
  List.iter (iter_statement_locations f) function_.body

let iter_class_locations f (class_ : class_decl) =
  f class_.location;
  List.iter
    (fun (trait_use : trait_use) ->
      f trait_use.location;
      List.iter
        (function
          | Trait_precedence { location; _ } | Trait_alias { location; _ } ->
              f location)
        trait_use.adaptations)
    class_.traits;
  List.iter
    (fun (property : property_decl) ->
      f property.location;
      Option.iter (iter_expression_locations f) property.default)
    class_.properties;
  List.iter
    (fun (method_ : method_decl) ->
      f method_.location;
      List.iter
        (fun (parameter : parameter) -> f parameter.location)
        method_.parameters;
      Option.iter (List.iter (iter_statement_locations f)) method_.body)
    class_.methods

let validate_source_file source_file functions classes statements =
  let validate location =
    if not (String.equal source_file location.file) then
      decode_error "$" "location file differs from source_file"
  in
  List.iter (iter_function_locations validate) functions;
  List.iter (iter_class_locations validate) classes;
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

let validate_class_names classes =
  let rec validate seen index = function
    | [] -> ()
    | (class_ : class_decl) :: classes ->
        let name = String.lowercase_ascii class_.name in
        if List.mem name seen then
          decode_error
            (Printf.sprintf "$.classes[%d].name" index)
            ("duplicate class " ^ class_.name);
        let rec validate_properties seen property_index = function
          | [] -> ()
          | (property : property_decl) :: properties ->
              if List.mem property.name seen then
                decode_error
                  (Printf.sprintf "$.classes[%d].properties[%d].name" index
                     property_index)
                  ("duplicate property " ^ property.name);
              validate_properties (property.name :: seen) (property_index + 1)
                properties
        in
        validate_properties [] 0 class_.properties;
        let rec validate_methods seen method_index = function
          | [] -> ()
          | (method_ : method_decl) :: methods ->
              let method_name = String.lowercase_ascii method_.name in
              if List.mem method_name seen then
                decode_error
                  (Printf.sprintf "$.classes[%d].methods[%d].name" index
                     method_index)
                  ("duplicate method " ^ method_.name);
              let rec validate_parameters seen parameter_index = function
                | [] -> ()
                | (parameter : parameter) :: parameters ->
                    if String.equal parameter.name "this" then
                      decode_error
                        (Printf.sprintf
                           "$.classes[%d].methods[%d].parameters[%d].name" index
                           method_index parameter_index)
                        "method parameter cannot be named this";
                    if List.mem parameter.name seen then
                      decode_error
                        (Printf.sprintf
                           "$.classes[%d].methods[%d].parameters[%d].name" index
                           method_index parameter_index)
                        ("duplicate parameter " ^ parameter.name);
                    validate_parameters (parameter.name :: seen)
                      (parameter_index + 1) parameters
              in
              validate_parameters [] 0 method_.parameters;
              validate_methods (method_name :: seen) (method_index + 1) methods
        in
        validate_methods [] 0 class_.methods;
        (match class_.kind with
        | Class ->
            if
              List.exists
                (fun method_ -> Option.is_none method_.body)
                class_.methods
            then
              decode_error
                (Printf.sprintf "$.classes[%d].methods" index)
                "class methods must have bodies"
        | Interface ->
            if Option.is_some class_.parent then
              decode_error
                (Printf.sprintf "$.classes[%d].parent" index)
                "interfaces cannot have a class parent";
            if class_.properties <> [] || class_.traits <> [] then
              decode_error
                (Printf.sprintf "$.classes[%d]" index)
                "interfaces cannot contain properties or trait uses";
            if
              List.exists
                (fun method_ ->
                  Option.is_some method_.body
                  || not
                       (method_.modifiers = [ Public ]
                       || method_.modifiers = [ Public; Static ]))
                class_.methods
            then
              decode_error
                (Printf.sprintf "$.classes[%d].methods" index)
                "interface methods must be public declarations without bodies"
        | Trait ->
            if Option.is_some class_.parent || class_.interfaces <> [] then
              decode_error
                (Printf.sprintf "$.classes[%d]" index)
                "traits cannot extend classes or implement interfaces";
            if
              List.exists
                (fun method_ -> Option.is_none method_.body)
                class_.methods
            then
              decode_error
                (Printf.sprintf "$.classes[%d].methods" index)
                "trait methods must have bodies");
        validate (name :: seen) (index + 1) classes
  in
  validate [] 0 classes

let of_yojson json =
  try
    let fields = as_assoc "$" json in
    check_fields "$"
      [
        "schema_version";
        "target_php_version";
        "source_file";
        "functions";
        "classes";
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
    let classes =
      field "$" "classes" fields
      |> as_list "$.classes"
      |> List.mapi (fun index ->
          decode_class (Printf.sprintf "$.classes[%d]" index))
    in
    let statements =
      field "$" "statements" fields
      |> as_list "$.statements"
      |> List.mapi (fun index ->
          decode_statement ~allow_return:false ~loop_depth:0
            (Printf.sprintf "$.statements[%d]" index))
    in
    validate_function_names functions;
    validate_class_names classes;
    validate_source_file source_file functions classes statements;
    Ok
      {
        target_php_version = actual_php_version;
        source_file;
        functions;
        classes;
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
    | Array items ->
        [
          ("kind", `String "array");
          ("items", `List (List.map array_item_to_yojson items));
        ]
    | Array_get target ->
        [ ("kind", `String "array_get"); ("target", lvalue_to_yojson target) ]
    | Property_get target ->
        [
          ("kind", `String "property_get"); ("target", lvalue_to_yojson target);
        ]
    | Assign (target, value) ->
        [
          ("kind", `String "assign");
          ("target", lvalue_to_yojson target);
          ("value", expression_to_yojson value);
        ]
    | Assign_reference (target, source) ->
        [
          ("kind", `String "assign_reference");
          ("target", lvalue_to_yojson target);
          ("source", lvalue_to_yojson source);
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
    | Invoke (callee, arguments) ->
        [
          ("kind", `String "invoke");
          ("callee", expression_to_yojson callee);
          ("arguments", `List (List.map expression_to_yojson arguments));
        ]
    | Function_callable name ->
        [ ("kind", `String "function_callable"); ("name", `String name) ]
    | Method_call (object_, method_name, arguments) ->
        [
          ("kind", `String "method_call");
          ("object", expression_to_yojson object_);
          ("method", `String method_name);
          ("arguments", `List (List.map expression_to_yojson arguments));
        ]
    | Object_method_callable (object_, method_name) ->
        [
          ("kind", `String "object_method_callable");
          ("object", expression_to_yojson object_);
          ("method", `String method_name);
        ]
    | Static_method_call (class_name, method_name, arguments) ->
        [
          ("kind", `String "static_method_call");
          ("class", `String class_name);
          ("method", `String method_name);
          ("arguments", `List (List.map expression_to_yojson arguments));
        ]
    | Static_method_callable (class_name, method_name) ->
        [
          ("kind", `String "static_method_callable");
          ("class", `String class_name);
          ("method", `String method_name);
        ]
    | Parent_method_call (method_name, arguments) ->
        [
          ("kind", `String "parent_method_call");
          ("method", `String method_name);
          ("arguments", `List (List.map expression_to_yojson arguments));
        ]
    | Closure closure ->
        [
          ("kind", `String "closure");
          ("parameters", `List (List.map parameter_to_yojson closure.parameters));
          ( "captures",
            `List (List.map closure_capture_to_yojson closure.captures) );
          ("body", `List (List.map statement_to_yojson closure.body));
        ]
    | New (class_name, arguments) ->
        [
          ("kind", `String "new");
          ("class", `String class_name);
          ("arguments", `List (List.map expression_to_yojson arguments));
        ]
    | Throw expression ->
        [
          ("kind", `String "throw");
          ("expression", expression_to_yojson expression);
        ]
  in
  `Assoc (fields @ [ ("location", location_to_yojson expression.location) ])

and array_item_to_yojson (item : array_item) =
  `Assoc
    [
      ("key", Option.fold ~none:`Null ~some:expression_to_yojson item.key);
      ("value", expression_to_yojson item.value);
      ("location", location_to_yojson item.location);
    ]

and lvalue_to_yojson lvalue =
  let fields =
    match lvalue.desc with
    | Variable_lvalue name ->
        [ ("kind", `String "variable"); ("name", `String name) ]
    | Array_element_lvalue (array, key) ->
        [
          ("kind", `String "array_element");
          ("array", lvalue_to_yojson array);
          ("key", Option.fold ~none:`Null ~some:expression_to_yojson key);
        ]
    | Object_property_lvalue (object_, name) ->
        [
          ("kind", `String "object_property");
          ("object", lvalue_to_yojson object_);
          ("name", `String name);
        ]
    | Static_property_lvalue (class_name, name) ->
        [
          ("kind", `String "static_property");
          ("class", `String class_name);
          ("name", `String name);
        ]
  in
  `Assoc (fields @ [ ("location", location_to_yojson lvalue.location) ])

and statement_to_yojson = function
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
  | Foreach (iterable, key, value, by_reference, body, location) ->
      `Assoc
        [
          ("kind", `String "foreach");
          ("iterable", expression_to_yojson iterable);
          ("key", Option.fold ~none:`Null ~some:lvalue_to_yojson key);
          ("value", lvalue_to_yojson value);
          ("by_reference", `Bool by_reference);
          ("body", `List (List.map statement_to_yojson body));
          ("location", location_to_yojson location);
        ]
  | Break (depth, location) ->
      `Assoc
        [
          ("kind", `String "break");
          ("depth", `Int depth);
          ("location", location_to_yojson location);
        ]
  | Continue (depth, location) ->
      `Assoc
        [
          ("kind", `String "continue");
          ("depth", `Int depth);
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
  | Try (body, catches, finally, location) ->
      `Assoc
        [
          ("kind", `String "try");
          ("body", `List (List.map statement_to_yojson body));
          ("catches", `List (List.map catch_clause_to_yojson catches));
          ( "finally",
            Option.fold ~none:`Null
              ~some:(fun statements ->
                `List (List.map statement_to_yojson statements))
              finally );
          ("location", location_to_yojson location);
        ]
  | Unset (targets, location) ->
      `Assoc
        [
          ("kind", `String "unset");
          ("targets", `List (List.map lvalue_to_yojson targets));
          ("location", location_to_yojson location);
        ]
  | Nop location ->
      `Assoc
        [ ("kind", `String "nop"); ("location", location_to_yojson location) ]

and catch_clause_to_yojson (catch : catch_clause) =
  `Assoc
    [
      ("types", `List (List.map (fun name -> `String name) catch.types));
      ( "variable",
        Option.fold ~none:`Null ~some:(fun name -> `String name) catch.variable
      );
      ("body", `List (List.map statement_to_yojson catch.body));
      ("location", location_to_yojson catch.location);
    ]

and parameter_to_yojson (parameter : parameter) =
  `Assoc
    [
      ("name", `String parameter.name);
      ("location", location_to_yojson parameter.location);
    ]

and closure_capture_to_yojson (capture : closure_capture) =
  `Assoc
    [
      ("name", `String capture.name);
      ("by_reference", `Bool capture.by_reference);
      ("location", location_to_yojson capture.location);
    ]

let function_to_yojson (function_ : function_decl) =
  `Assoc
    [
      ("name", `String function_.name);
      ("parameters", `List (List.map parameter_to_yojson function_.parameters));
      ("body", `List (List.map statement_to_yojson function_.body));
      ("location", location_to_yojson function_.location);
    ]

let member_modifier_to_yojson = function
  | Public -> `String "public"
  | Protected -> `String "protected"
  | Private -> `String "private"
  | Static -> `String "static"

let property_to_yojson (property : property_decl) =
  `Assoc
    [
      ("name", `String property.name);
      ( "default",
        Option.fold ~none:`Null ~some:expression_to_yojson property.default );
      ( "modifiers",
        `List (List.map member_modifier_to_yojson property.modifiers) );
      ("location", location_to_yojson property.location);
    ]

let method_to_yojson (method_ : method_decl) =
  `Assoc
    [
      ("name", `String method_.name);
      ("parameters", `List (List.map parameter_to_yojson method_.parameters));
      ( "body",
        Option.fold ~none:`Null
          ~some:(fun body -> `List (List.map statement_to_yojson body))
          method_.body );
      ("modifiers", `List (List.map member_modifier_to_yojson method_.modifiers));
      ("location", location_to_yojson method_.location);
    ]

let declaration_kind_to_yojson = function
  | Class -> `String "class"
  | Interface -> `String "interface"
  | Trait -> `String "trait"

let trait_adaptation_to_yojson = function
  | Trait_precedence { trait; method_name; instead_of; location } ->
      `Assoc
        [
          ("kind", `String "precedence");
          ("trait", `String trait);
          ("method", `String method_name);
          ("instead_of", `List (List.map (fun name -> `String name) instead_of));
          ("location", location_to_yojson location);
        ]
  | Trait_alias { trait; method_name; alias; visibility; location } ->
      `Assoc
        [
          ("kind", `String "alias");
          ( "trait",
            Option.fold ~none:`Null ~some:(fun name -> `String name) trait );
          ("method", `String method_name);
          ( "alias",
            Option.fold ~none:`Null ~some:(fun name -> `String name) alias );
          ( "visibility",
            Option.fold ~none:`Null ~some:member_modifier_to_yojson visibility
          );
          ("location", location_to_yojson location);
        ]

let trait_use_to_yojson (trait_use : trait_use) =
  `Assoc
    [
      ("traits", `List (List.map (fun name -> `String name) trait_use.traits));
      ( "adaptations",
        `List (List.map trait_adaptation_to_yojson trait_use.adaptations) );
      ("location", location_to_yojson trait_use.location);
    ]

let class_to_yojson (class_ : class_decl) =
  `Assoc
    [
      ("kind", declaration_kind_to_yojson class_.kind);
      ("name", `String class_.name);
      ( "parent",
        Option.fold ~none:`Null ~some:(fun name -> `String name) class_.parent
      );
      ( "interfaces",
        `List (List.map (fun name -> `String name) class_.interfaces) );
      ("traits", `List (List.map trait_use_to_yojson class_.traits));
      ("properties", `List (List.map property_to_yojson class_.properties));
      ("methods", `List (List.map method_to_yojson class_.methods));
      ("location", location_to_yojson class_.location);
    ]

let to_yojson program =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("target_php_version", `String program.target_php_version);
      ("source_file", `String program.source_file);
      ("functions", `List (List.map function_to_yojson program.functions));
      ("classes", `List (List.map class_to_yojson program.classes));
      ("statements", `List (List.map statement_to_yojson program.statements));
    ]
