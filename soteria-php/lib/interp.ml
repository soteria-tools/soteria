let unsupported format = Format.kasprintf Phpsymex.not_impl format
let coercion_error error = unsupported "%a" Coercion.pp_error error

type pending_event = Error.Runtime_event.severity * string

type 'a operation =
  | Completed of 'a * pending_event list
  | Runtime_error of Error.runtime_error * pending_event list

let completed ?(events = []) value = Completed (value, events)

let runtime_error ?(events = []) class_name message =
  Runtime_error ({ Error.class_name; message }, events)

let coerce target value =
  match Coercion.coerce target value with
  | Ok value -> Phpsymex.Result.ok value
  | Error error -> coercion_error error

let numeric_operand value =
  match Coercion.to_number value with
  | Ok value -> Ok (value, [])
  | Error (Leading_numeric_string string) -> (
      match Coercion.classify_numeric_string string with
      | Leading_numeric number ->
          Ok
            ( Coercion.value_of_number number,
              [
                (Error.Runtime_event.Warning, "A non-numeric value encountered");
              ] )
      | Numeric _ | Non_numeric -> assert false)
  | Error error -> Error error

let boolean_value = function
  | Value.Bool value -> value
  | _ -> failwith "boolean coercion returned a non-boolean value"

let string_value = function
  | Value.String value -> value
  | _ -> failwith "string coercion returned a non-string value"

let simplify_value value =
  let open Phpsymex.Syntax in
  let normalize value =
    value
    |> Value.Typed.untyped
    |> Value.Typed.Eval.eval ~force:true
    |> Value.Typed.type_
  in
  match value with
  | Value.Bool value ->
      let+ value = Phpsymex.simplify value in
      Value.Bool (normalize value)
  | Value.Int value ->
      let+ value = Phpsymex.simplify value in
      Value.Int (normalize value)
  | Value.Float value ->
      let+ value = Phpsymex.simplify value in
      Value.Float (normalize value)
  | ( Value.Undef | Value.Null | Value.String _ | Value.Array _ | Value.Object _
    | Value.Callable _ ) as value ->
      Phpsymex.return value

let condition value =
  let open Phpsymex.Syntax in
  let** value = coerce Coercion.Boolean value in
  Phpsymex.Result.ok (boolean_value value)

let float_of_integer value =
  Value.Typed.BitVec.to_float
    ~rounding:Value.Typed.RoundingMode.NearestTiesToEven ~signed:true
    ~fp:Value.Typed.FloatPrecision.F64 value

let numeric_float = function
  | Value.Int value -> float_of_integer value
  | Value.Float value -> value
  | _ -> failwith "numeric coercion returned a non-numeric value"

let concrete_numeric_float = function
  | Value.Int _ as value -> Option.map Int64.to_float (Value.int_value value)
  | Value.Float _ as value -> Value.float_value value
  | _ -> failwith "numeric coercion returned a non-numeric value"

let checked_integer check float left right =
  let result, overflow = check ~signed:true left right in
  let open Phpsymex.Syntax in
  if%sat[@lname "Integer overflow"] [@rname "Integer result"] overflow then
    Phpsymex.Result.ok
      (Value.Float (float (float_of_integer left) (float_of_integer right)))
  else Phpsymex.Result.ok (Value.Int result)

let runtime_type_name state = function
  | Value.Object id -> (
      match State.find_object id state with
      | Some object_ -> object_.class_name
      | None -> failwith "PHP object value refers to an unknown object")
  | value -> Value.type_name value

let unsupported_operand_types state operator left right =
  Printf.sprintf "Unsupported operand types: %s %s %s"
    (runtime_type_name state left)
    operator
    (runtime_type_name state right)

let arithmetic state operator integer float concrete_float left right =
  let open Phpsymex.Syntax in
  match (numeric_operand left, numeric_operand right) with
  | Error _, _ | _, Error _ ->
      Phpsymex.Result.ok
        (runtime_error "TypeError"
           (unsupported_operand_types state operator left right))
  | Ok (left_number, left_events), Ok (right_number, right_events) -> (
      let events = left_events @ right_events in
      match (left_number, right_number) with
      | Value.Int left, Value.Int right ->
          let** value = checked_integer integer float left right in
          Phpsymex.Result.ok (completed ~events value)
      | _ -> (
          match
            ( concrete_numeric_float left_number,
              concrete_numeric_float right_number )
          with
          | Some left, Some right ->
              Phpsymex.Result.ok
                (completed ~events (Value.float (concrete_float left right)))
          | _ ->
              Phpsymex.Result.ok
                (completed ~events
                   (Value.Float
                      (float
                         (numeric_float left_number)
                         (numeric_float right_number))))))

let division state left right =
  let open Phpsymex.Syntax in
  match (numeric_operand left, numeric_operand right) with
  | Error _, _ | _, Error _ ->
      Phpsymex.Result.ok
        (runtime_error "TypeError"
           (unsupported_operand_types state "/" left right))
  | Ok (left, left_events), Ok (right, right_events) -> (
      let events = left_events @ right_events in
      match (concrete_numeric_float left, concrete_numeric_float right) with
      | _, Some denominator when denominator = 0.0 ->
          Phpsymex.Result.ok
            (runtime_error ~events "DivisionByZeroError" "Division by zero")
      | Some numerator, Some denominator ->
          Phpsymex.Result.ok
            (completed ~events (Value.float (numerator /. denominator)))
      | _ ->
          let numerator = numeric_float left in
          let denominator = numeric_float right in
          if%sat[@lname "Division by zero"] [@rname "Division"]
            Value.Typed.Float.is_zero denominator
          then
            Phpsymex.Result.ok
              (runtime_error ~events "DivisionByZeroError" "Division by zero")
          else
            Phpsymex.Result.ok
              (completed ~events
                 (Value.Float (Value.Typed.Float.div numerator denominator))))

let rec strict_equal state left right =
  match (left, right) with
  | Value.Undef, Value.Undef | Value.Null, Value.Null -> Value.Typed.Bool.v_true
  | Value.Bool left, Value.Bool right -> Value.Typed.sem_eq left right
  | Value.Int left, Value.Int right -> Value.Typed.sem_eq left right
  | Value.Float left, Value.Float right -> Value.Typed.Float.eq left right
  | Value.String left, Value.String right ->
      Value.Typed.Bool.of_bool (String.equal left right)
  | Value.Array left, Value.Array right -> strict_equal_arrays state left right
  | Value.Object left, Value.Object right ->
      Value.Typed.Bool.of_bool (left = right)
  | Value.Callable left, Value.Callable right ->
      Value.Typed.Bool.of_bool (left = right)
  | _ -> Value.Typed.Bool.v_false

and strict_equal_arrays state left right =
  let left = Value.array_bindings left in
  let right = Value.array_bindings right in
  if List.length left <> List.length right then Value.Typed.Bool.v_false
  else
    List.fold_left2
      (fun equal (left_key, left_entry) (right_key, right_entry) ->
        let keys_equal =
          match (left_key, right_key) with
          | Value.Integer_key left, Value.Integer_key right ->
              Value.Typed.Bool.of_bool (Int64.equal left right)
          | Value.String_key left, Value.String_key right ->
              Value.Typed.Bool.of_bool (String.equal left right)
          | Value.Symbolic_integer_key left, Value.Symbolic_integer_key right ->
              Value.Typed.sem_eq left right
          | Value.Symbolic_integer_key symbolic, Integer_key concrete
          | Integer_key concrete, Value.Symbolic_integer_key symbolic ->
              Value.Typed.sem_eq symbolic
                (Value.Typed.BitVec.mk_masked Value.integer_bits
                   (Z.of_int64 concrete))
          | (Integer_key _ | Symbolic_integer_key _), String_key _
          | String_key _, (Integer_key _ | Symbolic_integer_key _) ->
              Value.Typed.Bool.v_false
        in
        let left_value =
          State.value_of_array_entry left_entry state
          |> Option.value ~default:Value.undef
        in
        let right_value =
          State.value_of_array_entry right_entry state
          |> Option.value ~default:Value.undef
        in
        let values_equal =
          match (left_entry, right_entry) with
          | Value.Reference left, Value.Reference right when left = right ->
              Value.Typed.Bool.v_true
          | _ -> strict_equal state left_value right_value
        in
        Value.Typed.Bool.and_ equal
          (Value.Typed.Bool.and_ keys_equal values_equal))
      Value.Typed.Bool.v_true left right

let loose_equal left right =
  match Coercion.compare_scalar Coercion.Equal left right with
  | Ok result -> Phpsymex.Result.ok (Value.Bool result)
  | Error error -> coercion_error error

let comparison operator left right =
  let operator =
    match operator with
    | Php_ir.Less_than -> Coercion.Less_than
    | Less_than_or_equal -> Coercion.Less_than_or_equal
    | Greater_than -> Coercion.Greater_than
    | Greater_than_or_equal -> Coercion.Greater_than_or_equal
    | _ -> failwith "non-ordering operator passed to comparison"
  in
  match Coercion.compare_scalar operator left right with
  | Ok result -> Phpsymex.Result.ok (Value.Bool result)
  | Error error -> coercion_error error

let binary state operator left right =
  match operator with
  | Php_ir.Add -> (
      match (left, right) with
      | Value.Array left, Value.Array right ->
          if
            Value.array_next_key_is_symbolic left
            || Value.array_next_key_is_symbolic right
          then unsupported "array union with symbolic integer keys"
          else
            Phpsymex.Result.ok
              (completed (Value.array (Value.array_union left right)))
      | _ ->
          arithmetic state "+" Value.Typed.BitVec.add_checked
            Value.Typed.Float.add ( +. ) left right)
  | Subtract ->
      arithmetic state "-" Value.Typed.BitVec.sub_checked Value.Typed.Float.sub
        ( -. ) left right
  | Multiply ->
      arithmetic state "*" Value.Typed.BitVec.mul_checked Value.Typed.Float.mul
        ( *. ) left right
  | Divide -> division state left right
  | Concat ->
      let open Phpsymex.Syntax in
      let** left = coerce Coercion.String left in
      let** right = coerce Coercion.String right in
      Phpsymex.Result.ok
        (completed (Value.string (string_value left ^ string_value right)))
  | Identical ->
      Phpsymex.Result.ok
        (completed (Value.Bool (strict_equal state left right)))
  | Not_identical ->
      Phpsymex.Result.ok
        (completed
           (Value.Bool (Value.Typed.Bool.not (strict_equal state left right))))
  | Equal ->
      let open Phpsymex.Syntax in
      let** value = loose_equal left right in
      Phpsymex.Result.ok (completed value)
  | Not_equal ->
      let open Phpsymex.Syntax in
      let** equal = loose_equal left right in
      Phpsymex.Result.ok
        (completed (Value.Bool (Value.Typed.Bool.not (boolean_value equal))))
  | Less_than | Less_than_or_equal | Greater_than | Greater_than_or_equal ->
      let open Phpsymex.Syntax in
      let** value = comparison operator left right in
      Phpsymex.Result.ok (completed value)
  | Boolean_and | Boolean_or ->
      failwith "short-circuit operator passed to eager evaluation"

let unary operator value =
  match operator with
  | Php_ir.Boolean_not ->
      let open Phpsymex.Syntax in
      let** condition = condition value in
      Phpsymex.Result.ok
        (completed (Value.Bool (Value.Typed.Bool.not condition)))
  | Numeric_identity -> (
      match numeric_operand value with
      | Ok (value, events) -> Phpsymex.Result.ok (completed ~events value)
      | Error _ ->
          Phpsymex.Result.ok
            (runtime_error "TypeError"
               (Printf.sprintf "Unsupported operand type: %s"
                  (Value.type_name value))))
  | Numeric_negation -> (
      let open Phpsymex.Syntax in
      match numeric_operand value with
      | Error _ ->
          Phpsymex.Result.ok
            (runtime_error "TypeError"
               (Printf.sprintf "Unsupported operand type: %s"
                  (Value.type_name value)))
      | Ok (value, events) -> (
          match value with
          | Value.Int value ->
              let result, overflow = Value.Typed.BitVec.neg_checked value in
              if%sat[@lname "Integer overflow"] [@rname "Integer result"]
                overflow
              then
                Phpsymex.Result.ok
                  (completed ~events
                     (Value.Float
                        (Value.Typed.Float.neg (float_of_integer value))))
              else Phpsymex.Result.ok (completed ~events (Value.Int result))
          | Value.Float value ->
              Phpsymex.Result.ok
                (completed ~events (Value.Float (Value.Typed.Float.neg value)))
          | _ -> assert false))

module Function_map = Map.Make (String)
module Class_map = Map.Make (String)

type method_member = {
  declaring_class : string;
  declaration : Php_ir.method_decl;
}

type property_member = {
  declaring_class : string;
  declaration : Php_ir.property_decl;
}

type class_info = {
  kind : Php_ir.declaration_kind;
  name : string;
  parent : string option;
  interfaces : string list;
  properties : property_member list;
  methods : method_member list;
  location : Php_ir.location;
}

type declarations = {
  functions : Php_ir.function_decl Function_map.t;
  classes : class_info Class_map.t;
}

type thrown = { value : Value.t; trace : Error.Trace.t }
type 'a evaluation = Evaluated of 'a | Raised of thrown

type control =
  | Normal
  | Return of Value.t
  | Break of int
  | Continue of int
  | Throw of thrown

type access = Read | Write | Unset

type place =
  | Variable of string
  | Array_element of place * Value.array_key
  | Object_property of State.object_id * State.object_property
  | Static_property of string * string
  | Temporary of Value.t
  | Magic_set of State.object_id * string * method_member
  | Magic_unset of State.object_id * string * method_member
  | Inaccessible_property of place * Error.runtime_error
  | Invalid_read

type throwable_class = {
  name : string;
  parent : string option;
  constructible : bool;
}

let throwable ?parent name = { name; parent; constructible = true }

let throwable_classes =
  [
    { name = "Throwable"; parent = None; constructible = false };
    throwable ~parent:"Throwable" "Exception";
    throwable ~parent:"Exception" "LogicException";
    throwable ~parent:"LogicException" "InvalidArgumentException";
    throwable ~parent:"LogicException" "DomainException";
    throwable ~parent:"LogicException" "LengthException";
    throwable ~parent:"LogicException" "OutOfRangeException";
    throwable ~parent:"Exception" "RuntimeException";
    throwable ~parent:"RuntimeException" "OutOfBoundsException";
    throwable ~parent:"RuntimeException" "OverflowException";
    throwable ~parent:"RuntimeException" "RangeException";
    throwable ~parent:"RuntimeException" "UnderflowException";
    throwable ~parent:"RuntimeException" "UnexpectedValueException";
    throwable ~parent:"Throwable" "Error";
    throwable ~parent:"Error" "ArithmeticError";
    throwable ~parent:"ArithmeticError" "DivisionByZeroError";
    throwable ~parent:"Error" "AssertionError";
    throwable ~parent:"Error" "TypeError";
    throwable ~parent:"TypeError" "ArgumentCountError";
    throwable ~parent:"Error" "ValueError";
  ]

let find_throwable_class name =
  let name = Builtins.canonical_name name in
  List.find_opt
    (fun class_ -> String.equal (String.lowercase_ascii class_.name) name)
    throwable_classes

let rec builtin_class_is_a actual expected =
  let expected = Builtins.canonical_name expected in
  if String.equal (String.lowercase_ascii actual) expected then true
  else
    match find_throwable_class actual with
    | Some { parent = Some parent; _ } -> builtin_class_is_a parent expected
    | Some { parent = None; _ } | None -> false

let rec class_is_a declarations actual expected =
  let expected = Builtins.canonical_name expected in
  if String.equal (String.lowercase_ascii actual) expected then true
  else
    match
      Class_map.find_opt (Builtins.canonical_name actual) declarations.classes
    with
    | Some class_ ->
        List.exists
          (fun name -> class_is_a declarations name expected)
          class_.interfaces
        || Option.fold ~none:false
             ~some:(fun parent -> class_is_a declarations parent expected)
             class_.parent
    | None -> builtin_class_is_a actual expected

let evaluated value state = Phpsymex.Result.ok (Evaluated value, state)

let bind_evaluation evaluation continuation =
  let open Phpsymex.Syntax in
  let** result, state = evaluation in
  match result with
  | Evaluated value -> continuation (value, state)
  | Raised thrown -> Phpsymex.Result.ok (Raised thrown, state)

let ( let*** ) = bind_evaluation

let throwable_message name arguments =
  let open Phpsymex.Syntax in
  match arguments with
  | [] -> Phpsymex.Result.ok ""
  | [ value ] ->
      let** value = coerce Coercion.String value in
      Phpsymex.Result.ok (string_value value)
  | _ -> unsupported "%s::__construct() arguments beyond the message" name

let construct_throwable state name arguments =
  match find_throwable_class name with
  | Some { constructible = true; name; _ } ->
      let open Phpsymex.Syntax in
      let** message = throwable_message name arguments in
      let id, state = State.allocate_object name message state in
      evaluated (Value.object_ id) state
  | Some { constructible = false; _ } -> unsupported "construction of Throwable"
  | None -> failwith "non-throwable class passed to throwable construction"

let raise_value ?declarations value state =
  let value, state =
    match value with
    | Value.Object id -> (
        match State.find_object id state with
        | Some object_
          when Option.fold
                 ~none:(builtin_class_is_a object_.class_name "Throwable")
                 ~some:(fun declarations ->
                   class_is_a declarations object_.class_name "Throwable")
                 declarations ->
            (value, state)
        | Some _ ->
            let id, state =
              State.allocate_object "Error"
                "Cannot throw objects that do not implement Throwable" state
            in
            (Value.object_ id, state)
        | None -> failwith "PHP object value refers to an unknown object")
    | _ ->
        let id, state =
          State.allocate_object "Error" "Can only throw objects" state
        in
        (Value.object_ id, state)
  in
  let open Phpsymex.Syntax in
  let* trace = Phpsymex.get_trace () in
  Phpsymex.Result.ok (Raised { value; trace }, state)

let raise_runtime_error state (error : Error.runtime_error) =
  let id, state = State.allocate_object error.class_name error.message state in
  raise_value (Value.object_ id) state

let record_runtime_event severity message state =
  let open Phpsymex.Syntax in
  let* trace = Phpsymex.get_trace () in
  let event = Error.Runtime_event.make severity message trace in
  evaluated () (State.emit_runtime_event event state)

let rec record_runtime_events events state =
  match events with
  | [] -> evaluated () state
  | (severity, message) :: events ->
      let open Phpsymex.Syntax in
      let*** (), state = record_runtime_event severity message state in
      record_runtime_events events state

let apply_operation operation state =
  match operation with
  | Runtime_error (error, events) ->
      let open Phpsymex.Syntax in
      let*** (), state = record_runtime_events events state in
      raise_runtime_error state error
  | Completed (value, events) ->
      let open Phpsymex.Syntax in
      let*** (), state = record_runtime_events events state in
      evaluated value state

let rec read_place state = function
  | Variable name ->
      Option.value ~default:Value.undef (State.find_variable name state)
  | Array_element (array, key) -> (
      match read_place state array with
      | Value.Array array ->
          State.find_array_value key array state
          |> Option.value ~default:Value.undef
      | _ -> Value.undef)
  | Object_property (object_id, name) ->
      State.find_object_property object_id name state
      |> Option.value ~default:Value.undef
  | Static_property (declaring_class, name) ->
      State.find_static_property ~declaring_class name state
      |> Option.value ~default:Value.undef
  | Temporary value -> value
  | Magic_set _ | Magic_unset _ ->
      failwith "read from a pending PHP magic-property operation"
  | Inaccessible_property (property, _) -> read_place state property
  | Invalid_read -> Value.null

let rec place_contains_inaccessible = function
  | Array_element (parent, _) -> place_contains_inaccessible parent
  | Inaccessible_property _ -> true
  | Variable _ | Object_property _ | Static_property _ | Temporary _
  | Magic_set _ | Magic_unset _ | Invalid_read ->
      false

let cannot_use_as_array state value =
  match value with
  | Value.Object id -> (
      match State.find_object id state with
      | Some object_ ->
          {
            Error.class_name = "Error";
            message =
              Printf.sprintf "Cannot use object of type %s as array"
                object_.class_name;
          }
      | None -> failwith "PHP object value refers to an unknown object")
  | _ ->
      {
        Error.class_name = "Error";
        message = "Cannot use a scalar value as an array";
      }

let rec write_place place value state =
  match place with
  | Variable name -> evaluated () (State.set_variable name value state)
  | Array_element (parent, key) -> (
      let array =
        match read_place state parent with
        | Value.Array array -> Some array
        | Value.Undef | Value.Null -> Some Value.empty_array
        | Value.Bool value when Value.Typed.Bool.to_bool value = Some false ->
            Some Value.empty_array
        | _ -> None
      in
      match array with
      | Some array -> (
          match Value.array_find key array with
          | Some (Value.Reference cell) ->
              evaluated () (State.set_cell cell value state)
          | Some (Value.Inline _) | None ->
              write_place parent
                (Value.array (Value.array_set key value array))
                state)
      | None ->
          raise_runtime_error state
            (cannot_use_as_array state (read_place state parent)))
  | Object_property (object_id, name) ->
      evaluated () (State.set_object_property object_id name value state)
  | Static_property (declaring_class, name) ->
      evaluated () (State.set_static_property ~declaring_class name value state)
  | Magic_set _ -> unsupported "indirect write through an overloaded property"
  | Temporary _ | Magic_unset _ ->
      failwith "write through a non-writable PHP temporary"
  | Inaccessible_property (_, error) -> raise_runtime_error state error
  | Invalid_read -> failwith "write through an invalid PHP read"

let bind_place_reference place cell state =
  match place with
  | Variable name -> evaluated () (State.bind_variable name cell state)
  | Array_element (parent, key) -> (
      match read_place state parent with
      | Value.Array array ->
          write_place parent
            (Value.array (Value.array_set_reference key cell array))
            state
      | Value.Undef | Value.Null ->
          write_place parent
            (Value.array (Value.array_set_reference key cell Value.empty_array))
            state
      | Value.Bool value when Value.Typed.Bool.to_bool value = Some false ->
          write_place parent
            (Value.array (Value.array_set_reference key cell Value.empty_array))
            state
      | value -> raise_runtime_error state (cannot_use_as_array state value))
  | Object_property (object_id, name) ->
      evaluated () (State.bind_object_property object_id name cell state)
  | Static_property (declaring_class, name) ->
      evaluated () (State.bind_static_property ~declaring_class name cell state)
  | Magic_set _ | Magic_unset _ ->
      unsupported "reference assignment involving an overloaded property"
  | Temporary _ -> failwith "bind a reference to a PHP temporary"
  | Inaccessible_property (_, error) -> raise_runtime_error state error
  | Invalid_read -> failwith "bind through an invalid PHP read"

let cell_for_reference place state =
  match place with
  | Variable name ->
      let cell, state = State.ensure_variable name state in
      let state =
        match State.find_cell cell state with
        | Some Value.Undef -> State.set_cell cell Value.null state
        | Some _ -> state
        | None -> failwith "variable is bound to an unknown PHP cell"
      in
      evaluated cell state
  | Array_element (parent, key) -> (
      let array =
        match read_place state parent with
        | Value.Array array -> Some array
        | Value.Undef | Value.Null -> Some Value.empty_array
        | Value.Bool value when Value.Typed.Bool.to_bool value = Some false ->
            Some Value.empty_array
        | _ -> None
      in
      match array with
      | None ->
          raise_runtime_error state
            (cannot_use_as_array state (read_place state parent))
      | Some array -> (
          match Value.array_find key array with
          | Some (Value.Reference cell) -> evaluated cell state
          | entry ->
              let value =
                match entry with
                | Some (Value.Inline value) -> value
                | None -> Value.null
                | Some (Value.Reference _) -> assert false
              in
              let cell, state = State.allocate_cell value state in
              let open Phpsymex.Syntax in
              let*** (), state =
                write_place parent
                  (Value.array (Value.array_set_reference key cell array))
                  state
              in
              evaluated cell state))
  | Object_property (object_id, name) -> (
      match State.find_object_property_cell object_id name state with
      | Some cell -> evaluated cell state
      | None ->
          let cell, state = State.allocate_cell Value.null state in
          let state = State.bind_object_property object_id name cell state in
          evaluated cell state)
  | Static_property (declaring_class, name) ->
      evaluated
        (State.find_static_property_cell ~declaring_class name state
        |> Option.get)
        state
  | Magic_set _ | Magic_unset _ ->
      unsupported "reference assignment involving an overloaded property"
  | Temporary _ -> unsupported "reference to an overloaded property value"
  | Inaccessible_property (_, error) -> raise_runtime_error state error
  | Invalid_read -> failwith "take a reference through an invalid PHP read"

let unset_place place state =
  match place with
  | Variable name -> evaluated () (State.unset_variable name state)
  | Array_element (parent, key) -> (
      match read_place state parent with
      | Value.Array array ->
          write_place parent (Value.array (Value.array_remove key array)) state
      | Value.Undef | Value.Null -> evaluated () state
      | Value.Object _ as value ->
          raise_runtime_error state (cannot_use_as_array state value)
      | _ ->
          raise_runtime_error state
            {
              Error.class_name = "Error";
              message = "Cannot unset offset in a non-array variable";
            })
  | Object_property (object_id, name) ->
      evaluated () (State.unset_object_property object_id name state)
  | Static_property _ ->
      raise_runtime_error state
        {
          Error.class_name = "Error";
          message = "Attempt to unset static property";
        }
  | Magic_unset _ -> unsupported "nested unset through an overloaded property"
  | Temporary _ | Magic_set _ ->
      failwith "unset through a non-unsettable PHP temporary"
  | Inaccessible_property (_, error) -> raise_runtime_error state error
  | Invalid_read -> evaluated () state

let normalized_array_key value state =
  match Coercion.to_array_key value with
  | Ok key -> (
      let deprecation =
        match Value.float_value value with
        | Some float when Float.trunc float <> float ->
            Some
              (Printf.sprintf
                 "Implicit conversion from float %s to int loses precision"
                 (Coercion.string_of_float float))
        | Some _ | None -> None
      in
      match deprecation with
      | None -> evaluated key state
      | Some message ->
          let open Phpsymex.Syntax in
          let*** (), state =
            record_runtime_event Error.Runtime_event.Deprecation message state
          in
          evaluated key state)
  | Error (Invalid_array_key _) ->
      raise_runtime_error state
        {
          Error.class_name = "TypeError";
          message =
            Printf.sprintf "Cannot access offset of type %s on array"
              (runtime_type_name state value);
        }
  | Error error -> coercion_error error

let resolve_array_key ~for_write:_ array key state =
  let symbolic_value = function
    | Value.Integer_key key ->
        Value.Typed.BitVec.mk_masked Value.integer_bits (Z.of_int64 key)
    | Value.Symbolic_integer_key key -> key
    | Value.String_key _ -> assert false
  in
  let choose symbolic_key fresh_key keys =
    let rec choose = function
      | [] -> evaluated fresh_key state
      | key :: keys ->
          let open Phpsymex.Syntax in
          if%sat[@lname "Existing array key"] [@rname "Different array key"]
            Value.Typed.sem_eq symbolic_key (symbolic_value key)
          then evaluated key state
          else choose keys
    in
    choose keys
  in
  match key with
  | Coercion.Concrete_key (Value.Integer_key _ as concrete_key) ->
      choose
        (symbolic_value concrete_key)
        concrete_key
        (Value.array_integer_keys array
        |> List.filter (function
          | Value.Symbolic_integer_key _ -> true
          | Value.Integer_key _ | String_key _ -> false))
  | Concrete_key (Value.String_key _ as key) -> evaluated key state
  | Concrete_key (Value.Symbolic_integer_key _) -> assert false
  | Coercion.Symbolic_integer_key symbolic_key ->
      choose symbolic_key (Value.Symbolic_integer_key symbolic_key)
        (Value.array_integer_keys array)

let array_for_access ~for_write value state =
  match value with
  | Value.Array array -> evaluated (Some array) state
  | (Value.Undef | Value.Null) when for_write ->
      evaluated (Some Value.empty_array) state
  | Value.Bool value when for_write -> (
      match Value.Typed.Bool.to_bool value with
      | Some false ->
          let open Phpsymex.Syntax in
          let*** (), state =
            record_runtime_event Error.Runtime_event.Deprecation
              "Automatic conversion of false to array is deprecated" state
          in
          evaluated (Some Value.empty_array) state
      | Some true ->
          raise_runtime_error state
            (cannot_use_as_array state (Value.Bool value))
      | None -> unsupported "symbolic boolean array autovivification")
  | Value.String _ when for_write -> unsupported "string offset assignment"
  | value when for_write ->
      raise_runtime_error state (cannot_use_as_array state value)
  | Value.Object _ as value ->
      raise_runtime_error state (cannot_use_as_array state value)
  | Value.String _ -> unsupported "string offset access"
  | value ->
      let message =
        Printf.sprintf "Trying to access array offset on %s"
          (Value.type_name value)
      in
      let open Phpsymex.Syntax in
      let*** (), state =
        record_runtime_event Error.Runtime_event.Warning message state
      in
      evaluated None state

let bind_parameters (parameters : Php_ir.parameter list) arguments =
  let rec bind bindings parameters arguments =
    match (parameters, arguments) with
    | [], _ -> List.rev bindings
    | (parameter : Php_ir.parameter) :: parameters, argument :: arguments ->
        bind ((parameter.name, argument) :: bindings) parameters arguments
    | _ :: _, [] -> failwith "not enough arguments to bind PHP parameters"
  in
  bind [] parameters arguments

let array_of_values values =
  values
  |> List.mapi (fun index value ->
      (Value.Integer_key (Int64.of_int index), value))
  |> List.fold_left
       (fun array (key, value) -> Value.array_set key value array)
       Value.empty_array
  |> Value.array

let member_is_static modifiers = List.mem Php_ir.Static modifiers

let member_visibility = function
  | Php_ir.Public :: _ -> Php_ir.Public
  | Protected :: _ -> Protected
  | Private :: _ -> Private
  | _ -> failwith "PHP member has invalid visibility modifiers"

let find_local_method (class_ : class_info) name =
  let canonical_name = Builtins.canonical_name name in
  List.find_opt
    (fun (method_ : method_member) ->
      String.equal
        (Builtins.canonical_name method_.declaration.Php_ir.name)
        canonical_name)
    class_.methods

let rec find_method_from declarations class_ name =
  match find_local_method class_ name with
  | Some method_ -> Some method_
  | None ->
      Option.bind class_.parent (fun parent ->
          Option.bind
            (Class_map.find_opt
               (Builtins.canonical_name parent)
               declarations.classes)
            (fun parent -> find_method_from declarations parent name))

let find_method declarations state class_ name =
  let contextual_private =
    Option.bind (State.current_class_context state) (fun context ->
        match
          Option.bind
            (Class_map.find_opt
               (Builtins.canonical_name context)
               declarations.classes)
            (fun context -> find_local_method context name)
        with
        | Some method_
          when member_visibility method_.declaration.modifiers = Php_ir.Private
          ->
            Some method_
        | Some _ | None -> None)
  in
  match contextual_private with
  | Some method_ -> Some method_
  | None -> find_method_from declarations class_ name

let rec find_builtin_parent declarations (class_ : class_info) =
  Option.bind class_.parent (fun parent ->
      match
        Class_map.find_opt (Builtins.canonical_name parent) declarations.classes
      with
      | Some parent -> find_builtin_parent declarations parent
      | None -> find_throwable_class parent)

let find_local_property ?(static = false) (class_ : class_info) name =
  List.find_opt
    (fun (property : property_member) ->
      String.equal property.declaration.Php_ir.name name
      && Bool.equal (member_is_static property.declaration.modifiers) static)
    class_.properties

let rec find_property_from ?(static = false) declarations class_ name =
  match find_local_property ~static class_ name with
  | Some property -> Some property
  | None ->
      Option.bind class_.parent (fun parent ->
          Option.bind
            (Class_map.find_opt
               (Builtins.canonical_name parent)
               declarations.classes)
            (fun parent -> find_property_from ~static declarations parent name))

let rec find_any_property_from declarations class_ name =
  match
    List.find_opt
      (fun (property : property_member) ->
        String.equal property.declaration.Php_ir.name name)
      class_.properties
  with
  | Some property -> Some property
  | None ->
      Option.bind class_.parent (fun parent ->
          Option.bind
            (Class_map.find_opt
               (Builtins.canonical_name parent)
               declarations.classes)
            (fun parent -> find_any_property_from declarations parent name))

let find_property declarations state class_ name =
  let contextual_private =
    Option.bind (State.current_class_context state) (fun context ->
        match
          Option.bind
            (Class_map.find_opt
               (Builtins.canonical_name context)
               declarations.classes)
            (fun context -> find_local_property context name)
        with
        | Some property_
          when member_visibility property_.declaration.modifiers
               = Php_ir.Private ->
            Some property_
        | Some _ | None -> None)
  in
  match contextual_private with
  | Some property -> Some property
  | None -> find_property_from declarations class_ name

let object_builtin_names =
  [ "get_class"; "is_a"; "property_exists"; "method_exists" ]

let is_object_builtin name =
  List.mem (Builtins.canonical_name name) object_builtin_names

let known_class declarations name =
  Class_map.mem (Builtins.canonical_name name) declarations.classes
  || Option.is_some (find_throwable_class name)

let class_has_to_string declarations name =
  match
    Class_map.find_opt (Builtins.canonical_name name) declarations.classes
  with
  | Some class_ ->
      Option.is_some (find_method_from declarations class_ "__toString")
  | None -> Option.is_some (find_throwable_class name)

let builtin_is_a declarations actual expected =
  if String.equal (Builtins.canonical_name expected) "stringable" then
    class_has_to_string declarations actual
  else
    known_class declarations expected && class_is_a declarations actual expected

let rec declared_property_exists declarations ~include_private class_ name =
  let local =
    List.exists
      (fun (property : property_member) ->
        String.equal property.declaration.name name
        && (include_private
           || member_visibility property.declaration.modifiers <> Php_ir.Private
           ))
      class_.properties
  in
  local
  || Option.fold ~none:false
       ~some:(fun parent ->
         Option.fold ~none:false
           ~some:(fun parent ->
             declared_property_exists declarations ~include_private:false parent
               name)
           (Class_map.find_opt
              (Builtins.canonical_name parent)
              declarations.classes))
       class_.parent

let member_accessible declarations state declaring_class = function
  | modifiers when member_visibility modifiers = Php_ir.Public -> true
  | modifiers when member_visibility modifiers = Private -> (
      match State.current_class_context state with
      | Some current_class ->
          String.equal
            (Builtins.canonical_name current_class)
            (Builtins.canonical_name declaring_class)
      | None -> false)
  | modifiers when member_visibility modifiers = Protected -> (
      match State.current_class_context state with
      | Some current_class ->
          class_is_a declarations current_class declaring_class
          || class_is_a declarations declaring_class current_class
      | None -> false)
  | _ -> failwith "PHP member has invalid visibility modifiers"

let inaccessible_method_error state (method_ : method_member) =
  let visibility =
    match member_visibility method_.declaration.modifiers with
    | Private -> "private"
    | Protected -> "protected"
    | Public -> failwith "public PHP method reported as inaccessible"
    | Static -> assert false
  in
  let scope =
    match State.current_class_context state with
    | Some name -> "scope " ^ name
    | None -> "global scope"
  in
  let member =
    if
      List.mem
        (Builtins.canonical_name method_.declaration.name)
        [ "__construct"; "__clone" ]
    then
      Printf.sprintf "%s::%s()" method_.declaring_class method_.declaration.name
    else
      Printf.sprintf "method %s::%s()" method_.declaring_class
        method_.declaration.name
  in
  {
    Error.class_name = "Error";
    message = Printf.sprintf "Call to %s %s from %s" visibility member scope;
  }

let inaccessible_property_error (property : property_member) =
  let visibility =
    match member_visibility property.declaration.modifiers with
    | Private -> "private"
    | Protected -> "protected"
    | Public -> failwith "public PHP property reported as inaccessible"
    | Static -> assert false
  in
  {
    Error.class_name = "Error";
    message =
      Printf.sprintf "Cannot access %s property %s::$%s" visibility
        property.declaring_class property.declaration.name;
  }

let resolve_class_reference declarations state name =
  match Builtins.canonical_name name with
  | "self" -> State.current_class_context state
  | "static" -> State.current_called_class state
  | "parent" ->
      Option.bind (State.current_class_context state) (fun current ->
          Option.bind
            (Class_map.find_opt
               (Builtins.canonical_name current)
               declarations.classes)
            (fun class_ -> class_.parent))
  | _ -> Some name

let finish_evaluation evaluation continuation =
  let open Phpsymex.Syntax in
  let** result, state = evaluation in
  match result with
  | Evaluated value -> continuation value state
  | Raised thrown -> Phpsymex.Result.ok (Throw thrown, state)

let rec eval_expressions functions state expressions =
  let open Phpsymex.Syntax in
  match expressions with
  | [] -> evaluated [] state
  | expression :: expressions ->
      let*** value, state = eval_expression functions state expression in
      let*** values, state = eval_expressions functions state expressions in
      evaluated (value :: values) state

and eval_array_items functions state array items =
  let open Phpsymex.Syntax in
  match items with
  | [] -> evaluated (Value.array array) state
  | (item : Php_ir.array_item) :: items ->
      let process =
        let*** key, state =
          match item.key with
          | None -> (
              if Value.array_next_key_is_symbolic array then
                unsupported "append after symbolic array-key insertion"
              else
                match Value.array_next_key array with
                | Some key -> evaluated key state
                | None ->
                    raise_runtime_error state
                      {
                        Error.class_name = "Error";
                        message =
                          "Cannot add element to the array as the next element \
                           is already occupied";
                      })
          | Some expression ->
              let*** value, state =
                eval_expression functions state expression
              in
              let*** key, state = normalized_array_key value state in
              resolve_array_key ~for_write:true array key state
        in
        let*** value, state = eval_expression functions state item.value in
        eval_array_items functions state (Value.array_set key value array) items
      in
      Phpsymex.with_location ~location:item.location process

and eval_property_defaults declarations state properties =
  match properties with
  | [] -> evaluated [] state
  | (property : property_member) :: properties ->
      let open Phpsymex.Syntax in
      let*** value, state =
        match property.declaration.default with
        | None -> evaluated Value.null state
        | Some expression -> eval_expression declarations state expression
      in
      let*** values, state =
        eval_property_defaults declarations state properties
      in
      let property_key =
        State.declared_property ~declaring_class:property.declaring_class
          property.declaration.name
      in
      evaluated ((property_key, value) :: values) state

and property_layout (declarations : declarations) (class_ : class_info) =
  let inherited =
    Option.bind class_.parent (fun parent ->
        Option.map
          (property_layout declarations)
          (Class_map.find_opt
             (Builtins.canonical_name parent)
             declarations.classes))
    |> Option.value ~default:[]
  in
  List.fold_left
    (fun layout (property : property_member) ->
      if member_is_static property.declaration.modifiers then layout
      else
        match member_visibility property.declaration.modifiers with
        | Php_ir.Private -> layout @ [ property ]
        | Public | Protected ->
            List.filter
              (fun (inherited : property_member) ->
                not
                  (String.equal inherited.declaration.name
                     property.declaration.name
                  && member_visibility inherited.declaration.modifiers
                     <> Php_ir.Private))
              layout
            @ [ property ]
        | Static -> assert false)
    inherited class_.properties

and construct_object declarations state location name arguments =
  let canonical_name = Builtins.canonical_name name in
  match Class_map.find_opt canonical_name declarations.classes with
  | None -> unsupported "object construction for class %s" name
  | Some class_ when class_.kind <> Php_ir.Class ->
      unsupported "construction of %s %s"
        (match class_.kind with
        | Interface -> "interface"
        | Trait -> "trait"
        | Class -> assert false)
        class_.name
  | Some class_ -> (
      let open Phpsymex.Syntax in
      let*** properties, state =
        eval_property_defaults declarations state
          (property_layout declarations class_)
      in
      let id, state = State.allocate_object ~properties class_.name "" state in
      let object_ = Value.object_ id in
      match find_method_from declarations class_ "__construct" with
      | None when class_is_a declarations class_.name "Throwable" ->
          let** message = throwable_message class_.name arguments in
          evaluated object_ (State.set_object_message id message state)
      | None -> evaluated object_ state
      | Some constructor -> (
          let open Phpsymex.Syntax in
          let** result, state =
            call_method declarations state location id constructor arguments
          in
          match result with
          | Evaluated _ -> evaluated object_ state
          | Raised thrown -> Phpsymex.Result.ok (Raised thrown, state)))

and resolve_lvalue functions state ~access (lvalue : Php_ir.lvalue) =
  let process =
    let open Phpsymex.Syntax in
    match lvalue.desc with
    | Variable_lvalue name -> (
        match access with
        | (Write | Unset) when String.equal name "this" ->
            raise_runtime_error state
              {
                Error.class_name = "Error";
                message =
                  (if access = Unset then "Cannot unset $this"
                   else "Cannot re-assign $this");
              }
        | Write | Unset -> evaluated (Variable name) state
        | Read -> (
            match State.find_variable name state with
            | Some value when Value.kind value <> `Undefined ->
                evaluated (Variable name) state
            | (Some _ | None) when String.equal name "this" ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message = "Using $this when not in object context";
                  }
            | Some _ | None ->
                let*** (), state =
                  record_runtime_event Error.Runtime_event.Warning
                    (Printf.sprintf "Undefined variable $%s" name)
                    state
                in
                evaluated Invalid_read state))
    | Array_element_lvalue (parent, key_expression) ->
        let*** parent, state = resolve_lvalue functions state ~access parent in
        let*** (), state =
          match parent with
          | Magic_set _ | Magic_unset _ ->
              unsupported "indirect modification of an overloaded property"
          | Variable _ | Array_element _ | Object_property _ | Static_property _
          | Temporary _ | Inaccessible_property _ | Invalid_read ->
              evaluated () state
        in
        let*** place, state =
          match key_expression with
          | None -> (
              if access <> Write then unsupported "array append read"
              else if
                match read_place state parent with
                | Value.Array array -> Value.array_next_key_is_symbolic array
                | _ -> false
              then unsupported "append after symbolic array-key insertion"
              else
                let*** array, state =
                  array_for_access ~for_write:true (read_place state parent)
                    state
                in
                let array = Option.get array in
                match Value.array_reserve_next array with
                | Some (key, array) ->
                    let*** (), state =
                      if place_contains_inaccessible parent then
                        evaluated () state
                      else write_place parent (Value.array array) state
                    in
                    evaluated (Array_element (parent, key)) state
                | None ->
                    raise_runtime_error state
                      {
                        Error.class_name = "Error";
                        message =
                          "Cannot add element to the array as the next element \
                           is already occupied";
                      })
          | Some expression -> (
              let*** value, state =
                eval_expression functions state expression
              in
              let*** array, state =
                match (access, read_place state parent) with
                | Unset, Value.Array array -> evaluated (Some array) state
                | Unset, _ -> evaluated (Some Value.empty_array) state
                | (Read | Write), value ->
                    array_for_access ~for_write:(access = Write) value state
              in
              match array with
              | None -> evaluated Invalid_read state
              | Some array ->
                  let*** key, state = normalized_array_key value state in
                  let*** key, state =
                    resolve_array_key ~for_write:(access = Write) array key
                      state
                  in
                  let place = Array_element (parent, key) in
                  if
                    access <> Read
                    || Value.kind (read_place state place) <> `Undefined
                  then evaluated place state
                  else
                    let key = Format.asprintf "%a" Value.pp_array_key key in
                    let*** (), state =
                      record_runtime_event Error.Runtime_event.Warning
                        ("Undefined array key " ^ key)
                        state
                    in
                    evaluated Invalid_read state)
        in
        evaluated place state
    | Static_property_lvalue (class_name, name) -> (
        match resolve_class_reference functions state class_name with
        | None ->
            raise_runtime_error state
              {
                Error.class_name = "Error";
                message =
                  "Cannot access parent when current class scope has no parent";
              }
        | Some class_name -> (
            match
              Class_map.find_opt
                (Builtins.canonical_name class_name)
                functions.classes
            with
            | None ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message = Printf.sprintf "Class %S not found" class_name;
                  }
            | Some class_ -> (
                match find_property_from ~static:true functions class_ name with
                | None ->
                    raise_runtime_error state
                      {
                        Error.class_name = "Error";
                        message =
                          Printf.sprintf
                            "Access to undeclared static property %s::$%s"
                            class_.name name;
                      }
                | Some property
                  when not
                         (member_accessible functions state
                            property.declaring_class
                            property.declaration.modifiers) ->
                    raise_runtime_error state
                      (inaccessible_property_error property)
                | Some property ->
                    evaluated
                      (Static_property
                         (property.declaring_class, property.declaration.name))
                      state)))
    | Object_property_lvalue (object_, name) -> (
        let*** object_, state =
          resolve_lvalue functions state ~access:Read object_
        in
        match read_place state object_ with
        | Value.Object object_id -> (
            let object_state =
              match State.find_object object_id state with
              | Some object_ -> object_
              | None -> failwith "PHP object value refers to an unknown object"
            in
            let class_ =
              Class_map.find_opt
                (Builtins.canonical_name object_state.class_name)
                functions.classes
            in
            let property =
              Option.bind class_ (fun class_ ->
                  find_property functions state class_ name)
            in
            let property_key =
              match property with
              | Some property ->
                  State.declared_property
                    ~declaring_class:property.declaring_class
                    property.declaration.name
              | None -> State.dynamic_property name
            in
            let place = Object_property (object_id, property_key) in
            let accessible =
              Option.fold ~none:true
                ~some:(fun property ->
                  member_accessible functions state property.declaring_class
                    property.declaration.modifiers)
                property
            in
            let available =
              accessible
              && Option.is_some
                   (State.find_object_property_cell object_id property_key state)
            in
            let magic method_name =
              Option.bind class_ (fun class_ ->
                  find_method_from functions class_ method_name)
            in
            match (access, available) with
            | Read, false -> (
                match magic "__get" with
                | Some method_ ->
                    let*** value, state =
                      call_property_magic functions state lvalue.location
                        object_id name method_
                        [ Value.string name ]
                    in
                    evaluated (Temporary value) state
                | None when not accessible ->
                    raise_runtime_error state
                      (inaccessible_property_error (Option.get property))
                | None ->
                    let*** (), state =
                      record_runtime_event Error.Runtime_event.Warning
                        (Printf.sprintf "Undefined property: %s::$%s"
                           object_state.class_name name)
                        state
                    in
                    evaluated Invalid_read state)
            | Write, false -> (
                match magic "__set" with
                | Some method_ ->
                    evaluated (Magic_set (object_id, name, method_)) state
                | None when not accessible ->
                    let error =
                      inaccessible_property_error (Option.get property)
                    in
                    evaluated (Inaccessible_property (place, error)) state
                | None ->
                    let*** (), state =
                      match property with
                      | Some _ -> evaluated () state
                      | None ->
                          record_runtime_event Error.Runtime_event.Deprecation
                            (Printf.sprintf
                               "Creation of dynamic property %s::$%s is \
                                deprecated"
                               object_state.class_name name)
                            state
                    in
                    evaluated place state)
            | Unset, false -> (
                match magic "__unset" with
                | Some method_ ->
                    evaluated (Magic_unset (object_id, name, method_)) state
                | None when not accessible ->
                    raise_runtime_error state
                      (inaccessible_property_error (Option.get property))
                | None -> evaluated place state)
            | (Read | Write | Unset), true -> evaluated place state)
        | value -> (
            match access with
            | Read ->
                let*** (), state =
                  record_runtime_event Error.Runtime_event.Warning
                    (Printf.sprintf "Attempt to read property %S on %s" name
                       (Value.type_name value))
                    state
                in
                evaluated Invalid_read state
            | Write ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message =
                      Printf.sprintf "Attempt to assign property %S on %s" name
                        (Value.type_name value);
                  }
            | Unset -> evaluated Invalid_read state))
  in
  Phpsymex.with_location ~location:lvalue.location process

and call_property_magic declarations state location object_id property method_
    arguments =
  let method_name = method_.declaration.Php_ir.name in
  if State.magic_property_is_active object_id property method_name state then
    unsupported "recursive %s for property %s::$%s" method_name
      (State.find_object object_id state |> Option.get).class_name property
  else
    let state =
      State.enter_magic_property object_id property method_name state
    in
    let open Phpsymex.Syntax in
    let** result, state =
      call_method declarations state location object_id method_ arguments
    in
    Phpsymex.Result.ok
      (result, State.leave_magic_property object_id property method_name state)

and write_resolved_place declarations state location place value =
  match place with
  | Magic_set (object_id, name, method_) ->
      let open Phpsymex.Syntax in
      let*** _, state =
        call_property_magic declarations state location object_id name method_
          [ Value.string name; value ]
      in
      evaluated () state
  | Variable _ | Array_element _ | Object_property _ | Static_property _
  | Temporary _ | Magic_unset _ | Inaccessible_property _ | Invalid_read ->
      write_place place value state

and unset_resolved_place declarations state location place =
  match place with
  | Magic_unset (object_id, name, method_) ->
      let open Phpsymex.Syntax in
      let*** _, state =
        call_property_magic declarations state location object_id name method_
          [ Value.string name ]
      in
      evaluated () state
  | Variable _ | Array_element _ | Object_property _ | Static_property _
  | Temporary _ | Magic_set _ | Inaccessible_property _ | Invalid_read ->
      unset_place place state

and read_lvalue_quiet declarations state (lvalue : Php_ir.lvalue) =
  let process =
    let open Phpsymex.Syntax in
    match lvalue.desc with
    | Variable_lvalue name ->
        evaluated
          (State.find_variable name state |> Option.value ~default:Value.undef)
          state
    | Array_element_lvalue (parent, Some key_expression) -> (
        let*** parent, state = read_lvalue_quiet declarations state parent in
        let*** key_value, state =
          eval_expression declarations state key_expression
        in
        let*** key, state = normalized_array_key key_value state in
        match parent with
        | Value.Array array ->
            let*** key, state =
              resolve_array_key ~for_write:false array key state
            in
            evaluated
              (State.find_array_value key array state
              |> Option.value ~default:Value.undef)
              state
        | Value.Undef | Null | Bool _ | Int _ | Float _ | String _ | Object _
        | Callable _ ->
            evaluated Value.undef state)
    | Array_element_lvalue (_, None) ->
        failwith "isset lvalue contains an array append"
    | Object_property_lvalue (object_, name) -> (
        let*** object_, state = read_lvalue_quiet declarations state object_ in
        match object_ with
        | Value.Object object_id -> (
            let object_state =
              State.find_object object_id state |> Option.get
            in
            let class_ =
              Class_map.find_opt
                (Builtins.canonical_name object_state.class_name)
                declarations.classes
            in
            let property =
              Option.bind class_ (fun class_ ->
                  find_property declarations state class_ name)
            in
            let property_key =
              match property with
              | Some property ->
                  State.declared_property
                    ~declaring_class:property.declaring_class
                    property.declaration.name
              | None -> State.dynamic_property name
            in
            let accessible =
              Option.fold ~none:true
                ~some:(fun property ->
                  member_accessible declarations state property.declaring_class
                    property.declaration.modifiers)
                property
            in
            let value =
              if accessible then
                State.find_object_property object_id property_key state
              else None
            in
            match value with
            | Some value -> evaluated value state
            | None -> (
                let magic method_name =
                  Option.bind class_ (fun class_ ->
                      find_method_from declarations class_ method_name)
                in
                let read state =
                  match magic "__get" with
                  | Some method_ ->
                      call_property_magic declarations state lvalue.location
                        object_id name method_
                        [ Value.string name ]
                  | None -> evaluated Value.undef state
                in
                match magic "__isset" with
                | None -> read state
                | Some method_ ->
                    let*** value, state =
                      call_property_magic declarations state lvalue.location
                        object_id name method_
                        [ Value.string name ]
                    in
                    let** guard = condition value in
                    if%sat[@lname "Read isset property"]
                          [@rname "Short-circuit unset property"]
                      guard
                    then read state
                    else evaluated Value.undef state))
        | Value.Undef | Null | Bool _ | Int _ | Float _ | String _ | Array _
        | Callable _ ->
            evaluated Value.undef state)
    | Static_property_lvalue (class_name, name) -> (
        match resolve_class_reference declarations state class_name with
        | None -> evaluated Value.undef state
        | Some class_name -> (
            match
              Class_map.find_opt
                (Builtins.canonical_name class_name)
                declarations.classes
            with
            | None ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message = Printf.sprintf "Class %S not found" class_name;
                  }
            | Some class_ -> (
                match
                  find_property_from ~static:true declarations class_ name
                with
                | None -> evaluated Value.undef state
                | Some property
                  when not
                         (member_accessible declarations state
                            property.declaring_class
                            property.declaration.modifiers) ->
                    evaluated Value.undef state
                | Some property ->
                    evaluated
                      (State.find_static_property
                         ~declaring_class:property.declaring_class
                         property.declaration.name state
                      |> Option.value ~default:Value.undef)
                      state)))
  in
  Phpsymex.with_location ~location:lvalue.location process

and eval_isset_lvalue declarations state (lvalue : Php_ir.lvalue) =
  match lvalue.desc with
  | Object_property_lvalue (object_, name) -> (
      let open Phpsymex.Syntax in
      let*** object_, state = read_lvalue_quiet declarations state object_ in
      match object_ with
      | Value.Object object_id -> (
          let object_state = State.find_object object_id state |> Option.get in
          let class_ =
            Class_map.find_opt
              (Builtins.canonical_name object_state.class_name)
              declarations.classes
          in
          let property =
            Option.bind class_ (fun class_ ->
                find_property declarations state class_ name)
          in
          let property_key =
            match property with
            | Some property ->
                State.declared_property
                  ~declaring_class:property.declaring_class
                  property.declaration.name
            | None -> State.dynamic_property name
          in
          let accessible =
            Option.fold ~none:true
              ~some:(fun property ->
                member_accessible declarations state property.declaring_class
                  property.declaration.modifiers)
              property
          in
          let value =
            if accessible then
              State.find_object_property object_id property_key state
            else None
          in
          match value with
          | Some (Value.Undef | Null) -> evaluated (Value.bool false) state
          | Some _ -> evaluated (Value.bool true) state
          | None -> (
              match
                Option.bind class_ (fun class_ ->
                    find_method_from declarations class_ "__isset")
              with
              | None -> evaluated (Value.bool false) state
              | Some method_ ->
                  let*** value, state =
                    call_property_magic declarations state lvalue.location
                      object_id name method_
                      [ Value.string name ]
                  in
                  let** value = condition value in
                  evaluated (Value.Bool value) state))
      | Value.Undef | Null | Bool _ | Int _ | Float _ | String _ | Array _
      | Callable _ ->
          evaluated (Value.bool false) state)
  | Variable_lvalue _ | Array_element_lvalue _ | Static_property_lvalue _ ->
      let open Phpsymex.Syntax in
      let*** value, state = read_lvalue_quiet declarations state lvalue in
      evaluated
        (Value.bool
           (match value with Value.Undef | Null -> false | _ -> true))
        state

and eval_isset_lvalues declarations state lvalues =
  let open Phpsymex.Syntax in
  match lvalues with
  | [] -> evaluated (Value.bool true) state
  | lvalue :: lvalues ->
      let*** value, state = eval_isset_lvalue declarations state lvalue in
      let** guard = condition value in
      if%sat[@lname "Evaluate next isset target"] [@rname "Short-circuit isset"]
        guard
      then eval_isset_lvalues declarations state lvalues
      else evaluated (Value.bool false) state

and coerce_to_string declarations state location value =
  match value with
  | Value.Object object_id -> (
      let object_state = State.find_object object_id state |> Option.get in
      let class_ =
        Class_map.find_opt
          (Builtins.canonical_name object_state.class_name)
          declarations.classes
      in
      match
        Option.bind class_ (fun class_ ->
            find_method_from declarations class_ "__toString")
      with
      | None when class_is_a declarations object_state.class_name "Throwable" ->
          unsupported "built-in Throwable::__toString"
      | None ->
          raise_runtime_error state
            {
              Error.class_name = "Error";
              message =
                Printf.sprintf
                  "Object of class %s could not be converted to string"
                  object_state.class_name;
            }
      | Some method_ -> (
          let open Phpsymex.Syntax in
          let*** value, state =
            call_method declarations state location object_id method_ []
          in
          match Coercion.coerce Coercion.String value with
          | Ok (Value.String _ as value) -> evaluated value state
          | Ok _ -> assert false
          | Error _ ->
              raise_runtime_error state
                {
                  Error.class_name = "TypeError";
                  message =
                    Printf.sprintf
                      "%s::__toString(): Return value must be of type string, \
                       %s returned"
                      method_.declaring_class (Value.type_name value);
                }))
  | Value.Undef | Null | Bool _ | Int _ | Float _ | String _ | Array _
  | Callable _ ->
      let open Phpsymex.Syntax in
      let** value = coerce Coercion.String value in
      evaluated value state

and eval_short_circuit functions state left operator right =
  let open Phpsymex.Syntax in
  let*** left, state = eval_expression functions state left in
  let** guard = condition left in
  match operator with
  | Php_ir.Boolean_and ->
      if%sat[@lname "Evaluate right operand"] [@rname "Short-circuit false"]
        guard
      then
        let*** right, state = eval_expression functions state right in
        let** right = condition right in
        evaluated (Value.Bool right) state
      else evaluated (Value.bool false) state
  | Boolean_or ->
      if%sat[@lname "Short-circuit true"] [@rname "Evaluate right operand"]
        guard
      then evaluated (Value.bool true) state
      else
        let*** right, state = eval_expression functions state right in
        let** right = condition right in
        evaluated (Value.Bool right) state
  | _ -> failwith "non-short-circuit operator passed to evaluation"

and eval_expression functions state expression =
  let process =
    let open Phpsymex.Syntax in
    let* () = Phpsymex.consume_fuel_steps 1 in
    match expression.Php_ir.desc with
    | Literal literal -> evaluated (Value.of_literal literal) state
    | Array items -> eval_array_items functions state Value.empty_array items
    | Variable name -> (
        match State.find_variable name state with
        | Some value when Value.kind value <> `Undefined ->
            evaluated value state
        | (Some _ | None) when String.equal name "this" ->
            raise_runtime_error state
              {
                Error.class_name = "Error";
                message = "Using $this when not in object context";
              }
        | Some _ | None ->
            let*** (), state =
              record_runtime_event Error.Runtime_event.Warning
                (Printf.sprintf "Undefined variable $%s" name)
                state
            in
            evaluated Value.null state)
    | Array_get target ->
        let*** place, state =
          resolve_lvalue functions state ~access:Read target
        in
        evaluated (read_place state place) state
    | Property_get target ->
        let*** place, state =
          resolve_lvalue functions state ~access:Read target
        in
        evaluated (read_place state place) state
    | Isset targets -> eval_isset_lvalues functions state targets
    | Assign (target, expression) ->
        let*** place, state =
          resolve_lvalue functions state ~access:Write target
        in
        let*** value, state = eval_expression functions state expression in
        let*** (), state =
          write_resolved_place functions state target.location place value
        in
        evaluated value state
    | Assign_reference (target, source) ->
        let*** target, state =
          resolve_lvalue functions state ~access:Write target
        in
        let*** source, state =
          resolve_lvalue functions state ~access:Write source
        in
        let*** cell, state = cell_for_reference source state in
        let*** (), state = bind_place_reference target cell state in
        let value =
          State.find_cell cell state |> Option.value ~default:Value.undef
        in
        evaluated value state
    | Unary (operator, expression) ->
        let*** value, state = eval_expression functions state expression in
        let** operation = unary operator value in
        apply_operation operation state
    | Binary (left, ((Boolean_and | Boolean_or) as operator), right) ->
        eval_short_circuit functions state left operator right
    | Binary (left, Concat, right) ->
        let*** left, state = eval_expression functions state left in
        let*** right, state = eval_expression functions state right in
        let*** left, state =
          coerce_to_string functions state expression.location left
        in
        let*** right, state =
          coerce_to_string functions state expression.location right
        in
        evaluated (Value.string (string_value left ^ string_value right)) state
    | Binary (left, operator, right) ->
        let*** left, state = eval_expression functions state left in
        let*** right, state = eval_expression functions state right in
        let** operation = binary state operator left right in
        apply_operation operation state
    | Cast (cast, expression) -> (
        let*** value, state = eval_expression functions state expression in
        match cast with
        | Php_ir.To_string ->
            coerce_to_string functions state expression.location value
        | To_boolean | To_integer | To_float ->
            let target =
              match cast with
              | To_boolean -> Coercion.Boolean
              | To_integer -> Coercion.Integer
              | To_float -> Coercion.Float
              | To_string -> assert false
            in
            let** value = coerce target value in
            evaluated value state)
    | Call (name, arguments) ->
        let*** arguments, state = eval_expressions functions state arguments in
        invoke_callable functions state expression.location
          (Value.Function name) arguments
    | Invoke (callee, arguments) -> (
        let*** callee, state = eval_expression functions state callee in
        match callee with
        | Value.Callable callable ->
            let*** arguments, state =
              eval_expressions functions state arguments
            in
            invoke_callable functions state expression.location callable
              arguments
        | Value.String _ -> unsupported "string callable invocation"
        | Value.Array _ -> unsupported "array callable invocation"
        | Value.Object _ -> unsupported "invokable object"
        | value ->
            raise_runtime_error state
              {
                Error.class_name = "Error";
                message =
                  Printf.sprintf "Value of type %s is not callable"
                    (runtime_type_name state value);
              })
    | Function_callable name ->
        if
          is_object_builtin name
          || Option.is_some (Builtins.find name)
          || Function_map.mem (Builtins.canonical_name name) functions.functions
        then
          let identity, state = State.fresh_callable_id state in
          evaluated
            (Value.callable (Value.First_class_function { identity; name }))
            state
        else
          raise_runtime_error state
            {
              Error.class_name = "Error";
              message = Printf.sprintf "Call to undefined function %s()" name;
            }
    | Method_call (object_expression, name, arguments) -> (
        let*** object_value, state =
          eval_expression functions state object_expression
        in
        match object_value with
        | Value.Object object_id -> (
            let object_state =
              match State.find_object object_id state with
              | Some object_ -> object_
              | None -> failwith "PHP object value refers to an unknown object"
            in
            let class_ =
              Class_map.find_opt
                (Builtins.canonical_name object_state.class_name)
                functions.classes
            in
            let method_ =
              Option.bind class_ (fun class_ ->
                  find_method functions state class_ name)
            in
            let magic =
              Option.bind class_ (fun class_ ->
                  find_method_from functions class_ "__call")
            in
            match method_ with
            | Some method_
              when member_accessible functions state method_.declaring_class
                     method_.declaration.modifiers ->
                let*** arguments, state =
                  eval_expressions functions state arguments
                in
                if member_is_static method_.declaration.modifiers then
                  call_static_method functions state expression.location
                    object_state.class_name method_ arguments
                else
                  call_method functions state expression.location object_id
                    method_ arguments
            | Some inaccessible -> (
                match magic with
                | None ->
                    raise_runtime_error state
                      (inaccessible_method_error state inaccessible)
                | Some magic ->
                    let*** arguments, state =
                      eval_expressions functions state arguments
                    in
                    call_method functions state expression.location object_id
                      magic
                      [ Value.string name; array_of_values arguments ])
            | None -> (
                match magic with
                | None ->
                    raise_runtime_error state
                      {
                        Error.class_name = "Error";
                        message =
                          Printf.sprintf "Call to undefined method %s::%s()"
                            object_state.class_name name;
                      }
                | Some magic ->
                    let*** arguments, state =
                      eval_expressions functions state arguments
                    in
                    call_method functions state expression.location object_id
                      magic
                      [ Value.string name; array_of_values arguments ]))
        | value ->
            raise_runtime_error state
              {
                Error.class_name = "Error";
                message =
                  Printf.sprintf "Call to a member function %s() on %s" name
                    (Value.type_name value);
              })
    | Object_method_callable (object_expression, name) -> (
        let*** object_value, state =
          eval_expression functions state object_expression
        in
        match object_value with
        | Value.Object object_id -> (
            let object_state =
              State.find_object object_id state |> Option.get
            in
            let class_ =
              Class_map.find_opt
                (Builtins.canonical_name object_state.class_name)
                functions.classes
            in
            match
              Option.bind class_ (fun class_ ->
                  find_method functions state class_ name)
            with
            | Some method_
              when member_accessible functions state method_.declaring_class
                     method_.declaration.modifiers ->
                let identity, state = State.fresh_callable_id state in
                evaluated
                  (Value.callable
                     (Value.Object_method
                        {
                          identity;
                          object_id;
                          declaring_class = method_.declaring_class;
                          method_name = method_.declaration.name;
                        }))
                  state
            | Some method_ ->
                if
                  Option.exists
                    (fun class_ ->
                      Option.is_some
                        (find_method_from functions class_ "__call"))
                    class_
                then unsupported "first-class callable through __call"
                else
                  raise_runtime_error state
                    (inaccessible_method_error state method_)
            | None ->
                if
                  Option.exists
                    (fun class_ ->
                      Option.is_some
                        (find_method_from functions class_ "__call"))
                    class_
                then unsupported "first-class callable through __call"
                else
                  raise_runtime_error state
                    {
                      Error.class_name = "Error";
                      message =
                        Printf.sprintf "Call to undefined method %s::%s()"
                          object_state.class_name name;
                    })
        | value ->
            raise_runtime_error state
              {
                Error.class_name = "Error";
                message =
                  Printf.sprintf "Call to a member function %s() on %s" name
                    (Value.type_name value);
              })
    | Static_method_call (class_name, name, arguments) ->
        call_static_expression functions state expression.location class_name
          name arguments
    | Static_method_callable (class_name, name) ->
        let*** (class_name, (method_ : method_member)), state =
          resolve_static_method functions state class_name name
        in
        let identity, state = State.fresh_callable_id state in
        evaluated
          (Value.callable
             (Value.Static_method
                {
                  identity;
                  called_class = class_name;
                  declaring_class = method_.declaring_class;
                  method_name = method_.declaration.name;
                }))
          state
    | Closure closure ->
        let*** captures, state =
          capture_closure_variables functions state closure.captures
        in
        let captures, state =
          match State.find_variable "this" state with
          | Some value ->
              ( State.String_map.add "this" (State.By_value value) captures,
                state )
          | None -> (captures, state)
        in
        let id, state =
          State.allocate_closure closure captures
            (State.current_class_context state)
            (State.current_called_class state)
            state
        in
        evaluated (Value.callable (Value.Closure id)) state
    | Parent_method_call (name, arguments) -> (
        match
          (State.current_class_context state, State.find_variable "this" state)
        with
        | Some current_class, Some (Value.Object object_id) -> (
            match
              Class_map.find_opt
                (Builtins.canonical_name current_class)
                functions.classes
            with
            | Some { parent = Some parent; _ } -> (
                match
                  Class_map.find_opt
                    (Builtins.canonical_name parent)
                    functions.classes
                with
                | Some parent -> (
                    match find_method_from functions parent name with
                    | Some method_ ->
                        if
                          member_accessible functions state
                            method_.declaring_class
                            method_.declaration.modifiers
                        then
                          let*** arguments, state =
                            eval_expressions functions state arguments
                          in
                          if member_is_static method_.declaration.modifiers then
                            let called_class =
                              State.current_called_class state
                              |> Option.value ~default:parent.name
                            in
                            call_static_method functions state
                              expression.location called_class method_ arguments
                          else
                            call_method functions state expression.location
                              object_id method_ arguments
                        else
                          raise_runtime_error state
                            (inaccessible_method_error state method_)
                    | None -> (
                        match find_builtin_parent functions parent with
                        | Some { constructible = true; name = builtin; _ }
                          when String.equal
                                 (Builtins.canonical_name name)
                                 "__construct" ->
                            let*** arguments, state =
                              eval_expressions functions state arguments
                            in
                            let** message =
                              throwable_message builtin arguments
                            in
                            evaluated Value.null
                              (State.set_object_message object_id message state)
                        | Some _ | None ->
                            raise_runtime_error state
                              {
                                Error.class_name = "Error";
                                message =
                                  Printf.sprintf
                                    "Call to undefined method %s::%s()"
                                    parent.name name;
                              }))
                | None -> (
                    match find_throwable_class parent with
                    | Some { constructible = true; _ }
                      when String.equal
                             (Builtins.canonical_name name)
                             "__construct" ->
                        let*** arguments, state =
                          eval_expressions functions state arguments
                        in
                        let** message = throwable_message parent arguments in
                        evaluated Value.null
                          (State.set_object_message object_id message state)
                    | Some _ | None ->
                        unsupported "parent method call into built-in class %s"
                          parent))
            | Some { parent = None; _ } | None ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message =
                      "Cannot access parent when current class scope has no \
                       parent";
                  })
        | Some current_class, _ -> (
            match
              Class_map.find_opt
                (Builtins.canonical_name current_class)
                functions.classes
            with
            | Some { parent = Some parent_name; _ } -> (
                match
                  Class_map.find_opt
                    (Builtins.canonical_name parent_name)
                    functions.classes
                with
                | Some parent -> (
                    match find_method_from functions parent name with
                    | Some method_
                      when member_is_static method_.declaration.modifiers
                           && member_accessible functions state
                                method_.declaring_class
                                method_.declaration.modifiers ->
                        let*** arguments, state =
                          eval_expressions functions state arguments
                        in
                        let called_class =
                          State.current_called_class state
                          |> Option.value ~default:parent.name
                        in
                        call_static_method functions state expression.location
                          called_class method_ arguments
                    | Some method_
                      when not (member_is_static method_.declaration.modifiers)
                      ->
                        raise_runtime_error state
                          {
                            Error.class_name = "Error";
                            message =
                              Printf.sprintf
                                "Non-static method %s::%s() cannot be called \
                                 statically"
                                method_.declaring_class method_.declaration.name;
                          }
                    | Some method_ ->
                        raise_runtime_error state
                          (inaccessible_method_error state method_)
                    | None ->
                        raise_runtime_error state
                          {
                            Error.class_name = "Error";
                            message =
                              Printf.sprintf "Call to undefined method %s::%s()"
                                parent.name name;
                          })
                | None ->
                    unsupported "parent method call into built-in class %s"
                      parent_name)
            | Some { parent = None; _ } | None ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message =
                      "Cannot access parent when current class scope has no \
                       parent";
                  })
        | None, _ -> unsupported "parent method call outside a method")
    | New (name, arguments) -> (
        match find_throwable_class name with
        | Some _ ->
            let*** arguments, state =
              eval_expressions functions state arguments
            in
            construct_throwable state name arguments
        | None -> (
            match
              Class_map.find_opt
                (Builtins.canonical_name name)
                functions.classes
            with
            | None -> unsupported "object construction for class %s" name
            | Some class_ when class_.kind <> Php_ir.Class ->
                unsupported "construction of %s %s"
                  (match class_.kind with
                  | Interface -> "interface"
                  | Trait -> "trait"
                  | Class -> assert false)
                  class_.name
            | Some class_ -> (
                match find_method_from functions class_ "__construct" with
                | Some constructor
                  when not
                         (member_accessible functions state
                            constructor.declaring_class
                            constructor.declaration.modifiers) ->
                    raise_runtime_error state
                      (inaccessible_method_error state constructor)
                | Some _ | None ->
                    let*** arguments, state =
                      eval_expressions functions state arguments
                    in
                    construct_object functions state expression.location name
                      arguments)))
    | Clone operand -> (
        let*** value, state = eval_expression functions state operand in
        match value with
        | Value.Object object_id -> (
            let object_ =
              match State.find_object object_id state with
              | Some object_ -> object_
              | None -> failwith "PHP object value refers to an unknown object"
            in
            match
              Class_map.find_opt
                (Builtins.canonical_name object_.class_name)
                functions.classes
            with
            | None ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message =
                      Printf.sprintf
                        "Trying to clone an uncloneable object of class %s"
                        object_.class_name;
                  }
            | Some _ when class_is_a functions object_.class_name "Throwable" ->
                raise_runtime_error state
                  {
                    Error.class_name = "Error";
                    message =
                      Printf.sprintf
                        "Trying to clone an uncloneable object of class %s"
                        object_.class_name;
                  }
            | Some class_ -> (
                match find_method_from functions class_ "__clone" with
                | Some method_
                  when not
                         (member_accessible functions state
                            method_.declaring_class
                            method_.declaration.modifiers) ->
                    raise_runtime_error state
                      (inaccessible_method_error state method_)
                | clone_method -> (
                    let clone_id, state = State.clone_object object_id state in
                    let clone = Value.object_ clone_id in
                    match clone_method with
                    | None -> evaluated clone state
                    | Some method_ -> (
                        let** result, state =
                          call_method functions state expression.location
                            clone_id method_ []
                        in
                        match result with
                        | Evaluated _ -> evaluated clone state
                        | Raised thrown ->
                            Phpsymex.Result.ok (Raised thrown, state)))))
        | Value.Undef | Null | Bool _ | Int _ | Float _ | String _ | Array _
        | Callable _ ->
            raise_runtime_error state
              {
                Error.class_name = "Error";
                message = "__clone method called on non-object";
              })
    | Throw expression ->
        let*** value, state = eval_expression functions state expression in
        raise_value ~declarations:functions value state
  in
  Phpsymex.with_location ~location:expression.location process

and capture_closure_variables declarations state captures =
  let open Phpsymex.Syntax in
  match captures with
  | [] -> evaluated State.String_map.empty state
  | (capture : Php_ir.closure_capture) :: captures ->
      let process =
        let*** binding, state =
          if capture.by_reference then
            let cell, state = State.ensure_variable capture.name state in
            let state =
              match State.find_cell cell state with
              | Some Value.Undef -> State.set_cell cell Value.null state
              | Some _ -> state
              | None -> failwith "closure capture refers to an unknown cell"
            in
            evaluated (State.By_reference cell) state
          else
            let*** value, state =
              match State.find_variable capture.name state with
              | Some value when Value.kind value <> `Undefined ->
                  evaluated value state
              | Some _ | None ->
                  let*** (), state =
                    record_runtime_event Error.Runtime_event.Warning
                      (Printf.sprintf "Undefined variable $%s" capture.name)
                      state
                  in
                  evaluated Value.null state
            in
            evaluated (State.By_value value) state
        in
        let*** environment, state =
          capture_closure_variables declarations state captures
        in
        evaluated (State.String_map.add capture.name binding environment) state
      in
      Phpsymex.with_location ~location:capture.location process

and resolve_static_method declarations state class_reference name =
  match resolve_class_reference declarations state class_reference with
  | None ->
      raise_runtime_error state
        {
          Error.class_name = "Error";
          message =
            "Cannot access parent when current class scope has no parent";
        }
  | Some class_name -> (
      match
        Class_map.find_opt
          (Builtins.canonical_name class_name)
          declarations.classes
      with
      | None ->
          raise_runtime_error state
            {
              Error.class_name = "Error";
              message = Printf.sprintf "Class %S not found" class_name;
            }
      | Some class_ -> (
          match
            (find_method declarations state class_ name : method_member option)
          with
          | None ->
              raise_runtime_error state
                {
                  Error.class_name = "Error";
                  message =
                    Printf.sprintf "Call to undefined method %s::%s()"
                      class_.name name;
                }
          | Some method_
            when not (member_is_static method_.declaration.modifiers) ->
              raise_runtime_error state
                {
                  Error.class_name = "Error";
                  message =
                    Printf.sprintf
                      "Non-static method %s::%s() cannot be called statically"
                      method_.declaring_class method_.declaration.name;
                }
          | Some method_
            when not
                   (member_accessible declarations state method_.declaring_class
                      method_.declaration.modifiers) ->
              raise_runtime_error state
                (inaccessible_method_error state method_)
          | Some (method_ : method_member) ->
              evaluated (class_.name, method_) state))

and call_static_expression declarations state location class_name name arguments
    =
  let open Phpsymex.Syntax in
  let*** (called_class, (method_ : method_member)), state =
    resolve_static_method declarations state class_name name
  in
  let*** arguments, state = eval_expressions declarations state arguments in
  call_static_method declarations state location called_class method_ arguments

and call_object_builtin declarations state name arguments =
  let name = Builtins.canonical_name name in
  let invalid_count expected =
    raise_runtime_error state
      {
        Error.class_name = "ArgumentCountError";
        message =
          Printf.sprintf "%s() expects exactly %d argument%s, %d given" name
            expected
            (if expected = 1 then "" else "s")
            (List.length arguments);
      }
  in
  let invalid_type position expected value =
    raise_runtime_error state
      {
        Error.class_name = "TypeError";
        message =
          Printf.sprintf "%s(): Argument #%d must be of type %s, %s given" name
            position expected (Value.type_name value);
      }
  in
  let class_info name =
    Class_map.find_opt (Builtins.canonical_name name) declarations.classes
  in
  let object_class id =
    State.find_object id state |> Option.get |> fun object_ ->
    object_.class_name
  in
  match (name, arguments) with
  | "get_class", [ Value.Object id ] ->
      evaluated (Value.string (object_class id)) state
  | "get_class", [ value ] -> invalid_type 1 "object" value
  | "get_class", _ -> invalid_count 1
  | "is_a", [ subject; expected ] ->
      call_object_builtin declarations state name
        [ subject; expected; Value.bool false ]
  | "is_a", [ subject; Value.String expected; Value.Bool allow_string ] -> (
      match Value.Typed.Bool.to_bool allow_string with
      | None -> unsupported "symbolic is_a() allow_string argument"
      | Some allow_string ->
          let result =
            match subject with
            | Value.Object id ->
                builtin_is_a declarations (object_class id) expected
            | Value.String actual
              when allow_string && known_class declarations actual ->
                builtin_is_a declarations actual expected
            | Value.Undef | Null | Bool _ | Int _ | Float _ | String _ | Array _
            | Callable _ ->
                false
          in
          evaluated (Value.bool result) state)
  | "is_a", [ _; (Value.String _ as expected); value ] ->
      let _ = expected in
      invalid_type 3 "bool" value
  | "is_a", [ _; value; _ ] -> invalid_type 2 "string" value
  | "is_a", arguments ->
      let count = List.length arguments in
      raise_runtime_error state
        {
          Error.class_name = "ArgumentCountError";
          message =
            Printf.sprintf "is_a() expects between 2 and 3 arguments, %d given"
              count;
        }
  | ("property_exists" | "method_exists"), [ subject; Value.String member_name ]
    -> (
      match subject with
      | Value.Object id ->
          let class_name = object_class id in
          let result =
            match name with
            | "property_exists" ->
                let declared =
                  Option.fold ~none:false
                    ~some:(fun class_ ->
                      declared_property_exists declarations
                        ~include_private:true class_ member_name)
                    (class_info class_name)
                in
                declared
                || Option.is_some
                     (State.find_object_property_cell id
                        (State.dynamic_property member_name)
                        state)
            | "method_exists" ->
                Option.fold ~none:false
                  ~some:(fun class_ ->
                    Option.is_some
                      (find_method_from declarations class_ member_name))
                  (class_info class_name)
            | _ -> assert false
          in
          evaluated (Value.bool result) state
      | Value.String class_name ->
          let result =
            match class_info class_name with
            | None -> false
            | Some class_ -> (
                match name with
                | "property_exists" ->
                    declared_property_exists declarations ~include_private:true
                      class_ member_name
                | "method_exists" ->
                    Option.is_some
                      (find_method_from declarations class_ member_name)
                | _ -> assert false)
          in
          evaluated (Value.bool result) state
      | value -> invalid_type 1 "object|string" value)
  | ("property_exists" | "method_exists"), [ _; value ] ->
      invalid_type 2 "string" value
  | ("property_exists" | "method_exists"), _ -> invalid_count 2
  | _ -> failwith "non-object builtin passed to object builtin dispatch"

and invoke_callable declarations state location callable arguments =
  match callable with
  | Value.Function name -> (
      if is_object_builtin name then
        call_object_builtin declarations state name arguments
      else
        match Builtins.find name with
        | Some implementation -> (
            match Builtins.runtime_error name arguments with
            | Some error -> raise_runtime_error state error
            | None ->
                let open Phpsymex.Syntax in
                let** value = implementation ~args:arguments in
                evaluated value state)
        | None -> call_function declarations state location name arguments)
  | First_class_function { name; _ } ->
      invoke_callable declarations state location (Value.Function name)
        arguments
  | Static_method { called_class; declaring_class; method_name; _ } -> (
      match
        Class_map.find_opt
          (Builtins.canonical_name declaring_class)
          declarations.classes
      with
      | None -> failwith "callable has an unknown declaring PHP class"
      | Some class_ -> (
          match find_local_method class_ method_name with
          | Some method_ ->
              call_static_method declarations state location called_class
                method_ arguments
          | None -> failwith "callable refers to an unknown PHP static method"))
  | Object_method { object_id; declaring_class; method_name; _ } -> (
      match State.find_object object_id state with
      | None -> failwith "callable refers to an unknown PHP object"
      | Some object_ -> (
          match
            Class_map.find_opt
              (Builtins.canonical_name declaring_class)
              declarations.classes
          with
          | None -> failwith "callable object has an unknown PHP class"
          | Some class_ -> (
              match find_local_method class_ method_name with
              | None ->
                  raise_runtime_error state
                    {
                      Error.class_name = "Error";
                      message =
                        Printf.sprintf "Call to undefined method %s::%s()"
                          object_.class_name method_name;
                    }
              | Some method_ when member_is_static method_.declaration.modifiers
                ->
                  call_static_method declarations state location
                    object_.class_name method_ arguments
              | Some method_ ->
                  call_method declarations state location object_id method_
                    arguments)))
  | Closure id -> call_closure declarations state location id arguments

and initialize_static_properties declarations state classes =
  let open Phpsymex.Syntax in
  match classes with
  | [] -> evaluated () state
  | (_, class_) :: classes ->
      let properties =
        List.filter
          (fun (property : property_member) ->
            member_is_static property.declaration.modifiers)
          class_.properties
      in
      let*** (), state =
        initialize_class_static_properties declarations state properties
      in
      initialize_static_properties declarations state classes

and initialize_class_static_properties declarations state properties =
  let open Phpsymex.Syntax in
  match properties with
  | [] -> evaluated () state
  | (property : property_member) :: properties ->
      let*** value, state =
        match property.declaration.default with
        | None -> evaluated Value.null state
        | Some expression -> eval_expression declarations state expression
      in
      let state =
        State.allocate_static_property ~declaring_class:property.declaring_class
          property.declaration.name value state
      in
      initialize_class_static_properties declarations state properties

and call_function functions state location name arguments =
  let canonical_name = Builtins.canonical_name name in
  match Function_map.find_opt canonical_name functions.functions with
  | None -> unsupported "function %s" name
  | Some (function_ : Php_ir.function_decl) ->
      let expected = List.length function_.parameters in
      let actual = List.length arguments in
      if actual < expected then
        raise_runtime_error state
          {
            Error.class_name = "ArgumentCountError";
            message =
              Printf.sprintf "%s() expects exactly %d argument%s, %d given"
                function_.name expected
                (if expected = 1 then "" else "s")
                actual;
          }
      else
        let bindings = bind_parameters function_.parameters arguments in
        let local_state = State.enter_scope bindings state in
        let process =
          let open Phpsymex.Syntax in
          let** control, local_state =
            exec_statements functions local_state function_.body
          in
          let state = State.leave_scope local_state in
          match control with
          | Normal -> evaluated Value.null state
          | Return value -> evaluated value state
          | Throw thrown -> Phpsymex.Result.ok (Raised thrown, state)
          | Break _ | Continue _ ->
              failwith "loop control escaped a PHP function"
        in
        Phpsymex.with_call ~location
          ~message:("Call to " ^ function_.name)
          process

and call_static_method declarations state location called_class
    (method_ : method_member) arguments =
  let declaration = method_.declaration in
  let expected = List.length declaration.parameters in
  let actual = List.length arguments in
  if actual < expected then
    raise_runtime_error state
      {
        Error.class_name = "ArgumentCountError";
        message =
          Printf.sprintf "%s::%s() expects exactly %d argument%s, %d given"
            method_.declaring_class declaration.name expected
            (if expected = 1 then "" else "s")
            actual;
      }
  else
    let bindings = bind_parameters declaration.parameters arguments in
    let local_state =
      State.enter_scope ~class_context:(Some method_.declaring_class)
        ~called_class:(Some called_class) bindings state
    in
    let process =
      let open Phpsymex.Syntax in
      let** control, local_state =
        exec_statements declarations local_state (Option.get declaration.body)
      in
      let state = State.leave_scope local_state in
      match control with
      | Normal -> evaluated Value.null state
      | Return value -> evaluated value state
      | Throw thrown -> Phpsymex.Result.ok (Raised thrown, state)
      | Break _ | Continue _ ->
          failwith "loop control escaped a PHP static method"
    in
    Phpsymex.with_call ~location
      ~message:(Printf.sprintf "Call to %s::%s" called_class declaration.name)
      process

and call_closure declarations state location id arguments =
  match State.find_closure id state with
  | None -> failwith "callable refers to an unknown PHP closure"
  | Some closure ->
      let declaration = closure.declaration in
      let expected = List.length declaration.parameters in
      let actual = List.length arguments in
      if actual < expected then
        raise_runtime_error state
          {
            Error.class_name = "ArgumentCountError";
            message =
              Printf.sprintf "Closure expects exactly %d argument%s, %d given"
                expected
                (if expected = 1 then "" else "s")
                actual;
          }
      else
        let bindings = bind_parameters declaration.parameters arguments in
        let local_state =
          State.enter_closure_scope ~class_context:closure.class_context
            ~called_class:closure.called_class closure.captures bindings state
        in
        let process =
          let open Phpsymex.Syntax in
          let** control, local_state =
            exec_statements declarations local_state declaration.body
          in
          let state = State.leave_scope local_state in
          match control with
          | Normal -> evaluated Value.null state
          | Return value -> evaluated value state
          | Throw thrown -> Phpsymex.Result.ok (Raised thrown, state)
          | Break _ | Continue _ ->
              failwith "loop control escaped a PHP closure"
        in
        Phpsymex.with_call ~location ~message:"Call to closure" process

and call_method declarations state location object_id (method_ : method_member)
    arguments =
  let declaration = method_.declaration in
  let expected = List.length declaration.parameters in
  let actual = List.length arguments in
  if actual < expected then
    raise_runtime_error state
      {
        Error.class_name = "ArgumentCountError";
        message =
          Printf.sprintf "%s::%s() expects exactly %d argument%s, %d given"
            method_.declaring_class declaration.name expected
            (if expected = 1 then "" else "s")
            actual;
      }
  else
    let bindings =
      bind_parameters declaration.parameters arguments
      @ [ ("this", Value.object_ object_id) ]
    in
    let local_state =
      let called_class =
        State.find_object object_id state |> Option.get |> fun object_ ->
        Some object_.class_name
      in
      State.enter_scope ~class_context:(Some method_.declaring_class)
        ~called_class bindings state
    in
    let process =
      let open Phpsymex.Syntax in
      let** control, local_state =
        exec_statements declarations local_state (Option.get declaration.body)
      in
      let state = State.leave_scope local_state in
      match control with
      | Normal -> evaluated Value.null state
      | Return value -> evaluated value state
      | Throw thrown -> Phpsymex.Result.ok (Raised thrown, state)
      | Break _ | Continue _ -> failwith "loop control escaped a PHP method"
    in
    Phpsymex.with_call ~location
      ~message:
        (Printf.sprintf "Call to %s::%s" method_.declaring_class
           declaration.name)
      process

and emit_expressions functions state expressions =
  let open Phpsymex.Syntax in
  match expressions with
  | [] -> evaluated () state
  | expression :: expressions ->
      let*** value, state = eval_expression functions state expression in
      let* value = simplify_value value in
      let*** value, state =
        coerce_to_string functions state expression.location value
      in
      emit_expressions functions
        (State.emit (string_value value) state)
        expressions

and unset_lvalues functions state lvalues =
  let open Phpsymex.Syntax in
  match lvalues with
  | [] -> evaluated () state
  | lvalue :: lvalues ->
      let*** place, state =
        resolve_lvalue functions state ~access:Unset lvalue
      in
      let*** (), state =
        unset_resolved_place functions state lvalue.location place
      in
      unset_lvalues functions state lvalues

and exec_statements functions state statements =
  let open Phpsymex.Syntax in
  match statements with
  | [] -> Phpsymex.Result.ok (Normal, state)
  | statement :: statements -> (
      let** control, state = exec_statement functions state statement in
      match control with
      | Normal -> exec_statements functions state statements
      | Return _ | Break _ | Continue _ | Throw _ ->
          Phpsymex.Result.ok (control, state))

and exec_while functions state condition_expression body =
  let open Phpsymex.Syntax in
  finish_evaluation (eval_expression functions state condition_expression)
    (fun value state ->
      let** guard = condition value in
      if%sat[@lname "While body"] [@rname "While exit"] guard then
        let** control, state = exec_statements functions state body in
        match control with
        | Normal | Continue 1 ->
            exec_while functions state condition_expression body
        | Break 1 -> Phpsymex.Result.ok (Normal, state)
        | Break depth -> Phpsymex.Result.ok (Break (depth - 1), state)
        | Continue depth -> Phpsymex.Result.ok (Continue (depth - 1), state)
        | Return _ | Throw _ -> Phpsymex.Result.ok (control, state)
      else Phpsymex.Result.ok (Normal, state))

and assign_foreach_target functions state target value =
  let open Phpsymex.Syntax in
  let*** place, state = resolve_lvalue functions state ~access:Write target in
  write_resolved_place functions state target.location place value

and bind_foreach_target_reference functions state target cell =
  let open Phpsymex.Syntax in
  let*** place, state = resolve_lvalue functions state ~access:Write target in
  bind_place_reference place cell state

and foreach_key_value = function
  | Value.Integer_key key -> Value.int key
  | String_key key -> Value.string key
  | Symbolic_integer_key key -> Value.Int key

and exec_foreach_entries functions state key_target value_target body entries =
  let open Phpsymex.Syntax in
  match entries with
  | [] -> Phpsymex.Result.ok (Normal, state)
  | (key, entry) :: entries ->
      let key_value = foreach_key_value key in
      let value =
        match State.value_of_array_entry entry state with
        | Some value -> value
        | None -> failwith "foreach entry refers to an unknown PHP cell"
      in
      finish_evaluation
        (match key_target with
        | None -> evaluated () state
        | Some target -> assign_foreach_target functions state target key_value)
        (fun () state ->
          finish_evaluation
            (assign_foreach_target functions state value_target value)
            (fun () state ->
              let** control, state = exec_statements functions state body in
              match control with
              | Normal | Continue 1 ->
                  exec_foreach_entries functions state key_target value_target
                    body entries
              | Break 1 -> Phpsymex.Result.ok (Normal, state)
              | Break depth -> Phpsymex.Result.ok (Break (depth - 1), state)
              | Continue depth ->
                  Phpsymex.Result.ok (Continue (depth - 1), state)
              | Return _ | Throw _ -> Phpsymex.Result.ok (control, state)))

and foreach_iterable_lvalue (expression : Php_ir.expression) =
  match expression.desc with
  | Variable name ->
      Some
        { Php_ir.desc = Variable_lvalue name; location = expression.location }
  | Array_get target | Property_get target -> Some target
  | _ -> None

and eval_foreach_reference_iterable functions state iterable =
  match foreach_iterable_lvalue iterable with
  | None ->
      let open Phpsymex.Syntax in
      let*** value, state = eval_expression functions state iterable in
      evaluated (value, None) state
  | Some lvalue -> (
      let open Phpsymex.Syntax in
      let*** place, state =
        resolve_lvalue functions state ~access:Read lvalue
      in
      match place with
      | Temporary _ ->
          unsupported "foreach by reference over overloaded property"
      | Variable _ | Array_element _ | Object_property _ | Static_property _
      | Magic_set _ | Magic_unset _ | Inaccessible_property _ | Invalid_read ->
          evaluated (read_place state place, Some place) state)

and exec_foreach_reference_entries functions state source array key_target
    value_target body seen pending =
  let current_array state fallback =
    match source with
    | None -> Some fallback
    | Some place -> Value.array_value (read_place state place)
  in
  let keys array = List.map fst (Value.array_bindings array) in
  let rec suffix_from predicate = function
    | [] -> None
    | key :: keys as all ->
        if predicate key then Some all else suffix_from predicate keys
  in
  let pending_after_body array key cell seen pending =
    let current_keys = keys array in
    match
      suffix_from
        (fun key -> List.exists (Value.same_array_key key) pending)
        current_keys
    with
    | Some pending -> pending
    | None -> (
        match suffix_from (Value.same_array_key key) current_keys with
        | Some (_ :: keys as current_and_later) -> (
            match Value.array_find key array with
            | Some (Value.Reference current_cell) when current_cell = cell ->
                keys
            | Some (Inline _ | Reference _) -> current_and_later
            | None -> assert false)
        | Some [] -> assert false
        | None ->
            List.filter
              (fun key -> not (List.exists (Value.same_array_key key) seen))
              current_keys)
  in
  let rec continue state array seen pending =
    let pending =
      match pending with
      | _ :: _ -> pending
      | [] ->
          keys array
          |> List.filter (fun key ->
              not (List.exists (Value.same_array_key key) seen))
    in
    match pending with
    | [] -> Phpsymex.Result.ok (Normal, state)
    | key :: pending -> (
        match Value.array_find key array with
        | None -> continue state array (key :: seen) pending
        | Some entry ->
            let cell, array, state =
              match entry with
              | Value.Reference cell -> (cell, array, state)
              | Inline value ->
                  let cell, state = State.allocate_cell value state in
                  (cell, Value.array_set_reference key cell array, state)
            in
            let open Phpsymex.Syntax in
            finish_evaluation
              (match source with
              | None -> evaluated () state
              | Some place -> write_place place (Value.array array) state)
              (fun () state ->
                finish_evaluation
                  (match key_target with
                  | None -> evaluated () state
                  | Some target ->
                      assign_foreach_target functions state target
                        (foreach_key_value key))
                  (fun () state ->
                    finish_evaluation
                      (bind_foreach_target_reference functions state
                         value_target cell) (fun () state ->
                        let** control, state =
                          exec_statements functions state body
                        in
                        match control with
                        | Normal | Continue 1 -> (
                            match current_array state array with
                            | Some array ->
                                let seen = key :: seen in
                                let pending =
                                  pending_after_body array key cell seen pending
                                in
                                continue state array seen pending
                            | None -> Phpsymex.Result.ok (Normal, state))
                        | Break 1 -> Phpsymex.Result.ok (Normal, state)
                        | Break depth ->
                            Phpsymex.Result.ok (Break (depth - 1), state)
                        | Continue depth ->
                            Phpsymex.Result.ok (Continue (depth - 1), state)
                        | Return _ | Throw _ ->
                            Phpsymex.Result.ok (control, state)))))
  in
  continue state array seen pending

and exec_foreach_reference functions state iterable key_target value_target body
    =
  finish_evaluation (eval_foreach_reference_iterable functions state iterable)
    (fun (value, source) state ->
      match value with
      | Value.Array array ->
          exec_foreach_reference_entries functions state source array key_target
            value_target body []
            (List.map fst (Value.array_bindings array))
      | Value.Object _ -> unsupported "foreach over objects by reference"
      | value ->
          let message =
            Printf.sprintf
              "foreach() argument must be of type array|object, %s given"
              (Value.type_name value)
          in
          finish_evaluation
            (record_runtime_event Error.Runtime_event.Warning message state)
            (fun () state -> Phpsymex.Result.ok (Normal, state)))

and exec_foreach functions state iterable key_target value_target by_reference
    body =
  if by_reference then
    exec_foreach_reference functions state iterable key_target value_target body
  else
    finish_evaluation (eval_expression functions state iterable)
      (fun value state ->
        match value with
        | Value.Array array ->
            exec_foreach_entries functions state key_target value_target body
              (Value.array_bindings array)
        | Value.Object _ -> unsupported "foreach over objects"
        | value ->
            let message =
              Printf.sprintf
                "foreach() argument must be of type array|object, %s given"
                (Value.type_name value)
            in
            finish_evaluation
              (record_runtime_event Error.Runtime_event.Warning message state)
              (fun () state -> Phpsymex.Result.ok (Normal, state)))

and exec_try functions state body catches finally =
  let open Phpsymex.Syntax in
  let** control, state = exec_statements functions state body in
  let** control, state =
    match control with
    | Throw thrown -> exec_catches functions state thrown catches
    | Normal | Return _ | Break _ | Continue _ ->
        Phpsymex.Result.ok (control, state)
  in
  match finally with
  | None -> Phpsymex.Result.ok (control, state)
  | Some finally -> (
      let** finally_control, state = exec_statements functions state finally in
      match finally_control with
      | Normal -> Phpsymex.Result.ok (control, state)
      | Return _ | Break _ | Continue _ | Throw _ ->
          Phpsymex.Result.ok (finally_control, state))

and exec_catches functions state thrown catches =
  match catches with
  | [] -> Phpsymex.Result.ok (Throw thrown, state)
  | (catch : Php_ir.catch_clause) :: catches ->
      let object_ =
        match thrown.value with
        | Value.Object id -> (
            match State.find_object id state with
            | Some object_ -> object_
            | None -> failwith "thrown PHP object is missing from state")
        | _ -> failwith "non-object escaped as a PHP exception"
      in
      if List.exists (class_is_a functions object_.class_name) catch.types then
        let state =
          match catch.variable with
          | None -> state
          | Some name -> State.set_variable name thrown.value state
        in
        exec_statements functions state catch.body
      else exec_catches functions state thrown catches

and exec_statement functions state statement =
  let location =
    match statement with
    | Php_ir.Expression (_, location)
    | Echo (_, location)
    | If (_, _, _, location)
    | While (_, _, location)
    | Foreach (_, _, _, _, _, location)
    | Break (_, location)
    | Continue (_, location)
    | Return (_, location)
    | Try (_, _, _, location)
    | Unset (_, location)
    | Nop location ->
        location
  in
  let process =
    let open Phpsymex.Syntax in
    let* () = Phpsymex.consume_fuel_steps 1 in
    match statement with
    | Php_ir.Expression (expression, _) ->
        finish_evaluation (eval_expression functions state expression)
          (fun _ state -> Phpsymex.Result.ok (Normal, state))
    | Echo (expressions, _) ->
        finish_evaluation (emit_expressions functions state expressions)
          (fun () state -> Phpsymex.Result.ok (Normal, state))
    | If (condition_expression, then_, else_, _) ->
        finish_evaluation (eval_expression functions state condition_expression)
          (fun value state ->
            let** guard = condition value in
            if%sat[@lname "If branch"] [@rname "Else branch"] guard then
              exec_statements functions state then_
            else exec_statements functions state else_)
    | While (condition_expression, body, _) ->
        exec_while functions state condition_expression body
    | Foreach (iterable, key, value, by_reference, body, _) ->
        exec_foreach functions state iterable key value by_reference body
    | Break (depth, _) -> Phpsymex.Result.ok (Break depth, state)
    | Continue (depth, _) -> Phpsymex.Result.ok (Continue depth, state)
    | Return (expression, _) ->
        finish_evaluation
          (match expression with
          | None -> evaluated Value.null state
          | Some expression -> eval_expression functions state expression)
          (fun value state -> Phpsymex.Result.ok (Return value, state))
    | Try (body, catches, finally, _) ->
        exec_try functions state body catches finally
    | Unset (lvalues, _) ->
        finish_evaluation (unset_lvalues functions state lvalues)
          (fun () state -> Phpsymex.Result.ok (Normal, state))
    | Nop _ -> Phpsymex.Result.ok (Normal, state)
  in
  Phpsymex.with_location ~location process

let collect_functions declarations =
  let rec collect functions = function
    | [] -> Phpsymex.Result.ok functions
    | (function_ : Php_ir.function_decl) :: declarations ->
        let name = Builtins.canonical_name function_.Php_ir.name in
        if Function_map.mem name functions then
          Phpsymex.with_location ~location:function_.location
            (unsupported "duplicate function %s" function_.name)
        else collect (Function_map.add name function_ functions) declarations
  in
  collect Function_map.empty declarations

type class_error = Php_ir.location * string
type imported_method = { source_trait : string; member : method_member }

let class_error location format =
  Format.kasprintf (fun message -> Error (location, message)) format

let ( let* ) result continuation =
  match result with Ok value -> continuation value | Error _ as error -> error

let method_name (method_ : method_member) =
  Builtins.canonical_name method_.declaration.Php_ir.name

let visibility_rank = function
  | modifiers -> (
      match member_visibility modifiers with
      | Php_ir.Private -> 0
      | Protected -> 1
      | Public -> 2
      | Static -> assert false)

let replace_method ?name ?visibility declaring_class (method_ : imported_method)
    : method_member =
  let declaration =
    {
      method_.member.declaration with
      name = Option.value name ~default:method_.member.declaration.name;
      modifiers =
        Option.fold ~none:method_.member.declaration.modifiers
          ~some:(fun visibility ->
            if member_is_static method_.member.declaration.modifiers then
              [ visibility; Php_ir.Static ]
            else [ visibility ])
          visibility;
    }
  in
  ({ declaring_class; declaration } : method_member)

let has_duplicate_names names =
  let rec duplicate seen = function
    | [] -> false
    | name :: names ->
        let name = Builtins.canonical_name name in
        List.mem name seen || duplicate (name :: seen) names
  in
  duplicate [] names

let compose_trait_methods resolve declaration =
  let trait_names =
    List.concat_map
      (fun (use : Php_ir.trait_use) -> use.traits)
      declaration.Php_ir.traits
  in
  if has_duplicate_names trait_names then
    class_error declaration.location "duplicate trait use"
  else
    let own_names =
      List.map
        (fun (method_ : Php_ir.method_decl) ->
          Builtins.canonical_name method_.name)
        declaration.Php_ir.methods
    in
    let rec collect_uses accumulated_methods accumulated_properties = function
      | [] -> Ok (accumulated_methods, accumulated_properties)
      | (use : Php_ir.trait_use) :: uses ->
          let rec collect_traits imported properties = function
            | [] -> Ok (imported, properties)
            | trait_name :: traits -> (
                match resolve trait_name with
                | Error _ as error -> error
                | Ok trait_ when trait_.kind <> Php_ir.Trait ->
                    class_error use.location "%s is not a trait" trait_name
                | Ok trait_ ->
                    let methods =
                      List.map
                        (fun member -> { source_trait = trait_.name; member })
                        trait_.methods
                    in
                    collect_traits (imported @ methods)
                      (properties @ trait_.properties)
                      traits)
          in
          let open Stdlib.Result in
          let* imported, imported_properties =
            collect_traits [] [] use.traits
          in
          let original = imported in
          let rec precedences imported = function
            | [] -> Ok imported
            | Php_ir.Trait_alias _ :: adaptations ->
                precedences imported adaptations
            | Trait_precedence
                { trait; method_name = name; instead_of; location }
              :: adaptations ->
                let trait = Builtins.canonical_name trait in
                let name = Builtins.canonical_name name in
                let instead_of = List.map Builtins.canonical_name instead_of in
                if List.mem trait instead_of || has_duplicate_names instead_of
                then class_error location "invalid insteadof trait list"
                else if
                  not
                    (List.exists
                       (fun imported ->
                         String.equal
                           (Builtins.canonical_name imported.source_trait)
                           trait
                         && String.equal (method_name imported.member) name)
                       original)
                then
                  class_error location
                    "trait method selected by insteadof was not found"
                else if
                  List.exists
                    (fun excluded ->
                      not
                        (List.exists
                           (fun imported ->
                             String.equal
                               (Builtins.canonical_name imported.source_trait)
                               excluded
                             && String.equal (method_name imported.member) name)
                           original))
                    instead_of
                then
                  class_error location
                    "trait method excluded by insteadof was not found"
                else
                  let imported =
                    List.filter
                      (fun imported ->
                        not
                          (String.equal (method_name imported.member) name
                          && List.mem
                               (Builtins.canonical_name imported.source_trait)
                               instead_of))
                      imported
                  in
                  precedences imported adaptations
          in
          let* selected = precedences imported use.adaptations in
          let rec aliases selected additions = function
            | [] -> Ok (selected, additions)
            | Php_ir.Trait_precedence _ :: adaptations ->
                aliases selected additions adaptations
            | Trait_alias
                { trait; method_name = name; alias; visibility; location }
              :: adaptations -> (
                let name = Builtins.canonical_name name in
                let candidates =
                  List.filter
                    (fun imported ->
                      String.equal (method_name imported.member) name
                      && Option.fold ~none:true
                           ~some:(fun trait ->
                             String.equal
                               (Builtins.canonical_name imported.source_trait)
                               (Builtins.canonical_name trait))
                           trait)
                    original
                in
                match candidates with
                | [ candidate ] ->
                    let selected, additions =
                      match alias with
                      | Some alias ->
                          ( selected,
                            replace_method ~name:alias ?visibility
                              declaration.name candidate
                            :: additions )
                      | None ->
                          let selected =
                            List.map
                              (fun imported ->
                                if
                                  String.equal
                                    (Builtins.canonical_name
                                       imported.source_trait)
                                    (Builtins.canonical_name
                                       candidate.source_trait)
                                  && String.equal
                                       (method_name imported.member)
                                       name
                                then
                                  {
                                    imported with
                                    member =
                                      replace_method ?visibility
                                        declaration.name imported;
                                  }
                                else imported)
                              selected
                          in
                          (selected, additions)
                    in
                    aliases selected additions adaptations
                | [] ->
                    class_error location
                      "trait method selected by alias was not found"
                | _ ->
                    class_error location
                      "trait alias is ambiguous without an explicit trait name")
          in
          let* selected, aliases = aliases selected [] use.adaptations in
          let selected =
            List.filter
              (fun imported ->
                not (List.mem (method_name imported.member) own_names))
              selected
          in
          let rec reject_conflicts seen = function
            | [] -> Ok ()
            | imported :: methods ->
                let name = method_name imported.member in
                if List.mem name seen then
                  class_error use.location "trait method conflict for %s" name
                else reject_conflicts (name :: seen) methods
          in
          let* () = reject_conflicts [] selected in
          let methods =
            List.map (replace_method declaration.name) selected @ aliases
          in
          collect_uses
            (accumulated_methods @ methods)
            (accumulated_properties @ imported_properties)
            uses
    in
    let open Stdlib.Result in
    let* methods, properties = collect_uses [] [] declaration.traits in
    let methods =
      List.filter
        (fun method_ -> not (List.mem (method_name method_) own_names))
        methods
    in
    let rec reject_method_conflicts seen = function
      | [] -> Ok ()
      | (method_ : method_member) :: methods ->
          let name = method_name method_ in
          if List.mem name seen then
            class_error declaration.location "trait method conflict for %s" name
          else reject_method_conflicts (name :: seen) methods
    in
    let* () = reject_method_conflicts [] methods in
    let own_methods =
      List.map
        (fun (method_ : Php_ir.method_decl) ->
          ({ declaring_class = declaration.name; declaration = method_ }
            : method_member))
        declaration.methods
    in
    let own_properties =
      List.map
        (fun (property : Php_ir.property_decl) ->
          ({ declaring_class = declaration.name; declaration = property }
            : property_member))
        declaration.properties
    in
    let properties =
      List.map
        (fun (property : property_member) ->
          { property with declaring_class = declaration.name })
        properties
    in
    let rec reject_property_conflicts seen = function
      | [] -> Ok ()
      | (property : property_member) :: properties ->
          if List.mem property.declaration.Php_ir.name seen then
            class_error declaration.location "trait property conflict for %s"
              property.declaration.name
          else
            reject_property_conflicts
              (property.declaration.name :: seen)
              properties
    in
    let properties = own_properties @ properties in
    let* () = reject_property_conflicts [] properties in
    Ok (own_methods @ methods, properties)

let build_classes declarations =
  let rec collect_raw classes = function
    | [] -> Ok classes
    | (declaration : Php_ir.class_decl) :: rest ->
        let name = Builtins.canonical_name declaration.name in
        if
          Class_map.mem name classes
          || Option.is_some (find_throwable_class name)
        then
          class_error declaration.location "duplicate declaration %s"
            declaration.name
        else collect_raw (Class_map.add name declaration classes) rest
  in
  let open Stdlib.Result in
  let* raw = collect_raw Class_map.empty declarations in
  let rec resolve stack name =
    let canonical_name = Builtins.canonical_name name in
    if List.mem canonical_name stack then
      match Class_map.find_opt canonical_name raw with
      | Some declaration ->
          class_error declaration.location "cyclic declaration involving %s"
            declaration.name
      | None -> assert false
    else
      match Class_map.find_opt canonical_name raw with
      | None ->
          class_error
            (match declarations with
            | declaration :: _ -> declaration.Php_ir.location
            | [] -> failwith "missing declaration without a source location")
            "unknown declaration %s" name
      | Some declaration ->
          let resolve = resolve (canonical_name :: stack) in
          let* () =
            match declaration.parent with
            | None -> Ok ()
            | Some parent -> (
                match find_throwable_class parent with
                | Some { constructible = true; _ }
                  when declaration.kind = Php_ir.Class ->
                    Ok ()
                | Some _ ->
                    class_error declaration.location "%s cannot be extended"
                      parent
                | None ->
                    let* parent = resolve parent in
                    if parent.kind = Php_ir.Class && declaration.kind = Class
                    then Ok ()
                    else
                      class_error declaration.location "%s is not a class"
                        parent.name)
          in
          let expected_interface = declaration.kind <> Php_ir.Trait in
          let rec validate_interfaces = function
            | [] -> Ok ()
            | interface :: interfaces ->
                let* interface_ = resolve interface in
                if expected_interface && interface_.kind = Php_ir.Interface then
                  validate_interfaces interfaces
                else
                  class_error declaration.location "%s is not an interface"
                    interface
          in
          let* () =
            if has_duplicate_names declaration.interfaces then
              class_error declaration.location "duplicate interface reference"
            else validate_interfaces declaration.interfaces
          in
          let* methods, properties =
            compose_trait_methods resolve declaration
          in
          Ok
            {
              kind = declaration.kind;
              name = declaration.name;
              parent = declaration.parent;
              interfaces = declaration.interfaces;
              properties;
              methods;
              location = declaration.location;
            }
  in
  let* classes =
    Class_map.fold
      (fun name _ result ->
        let* classes = result in
        let* class_ = resolve [] name in
        Ok (Class_map.add name class_ classes))
      raw (Ok Class_map.empty)
  in
  let rec interface_methods seen (class_ : class_info) =
    let name = Builtins.canonical_name class_.name in
    if List.mem name seen then []
    else
      class_.methods
      @ List.concat_map
          (fun parent ->
            match
              Class_map.find_opt (Builtins.canonical_name parent) classes
            with
            | Some parent -> interface_methods (name :: seen) parent
            | None -> [])
          class_.interfaces
  in
  let validate_class (class_ : class_info) =
    match class_.kind with
    | Php_ir.Interface | Trait -> Ok ()
    | Class -> (
        let magic_arity name =
          List.assoc_opt
            (Builtins.canonical_name name)
            [
              ("__get", 1);
              ("__set", 2);
              ("__isset", 1);
              ("__unset", 1);
              ("__call", 2);
              ("__tostring", 0);
              ("__clone", 0);
            ]
        in
        let rec validate_magic_methods = function
          | [] -> Ok ()
          | (method_ : method_member) :: methods -> (
              let name = Builtins.canonical_name method_.declaration.name in
              match magic_arity method_.declaration.name with
              | None -> validate_magic_methods methods
              | Some arity
                when (String.equal name "__clone"
                     || member_visibility method_.declaration.modifiers = Public
                     )
                     && (not (member_is_static method_.declaration.modifiers))
                     && List.length method_.declaration.parameters = arity ->
                  validate_magic_methods methods
              | Some arity when String.equal name "__clone" ->
                  class_error method_.declaration.location
                    "%s::%s must be non-static and accept exactly %d arguments"
                    method_.declaring_class method_.declaration.name arity
              | Some arity ->
                  class_error method_.declaration.location
                    "%s::%s must be public, non-static, and accept exactly %d \
                     argument%s"
                    method_.declaring_class method_.declaration.name arity
                    (if arity = 1 then "" else "s"))
        in
        let* () = validate_magic_methods class_.methods in
        let requirements =
          List.concat_map
            (fun interface ->
              match
                Class_map.find_opt (Builtins.canonical_name interface) classes
              with
              | Some interface -> interface_methods [] interface
              | None -> [])
            class_.interfaces
        in
        let rec validate_requirements = function
          | [] -> Ok ()
          | (requirement : method_member) :: requirements -> (
              match
                find_method_from
                  { functions = Function_map.empty; classes }
                  class_ requirement.declaration.name
              with
              | Some implementation
                when implementation.declaration.body <> None
                     && member_visibility implementation.declaration.modifiers
                        = Php_ir.Public
                     && Bool.equal
                          (member_is_static implementation.declaration.modifiers)
                          (member_is_static requirement.declaration.modifiers)
                     && List.length implementation.declaration.parameters
                        = List.length requirement.declaration.parameters ->
                  validate_requirements requirements
              | Some _ | None ->
                  class_error class_.location
                    "%s does not implement interface method %s::%s" class_.name
                    requirement.declaring_class requirement.declaration.name)
        in
        let* () = validate_requirements requirements in
        match class_.parent with
        | None -> Ok ()
        | Some parent -> (
            match
              Class_map.find_opt (Builtins.canonical_name parent) classes
            with
            | None -> Ok ()
            | Some parent ->
                let rec validate_overrides = function
                  | [] -> Ok ()
                  | (method_ : method_member) :: methods -> (
                      match
                        find_method_from
                          { functions = Function_map.empty; classes }
                          parent method_.declaration.name
                      with
                      | Some inherited
                        when member_visibility inherited.declaration.modifiers
                             <> Php_ir.Private
                             && not
                                  (Bool.equal
                                     (member_is_static
                                        method_.declaration.modifiers)
                                     (member_is_static
                                        inherited.declaration.modifiers)) ->
                          class_error method_.declaration.location
                            "incompatible static override of %s::%s"
                            inherited.declaring_class inherited.declaration.name
                      | Some inherited
                        when member_visibility inherited.declaration.modifiers
                             <> Php_ir.Private
                             && Bool.equal
                                  (member_is_static
                                     method_.declaration.modifiers)
                                  (member_is_static
                                     inherited.declaration.modifiers)
                             && (not
                                   (String.equal
                                      (Builtins.canonical_name
                                         method_.declaration.name)
                                      "__construct"))
                             && (visibility_rank method_.declaration.modifiers
                                 < visibility_rank
                                     inherited.declaration.modifiers
                                || List.length method_.declaration.parameters
                                   <> List.length
                                        inherited.declaration.parameters) ->
                          class_error method_.declaration.location
                            "incompatible override of %s::%s"
                            inherited.declaring_class inherited.declaration.name
                      | Some _ | None -> validate_overrides methods)
                in
                let* () = validate_overrides class_.methods in
                let rec validate_property_overrides = function
                  | [] -> Ok ()
                  | (property : property_member) :: properties -> (
                      match
                        find_any_property_from
                          { functions = Function_map.empty; classes }
                          parent property.declaration.name
                      with
                      | Some inherited
                        when member_visibility inherited.declaration.modifiers
                             <> Php_ir.Private
                             && not
                                  (Bool.equal
                                     (member_is_static
                                        property.declaration.modifiers)
                                     (member_is_static
                                        inherited.declaration.modifiers)) ->
                          class_error property.declaration.location
                            "incompatible static property override of %s::$%s"
                            inherited.declaring_class inherited.declaration.name
                      | Some inherited
                        when member_visibility inherited.declaration.modifiers
                             <> Php_ir.Private
                             && Bool.equal
                                  (member_is_static
                                     property.declaration.modifiers)
                                  (member_is_static
                                     inherited.declaration.modifiers)
                             && visibility_rank property.declaration.modifiers
                                < visibility_rank
                                    inherited.declaration.modifiers ->
                          class_error property.declaration.location
                            "incompatible property override of %s::$%s"
                            inherited.declaring_class inherited.declaration.name
                      | Some _ | None -> validate_property_overrides properties)
                in
                validate_property_overrides class_.properties))
  in
  let* () =
    Class_map.fold
      (fun _ class_ result ->
        let* () = result in
        validate_class class_)
      classes (Ok ())
  in
  Ok classes

let collect_classes declarations =
  match build_classes declarations with
  | Ok classes -> Phpsymex.Result.ok classes
  | Error (location, message) ->
      Phpsymex.with_location ~location (unsupported "%s" message)

let find_entry_point program name =
  let canonical_name = Builtins.canonical_name name in
  List.find_opt
    (fun (function_ : Php_ir.function_decl) ->
      String.equal (Builtins.canonical_name function_.name) canonical_name)
    program.Php_ir.functions

let validate_entry_point program name =
  match find_entry_point program name with
  | None -> Error (Printf.sprintf "function %s was not found" name)
  | Some function_ when function_.Php_ir.parameters <> [] ->
      Error
        (Printf.sprintf
           "function %s has %d parameter(s); function entry points must have \
            no parameters"
           function_.name
           (List.length function_.parameters))
  | Some function_ -> Ok function_

let finish state = function
  | Evaluated _ -> Phpsymex.Result.ok state
  | Raised thrown -> (
      match thrown.value with
      | Value.Object id -> (
          match State.find_object id state with
          | Some object_ ->
              Phpsymex.error_at thrown.trace
                (Error.Uncaught_exception
                   {
                     class_name = object_.class_name;
                     message = object_.message;
                   })
          | None -> failwith "uncaught PHP object is missing from state")
      | _ -> failwith "non-object escaped as an uncaught PHP exception")

let run ?function_name program =
  let open Phpsymex.Syntax in
  let** functions = collect_functions program.Php_ir.functions in
  let** classes = collect_classes program.Php_ir.classes in
  let declarations = { functions; classes } in
  let** initialization, initial_state =
    initialize_static_properties declarations State.empty
      (Class_map.bindings classes)
  in
  match initialization with
  | Raised thrown -> finish initial_state (Raised thrown)
  | Evaluated () -> (
      match function_name with
      | Some name -> (
          match find_entry_point program name with
          | None -> unsupported "entry point function %s" name
          | Some function_ ->
              let** result, state =
                call_function declarations initial_state function_.location
                  function_.name []
              in
              finish state result)
      | None -> (
          let** control, state =
            exec_statements declarations initial_state program.statements
          in
          match control with
          | Normal -> Phpsymex.Result.ok state
          | Throw thrown -> finish state (Raised thrown)
          | Return _ | Break _ | Continue _ ->
              failwith "invalid structured control escaped the PHP program"))
