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
  | (Value.Undef | Value.Null | Value.String _ | Value.Array _ | Value.Object _)
    as value ->
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

type declarations = {
  functions : Php_ir.function_decl Function_map.t;
  classes : Php_ir.class_decl Class_map.t;
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
  | Object_property of State.object_id * string
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

let rec class_is_a actual expected =
  let expected = Builtins.canonical_name expected in
  if String.equal (String.lowercase_ascii actual) expected then true
  else
    match find_throwable_class actual with
    | Some { parent = Some parent; _ } -> class_is_a parent expected
    | Some { parent = None; _ } | None -> false

let evaluated value state = Phpsymex.Result.ok (Evaluated value, state)

let bind_evaluation evaluation continuation =
  let open Phpsymex.Syntax in
  let** result, state = evaluation in
  match result with
  | Evaluated value -> continuation (value, state)
  | Raised thrown -> Phpsymex.Result.ok (Raised thrown, state)

let ( let*** ) = bind_evaluation

let construct_throwable state name arguments =
  match find_throwable_class name with
  | Some { constructible = true; name; _ } ->
      let open Phpsymex.Syntax in
      let** message =
        match arguments with
        | [] -> Phpsymex.Result.ok ""
        | [ value ] ->
            let** value = coerce Coercion.String value in
            Phpsymex.Result.ok (string_value value)
        | _ -> unsupported "%s::__construct() arguments beyond the message" name
      in
      let id, state = State.allocate_object name message state in
      evaluated (Value.object_ id) state
  | Some { constructible = false; _ } -> unsupported "construction of Throwable"
  | None -> failwith "non-throwable class passed to throwable construction"

let raise_value value state =
  let value, state =
    match value with
    | Value.Object id -> (
        match State.find_object id state with
        | Some object_ when class_is_a object_.class_name "Throwable" ->
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
  | Invalid_read -> Value.null

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
  | (property : Php_ir.property_decl) :: properties ->
      let open Phpsymex.Syntax in
      let*** value, state =
        match property.default with
        | None -> evaluated Value.null state
        | Some expression -> eval_expression declarations state expression
      in
      let*** values, state =
        eval_property_defaults declarations state properties
      in
      evaluated ((property.name, value) :: values) state

and construct_object declarations state name arguments =
  let canonical_name = Builtins.canonical_name name in
  match Class_map.find_opt canonical_name declarations.classes with
  | None -> unsupported "object construction for class %s" name
  | Some (class_ : Php_ir.class_decl) ->
      let open Phpsymex.Syntax in
      let*** properties, state =
        eval_property_defaults declarations state class_.properties
      in
      let _ = arguments in
      let id, state = State.allocate_object ~properties class_.name "" state in
      evaluated (Value.object_ id) state

and resolve_lvalue functions state ~access (lvalue : Php_ir.lvalue) =
  let process =
    let open Phpsymex.Syntax in
    match lvalue.desc with
    | Variable_lvalue name -> (
        match access with
        | Write | Unset -> evaluated (Variable name) state
        | Read -> (
            match State.find_variable name state with
            | Some value when Value.kind value <> `Undefined ->
                evaluated (Variable name) state
            | Some _ | None ->
                let*** (), state =
                  record_runtime_event Error.Runtime_event.Warning
                    (Printf.sprintf "Undefined variable $%s" name)
                    state
                in
                evaluated Invalid_read state))
    | Array_element_lvalue (parent, key_expression) ->
        let*** parent, state = resolve_lvalue functions state ~access parent in
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
                      write_place parent (Value.array array) state
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
    | Object_property_lvalue (object_, name) -> (
        let*** object_, state =
          resolve_lvalue functions state ~access object_
        in
        match read_place state object_ with
        | Value.Object object_id -> (
            let object_state =
              match State.find_object object_id state with
              | Some object_ -> object_
              | None -> failwith "PHP object value refers to an unknown object"
            in
            let place = Object_property (object_id, name) in
            match
              ( access,
                State.object_declares_property object_id name state,
                State.find_object_property_cell object_id name state )
            with
            | Read, _, None ->
                let*** (), state =
                  record_runtime_event Error.Runtime_event.Warning
                    (Printf.sprintf "Undefined property: %s::$%s"
                       object_state.class_name name)
                    state
                in
                evaluated Invalid_read state
            | Write, false, None ->
                let*** (), state =
                  record_runtime_event Error.Runtime_event.Deprecation
                    (Printf.sprintf
                       "Creation of dynamic property %s::$%s is deprecated"
                       object_state.class_name name)
                    state
                in
                evaluated place state
            | (Read | Write | Unset), _, _ -> evaluated place state)
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
    | Assign (target, expression) ->
        let*** place, state =
          resolve_lvalue functions state ~access:Write target
        in
        let*** value, state = eval_expression functions state expression in
        let*** (), state = write_place place value state in
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
    | Binary (left, operator, right) ->
        let*** left, state = eval_expression functions state left in
        let*** right, state = eval_expression functions state right in
        let** operation = binary state operator left right in
        apply_operation operation state
    | Cast (cast, expression) ->
        let*** value, state = eval_expression functions state expression in
        let target =
          match cast with
          | Php_ir.To_boolean -> Coercion.Boolean
          | To_integer -> Coercion.Integer
          | To_float -> Coercion.Float
          | To_string -> Coercion.String
        in
        let** value = coerce target value in
        evaluated value state
    | Call (name, arguments) -> (
        let*** arguments, state = eval_expressions functions state arguments in
        match Builtins.find name with
        | Some implementation -> (
            match Builtins.runtime_error name arguments with
            | Some error -> raise_runtime_error state error
            | None ->
                let** value = implementation ~args:arguments in
                evaluated value state)
        | None ->
            call_function functions state expression.location name arguments)
    | New (name, arguments) -> (
        let*** arguments, state = eval_expressions functions state arguments in
        match find_throwable_class name with
        | Some _ -> construct_throwable state name arguments
        | None -> construct_object functions state name arguments)
    | Throw expression ->
        let*** value, state = eval_expression functions state expression in
        raise_value value state
  in
  Phpsymex.with_location ~location:expression.location process

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

and emit_expressions functions state expressions =
  let open Phpsymex.Syntax in
  match expressions with
  | [] -> evaluated () state
  | expression :: expressions ->
      let*** value, state = eval_expression functions state expression in
      let* value = simplify_value value in
      let** value = coerce Coercion.String value in
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
      let*** (), state = unset_place place state in
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
  write_place place value state

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
  | Some lvalue ->
      let open Phpsymex.Syntax in
      let*** place, state =
        resolve_lvalue functions state ~access:Read lvalue
      in
      evaluated (read_place state place, Some place) state

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
      if List.exists (class_is_a object_.class_name) catch.types then
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

let collect_classes declarations =
  let rec collect classes = function
    | [] -> Phpsymex.Result.ok classes
    | (class_ : Php_ir.class_decl) :: declarations ->
        let name = Builtins.canonical_name class_.Php_ir.name in
        if
          Class_map.mem name classes
          || Option.is_some (find_throwable_class name)
        then
          Phpsymex.with_location ~location:class_.location
            (unsupported "duplicate class %s" class_.name)
        else collect (Class_map.add name class_ classes) declarations
  in
  collect Class_map.empty declarations

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
  match function_name with
  | Some name -> (
      match find_entry_point program name with
      | None -> unsupported "entry point function %s" name
      | Some function_ ->
          let** result, state =
            call_function declarations State.empty function_.location
              function_.name []
          in
          finish state result)
  | None -> (
      let** control, state =
        exec_statements declarations State.empty program.statements
      in
      match control with
      | Normal -> Phpsymex.Result.ok state
      | Throw thrown -> finish state (Raised thrown)
      | Return _ | Break _ | Continue _ ->
          failwith "invalid structured control escaped the PHP program")
