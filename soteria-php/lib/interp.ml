let unsupported format = Format.kasprintf Phpsymex.not_impl format
let coercion_error error = unsupported "%a" Coercion.pp_error error

let coerce target value =
  match Coercion.coerce target value with
  | Ok value -> Phpsymex.Result.ok value
  | Error error -> coercion_error error

let coerce_number value =
  match Coercion.to_number value with
  | Ok value -> Phpsymex.Result.ok value
  | Error error -> coercion_error error

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
  match value with
  | Value.Bool condition -> Phpsymex.Result.ok condition
  | _ -> failwith "boolean coercion returned a non-boolean value"

let numeric_float = function
  | Value.Int value ->
      Some
        (Value.Typed.BitVec.to_float
           ~rounding:Value.Typed.RoundingMode.NearestTiesToEven ~signed:true
           ~fp:Value.Typed.FloatPrecision.F64 value)
  | Value.Float value -> Some value
  | _ -> None

let concrete_numeric_float = function
  | Value.Int _ as value -> Option.map Int64.to_float (Value.int_value value)
  | Value.Float _ as value -> Value.float_value value
  | _ -> None

let checked_integer check float left right =
  let result, overflow = check ~signed:true left right in
  Phpsymex.branch_on overflow ~left_branch_name:"Integer overflow"
    ~right_branch_name:"Integer result"
    ~then_:(fun () ->
      let left = Option.get (numeric_float (Value.Int left)) in
      let right = Option.get (numeric_float (Value.Int right)) in
      Phpsymex.Result.ok (Value.Float (float left right)))
    ~else_:(fun () -> Phpsymex.Result.ok (Value.Int result))

let arithmetic integer float concrete_float left right =
  let open Phpsymex.Syntax in
  let** left = coerce_number left in
  let** right = coerce_number right in
  match (left, right) with
  | Value.Int left, Value.Int right -> checked_integer integer float left right
  | _ -> (
      match (concrete_numeric_float left, concrete_numeric_float right) with
      | Some left, Some right ->
          Phpsymex.Result.ok (Value.float (concrete_float left right))
      | _ -> (
          match (numeric_float left, numeric_float right) with
          | Some left, Some right ->
              Phpsymex.Result.ok (Value.Float (float left right))
          | _ -> assert false))

let division left right =
  let open Phpsymex.Syntax in
  let** left = coerce_number left in
  let** right = coerce_number right in
  match (concrete_numeric_float left, concrete_numeric_float right) with
  | _, Some denominator when denominator = 0.0 ->
      Phpsymex.error Error.Division_by_zero
  | Some numerator, Some denominator ->
      Phpsymex.Result.ok (Value.float (numerator /. denominator))
  | _ -> (
      match (numeric_float left, numeric_float right) with
      | Some numerator, Some denominator ->
          Phpsymex.branch_on
            (Value.Typed.Float.is_zero denominator)
            ~left_branch_name:"Division by zero" ~right_branch_name:"Division"
            ~then_:(fun () -> Phpsymex.error Error.Division_by_zero)
            ~else_:(fun () ->
              Phpsymex.Result.ok
                (Value.Float (Value.Typed.Float.div numerator denominator)))
      | _ -> assert false)

let rec strict_equal state left right =
  let result =
    match (left, right) with
    | Value.Undef, Value.Undef | Value.Null, Value.Null ->
        Value.Typed.Bool.v_true
    | Value.Bool left, Value.Bool right -> Value.Typed.sem_eq left right
    | Value.Int left, Value.Int right -> Value.Typed.sem_eq left right
    | Value.Float left, Value.Float right -> Value.Typed.Float.eq left right
    | Value.String left, Value.String right ->
        Value.Typed.Bool.of_bool (String.equal left right)
    | Value.Array left, Value.Array right ->
        strict_equal_arrays state left right
    | Value.Object left, Value.Object right ->
        Value.Typed.Bool.of_bool (left = right)
    | _ -> Value.Typed.Bool.v_false
  in
  Value.Bool result

and strict_equal_arrays state left right =
  let left = Value.array_bindings left in
  let right = Value.array_bindings right in
  if List.length left <> List.length right then Value.Typed.Bool.v_false
  else
    List.fold_left2
      (fun equal (left_key, left_entry) (right_key, right_entry) ->
        let keys_equal = Stdlib.compare left_key right_key = 0 in
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
          | _ -> (
              match strict_equal state left_value right_value with
              | Value.Bool equal -> equal
              | _ -> assert false)
        in
        Value.Typed.Bool.and_ equal
          (Value.Typed.Bool.and_
             (Value.Typed.Bool.of_bool keys_equal)
             values_equal))
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
  | Php_ir.Add when Value.kind left = `Array && Value.kind right = `Array ->
      let left = Option.get (Value.array_value left) in
      let right = Option.get (Value.array_value right) in
      Phpsymex.Result.ok (Value.array (Value.array_union left right))
  | Php_ir.Add ->
      arithmetic Value.Typed.BitVec.add_checked Value.Typed.Float.add ( +. )
        left right
  | Subtract ->
      arithmetic Value.Typed.BitVec.sub_checked Value.Typed.Float.sub ( -. )
        left right
  | Multiply ->
      arithmetic Value.Typed.BitVec.mul_checked Value.Typed.Float.mul ( *. )
        left right
  | Divide -> division left right
  | Concat ->
      let open Phpsymex.Syntax in
      let** left = coerce Coercion.String left in
      let** right = coerce Coercion.String right in
      let left = Option.get (Value.string_value left) in
      let right = Option.get (Value.string_value right) in
      Phpsymex.Result.ok (Value.string (left ^ right))
  | Identical -> Phpsymex.Result.ok (strict_equal state left right)
  | Not_identical -> (
      match strict_equal state left right with
      | Value.Bool equal ->
          Phpsymex.Result.ok (Value.Bool (Value.Typed.Bool.not equal))
      | _ -> failwith "strict equality returned a non-boolean value")
  | Equal -> loose_equal left right
  | Not_equal ->
      let open Phpsymex.Syntax in
      let** equal = loose_equal left right in
      let equal =
        match equal with Value.Bool value -> value | _ -> assert false
      in
      Phpsymex.Result.ok (Value.Bool (Value.Typed.Bool.not equal))
  | Less_than | Less_than_or_equal | Greater_than | Greater_than_or_equal ->
      comparison operator left right
  | Boolean_and | Boolean_or ->
      failwith "short-circuit operator passed to eager evaluation"

let unary operator value =
  match operator with
  | Php_ir.Boolean_not ->
      let open Phpsymex.Syntax in
      let** condition = condition value in
      Phpsymex.Result.ok (Value.Bool (Value.Typed.Bool.not condition))
  | Numeric_identity -> coerce_number value
  | Numeric_negation -> (
      let open Phpsymex.Syntax in
      let** value = coerce_number value in
      match value with
      | Value.Int value ->
          let result, overflow = Value.Typed.BitVec.neg_checked value in
          Phpsymex.branch_on overflow ~left_branch_name:"Integer overflow"
            ~right_branch_name:"Integer result"
            ~then_:(fun () ->
              let value = Option.get (numeric_float (Value.Int value)) in
              Phpsymex.Result.ok (Value.Float (Value.Typed.Float.neg value)))
            ~else_:(fun () -> Phpsymex.Result.ok (Value.Int result))
      | Value.Float value ->
          Phpsymex.Result.ok (Value.Float (Value.Typed.Float.neg value))
      | _ -> assert false)

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
            Phpsymex.Result.ok (Option.get (Value.string_value value))
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

let rec write_place place value state =
  match place with
  | Variable name -> Phpsymex.Result.ok (State.set_variable name value state)
  | Array_element (parent, key) -> (
      let array =
        match read_place state parent with
        | Value.Array array -> Some array
        | Value.Undef | Value.Null -> Some Value.empty_array
        | _ -> None
      in
      match array with
      | Some array -> (
          match Value.array_find key array with
          | Some (Value.Reference cell) ->
              Phpsymex.Result.ok (State.set_cell cell value state)
          | Some (Value.Inline _) | None ->
              write_place parent
                (Value.array (Value.array_set key value array))
                state)
      | None ->
          Phpsymex.error
            (Error.Cannot_use_as_array (Value.kind (read_place state parent))))
  | Object_property (object_id, name) ->
      Phpsymex.Result.ok (State.set_object_property object_id name value state)

let bind_place_reference place cell state =
  match place with
  | Variable name -> Phpsymex.Result.ok (State.bind_variable name cell state)
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
      | value -> Phpsymex.error (Error.Cannot_use_as_array (Value.kind value)))
  | Object_property (object_id, name) ->
      Phpsymex.Result.ok (State.bind_object_property object_id name cell state)

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
      Phpsymex.Result.ok (cell, state)
  | Array_element (parent, key) -> (
      let array =
        match read_place state parent with
        | Value.Array array -> Some array
        | Value.Undef | Value.Null -> Some Value.empty_array
        | _ -> None
      in
      match array with
      | None ->
          Phpsymex.error
            (Error.Cannot_use_as_array (Value.kind (read_place state parent)))
      | Some array -> (
          match Value.array_find key array with
          | Some (Value.Reference cell) -> Phpsymex.Result.ok (cell, state)
          | entry ->
              let value =
                match entry with
                | Some (Value.Inline value) -> value
                | None -> Value.null
                | Some (Value.Reference _) -> assert false
              in
              let cell, state = State.allocate_cell value state in
              let open Phpsymex.Syntax in
              let** state =
                write_place parent
                  (Value.array (Value.array_set_reference key cell array))
                  state
              in
              Phpsymex.Result.ok (cell, state)))
  | Object_property (object_id, name) -> (
      match State.find_object_property_cell object_id name state with
      | Some cell -> Phpsymex.Result.ok (cell, state)
      | None ->
          let cell, state = State.allocate_cell Value.null state in
          let state = State.bind_object_property object_id name cell state in
          Phpsymex.Result.ok (cell, state))

let unset_place place state =
  match place with
  | Variable name -> Phpsymex.Result.ok (State.unset_variable name state)
  | Array_element (parent, key) -> (
      match read_place state parent with
      | Value.Array array ->
          write_place parent (Value.array (Value.array_remove key array)) state
      | Value.Undef | Value.Null -> Phpsymex.Result.ok state
      | value -> Phpsymex.error (Error.Cannot_use_as_array (Value.kind value)))
  | Object_property (object_id, name) ->
      Phpsymex.Result.ok (State.unset_object_property object_id name state)

let normalized_array_key value =
  match Coercion.to_array_key value with
  | Ok key -> Phpsymex.Result.ok key
  | Error (Invalid_array_key kind) ->
      Phpsymex.error (Error.Illegal_offset_type kind)
  | Error error -> coercion_error error

let resolve_array_key ~for_write array = function
  | Coercion.Concrete_key key -> Phpsymex.Result.ok key
  | Symbolic_integer_key symbolic_key ->
      let rec choose = function
        | [] ->
            unsupported
              (if for_write then "fresh symbolic array key assignment"
               else "read of an undefined symbolic array offset")
        | key :: keys ->
            let concrete_key =
              Value.Typed.BitVec.mk_masked Value.integer_bits (Z.of_int64 key)
            in
            Phpsymex.branch_on
              (Value.Typed.sem_eq symbolic_key concrete_key)
              ~left_branch_name:"Existing array key"
              ~right_branch_name:"Different array key"
              ~then_:(fun () -> Phpsymex.Result.ok (Value.Integer_key key))
              ~else_:(fun () -> choose keys)
      in
      choose (Value.array_integer_keys array)

let array_for_access ~for_write = function
  | Value.Array array -> Phpsymex.Result.ok array
  | (Value.Undef | Value.Null) when for_write ->
      Phpsymex.Result.ok Value.empty_array
  | value when for_write ->
      Phpsymex.error (Error.Cannot_use_as_array (Value.kind value))
  | value -> unsupported "array access on %s" (Value.type_name value)

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
              match Value.array_next_key array with
              | Some key -> evaluated key state
              | None -> Phpsymex.error Error.Array_append_overflow)
          | Some expression ->
              let*** value, state =
                eval_expression functions state expression
              in
              let** key = normalized_array_key value in
              let** key = resolve_array_key ~for_write:true array key in
              evaluated key state
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
            match State.find_variable_cell name state with
            | Some _ -> evaluated (Variable name) state
            | None -> unsupported "read of undefined variable $%s" name))
    | Array_element_lvalue (parent, key_expression) ->
        let*** parent, state = resolve_lvalue functions state ~access parent in
        let*** key, state =
          match key_expression with
          | None -> (
              if access <> Write then unsupported "array append read"
              else
                let** array =
                  read_place state parent |> array_for_access ~for_write:true
                in
                match Value.array_reserve_next array with
                | Some (key, array) ->
                    let** state =
                      write_place parent (Value.array array) state
                    in
                    evaluated key state
                | None -> Phpsymex.error Error.Array_append_overflow)
          | Some expression ->
              let*** value, state =
                eval_expression functions state expression
              in
              let** array =
                match (access, read_place state parent) with
                | Unset, Value.Array array -> Phpsymex.Result.ok array
                | Unset, _ -> Phpsymex.Result.ok Value.empty_array
                | (Read | Write), value ->
                    array_for_access ~for_write:(access = Write) value
              in
              let** key = normalized_array_key value in
              let** key =
                resolve_array_key ~for_write:(access = Write) array key
              in
              evaluated key state
        in
        evaluated (Array_element (parent, key)) state
    | Object_property_lvalue (object_, name) -> (
        let*** object_, state =
          resolve_lvalue functions state ~access object_
        in
        match read_place state object_ with
        | Value.Object object_id ->
            let object_state =
              match State.find_object object_id state with
              | Some object_ -> object_
              | None -> failwith "PHP object value refers to an unknown object"
            in
            if not (State.object_declares_property object_id name state) then
              unsupported "undeclared property %s::$%s" object_state.class_name
                name
            else if
              access = Read
              && Option.is_none
                   (State.find_object_property_cell object_id name state)
            then
              unsupported "read of unset property %s::$%s"
                object_state.class_name name
            else evaluated (Object_property (object_id, name)) state
        | value -> unsupported "property access on %s" (Value.type_name value))
  in
  Phpsymex.with_location ~location:lvalue.location process

and eval_short_circuit functions state left operator right =
  let open Phpsymex.Syntax in
  let*** left, state = eval_expression functions state left in
  let** guard = condition left in
  match operator with
  | Php_ir.Boolean_and ->
      Phpsymex.branch_on guard ~left_branch_name:"Evaluate right operand"
        ~right_branch_name:"Short-circuit false"
        ~then_:(fun () ->
          let*** right, state = eval_expression functions state right in
          let** right = condition right in
          evaluated (Value.Bool right) state)
        ~else_:(fun () -> evaluated (Value.bool false) state)
  | Boolean_or ->
      Phpsymex.branch_on guard ~left_branch_name:"Short-circuit true"
        ~right_branch_name:"Evaluate right operand"
        ~then_:(fun () -> evaluated (Value.bool true) state)
        ~else_:(fun () ->
          let*** right, state = eval_expression functions state right in
          let** right = condition right in
          evaluated (Value.Bool right) state)
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
        | Some value -> evaluated value state
        | None -> unsupported "read of undefined variable $%s" name)
    | Array_get target ->
        let*** place, state =
          resolve_lvalue functions state ~access:Read target
        in
        let value = read_place state place in
        if Value.kind value = `Undefined then
          unsupported "read of an undefined array offset"
        else evaluated value state
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
        let** state = write_place place value state in
        evaluated value state
    | Assign_reference (target, source) ->
        let*** target, state =
          resolve_lvalue functions state ~access:Write target
        in
        let*** source, state =
          resolve_lvalue functions state ~access:Write source
        in
        let** cell, state = cell_for_reference source state in
        let** state = bind_place_reference target cell state in
        let value =
          State.find_cell cell state |> Option.value ~default:Value.undef
        in
        evaluated value state
    | Unary (operator, expression) ->
        let*** value, state = eval_expression functions state expression in
        let** value = unary operator value in
        evaluated value state
    | Binary (left, ((Boolean_and | Boolean_or) as operator), right) ->
        eval_short_circuit functions state left operator right
    | Binary (left, operator, right) ->
        let*** left, state = eval_expression functions state left in
        let*** right, state = eval_expression functions state right in
        let** value = binary state operator left right in
        evaluated value state
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
        | Some implementation ->
            let** value = implementation ~args:arguments in
            evaluated value state
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
        Phpsymex.error
          (Error.Invalid_argument_count
             { function_name = function_.name ^ "()"; expected; actual })
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
      let output = Option.get (Value.string_value value) in
      emit_expressions functions (State.emit output state) expressions

and unset_lvalues functions state lvalues =
  let open Phpsymex.Syntax in
  match lvalues with
  | [] -> evaluated () state
  | lvalue :: lvalues ->
      let*** place, state =
        resolve_lvalue functions state ~access:Unset lvalue
      in
      let** state = unset_place place state in
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
      Phpsymex.branch_on guard ~left_branch_name:"While body"
        ~right_branch_name:"While exit"
        ~then_:(fun () ->
          let** control, state = exec_statements functions state body in
          match control with
          | Normal | Continue 1 ->
              exec_while functions state condition_expression body
          | Break 1 -> Phpsymex.Result.ok (Normal, state)
          | Break depth -> Phpsymex.Result.ok (Break (depth - 1), state)
          | Continue depth -> Phpsymex.Result.ok (Continue (depth - 1), state)
          | Return _ | Throw _ -> Phpsymex.Result.ok (control, state))
        ~else_:(fun () -> Phpsymex.Result.ok (Normal, state)))

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
            Phpsymex.branch_on guard ~left_branch_name:"If branch"
              ~right_branch_name:"Else branch"
              ~then_:(fun () -> exec_statements functions state then_)
              ~else_:(fun () -> exec_statements functions state else_))
    | While (condition_expression, body, _) ->
        exec_while functions state condition_expression body
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
