let unsupported format = Format.kasprintf Phpsymex.not_impl format
let coercion_error error = unsupported "%a" Coercion.pp_error error

let coerce target value =
  match Coercion.coerce target value with
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
  | (Value.Undef | Value.Null | Value.String _ | Value.Array _) as value ->
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

let checked_integer operation check left right =
  let result, overflow = check ~signed:true left right in
  Phpsymex.branch_on overflow ~left_branch_name:"Integer overflow"
    ~right_branch_name:"Integer result"
    ~then_:(fun () -> unsupported "integer overflow in %s" operation)
    ~else_:(fun () -> Phpsymex.Result.ok (Value.Int result))

let arithmetic operation integer float concrete_float left right =
  match (left, right) with
  | Value.Int left, Value.Int right ->
      checked_integer operation integer left right
  | _ -> (
      match (concrete_numeric_float left, concrete_numeric_float right) with
      | Some left, Some right ->
          Phpsymex.Result.ok (Value.float (concrete_float left right))
      | _ -> (
          match (numeric_float left, numeric_float right) with
          | Some left, Some right ->
              Phpsymex.Result.ok (Value.Float (float left right))
          | _ ->
              unsupported "operator %s for %s and %s" operation
                (Value.type_name left) (Value.type_name right)))

let symbolic_integer_division numerator denominator =
  let open Phpsymex.Syntax in
  let zero = Value.Typed.BitVec.zero Value.integer_bits in
  let denominator_is_zero = Value.Typed.sem_eq denominator zero in
  Phpsymex.branch_on denominator_is_zero ~left_branch_name:"Division by zero"
    ~right_branch_name:"Division"
    ~then_:(fun () -> Phpsymex.error Error.Division_by_zero)
    ~else_:(fun () ->
      let min_int =
        Value.Typed.BitVec.mk_masked Value.integer_bits
          (Z.of_int64 Int64.min_int)
      in
      let minus_one =
        Value.Typed.BitVec.mk_masked Value.integer_bits Z.minus_one
      in
      let overflows =
        Value.Typed.Bool.and_
          (Value.Typed.sem_eq numerator min_int)
          (Value.Typed.sem_eq denominator minus_one)
      in
      Phpsymex.branch_on overflows ~left_branch_name:"Division overflow"
        ~right_branch_name:"Division in range"
        ~then_:(fun () ->
          let numerator = Option.get (numeric_float (Value.Int numerator)) in
          let denominator =
            Option.get (numeric_float (Value.Int denominator))
          in
          Phpsymex.Result.ok
            (Value.Float (Value.Typed.Float.div numerator denominator)))
        ~else_:(fun () ->
          let denominator = Value.Typed.BitVec.cast_nonzero denominator in
          let quotient =
            Value.Typed.BitVec.div ~signed:true numerator denominator
            |> Value.Typed.BitVec.no_ovf_unsafe
          in
          let remainder =
            Value.Typed.BitVec.rem ~signed:true numerator denominator
            |> Value.Typed.BitVec.no_ovf_unsafe
          in
          let divides_evenly = Value.Typed.sem_eq remainder zero in
          Phpsymex.branch_on divides_evenly
            ~left_branch_name:"Integral quotient"
            ~right_branch_name:"Fractional quotient"
            ~then_:(fun () -> Phpsymex.Result.ok (Value.Int quotient))
            ~else_:(fun () ->
              let numerator =
                Option.get (numeric_float (Value.Int numerator))
              in
              let denominator =
                Option.get (numeric_float (Value.Int denominator))
              in
              Phpsymex.Result.ok
                (Value.Float (Value.Typed.Float.div numerator denominator)))))

let division left right =
  match (left, right) with
  | Value.Int numerator, Value.Int denominator -> (
      match
        ( Value.int_value (Value.Int numerator),
          Value.int_value (Value.Int denominator) )
      with
      | _, Some 0L -> Phpsymex.error Error.Division_by_zero
      | Some numerator, Some denominator
        when Int64.equal numerator Int64.min_int
             && Int64.equal denominator (-1L) ->
          Phpsymex.Result.ok
            (Value.float
               (Int64.to_float numerator /. Int64.to_float denominator))
      | Some numerator, Some denominator ->
          if Int64.equal (Int64.rem numerator denominator) 0L then
            Phpsymex.Result.ok (Value.int (Int64.div numerator denominator))
          else
            Phpsymex.Result.ok
              (Value.float
                 (Int64.to_float numerator /. Int64.to_float denominator))
      | _ -> symbolic_integer_division numerator denominator)
  | _ -> (
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
                ~left_branch_name:"Division by zero"
                ~right_branch_name:"Division"
                ~then_:(fun () -> Phpsymex.error Error.Division_by_zero)
                ~else_:(fun () ->
                  Phpsymex.Result.ok
                    (Value.Float (Value.Typed.Float.div numerator denominator)))
          | _ ->
              unsupported "operator / for %s and %s" (Value.type_name left)
                (Value.type_name right)))

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
  match (left, right) with
  | ( (Value.Undef | Value.Null | Value.Bool _),
      (Value.Undef | Value.Null | Value.Bool _) ) ->
      let open Phpsymex.Syntax in
      let** left = coerce Coercion.Boolean left in
      let** right = coerce Coercion.Boolean right in
      let left =
        match left with Value.Bool value -> value | _ -> assert false
      in
      let right =
        match right with Value.Bool value -> value | _ -> assert false
      in
      Phpsymex.Result.ok (Value.Bool (Value.Typed.sem_eq left right))
  | Value.Int left, Value.Int right ->
      Phpsymex.Result.ok (Value.Bool (Value.Typed.sem_eq left right))
  | Value.Float left, Value.Float right ->
      Phpsymex.Result.ok (Value.Bool (Value.Typed.Float.eq left right))
  | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
      let left = Option.get (numeric_float left) in
      let right = Option.get (numeric_float right) in
      Phpsymex.Result.ok (Value.Bool (Value.Typed.Float.eq left right))
  | _ ->
      unsupported "loose equality for %s and %s" (Value.type_name left)
        (Value.type_name right)

let comparison operator left right =
  let compare_int left right =
    match operator with
    | Php_ir.Less_than -> Value.Typed.BitVec.lt ~signed:true left right
    | Less_than_or_equal -> Value.Typed.BitVec.leq ~signed:true left right
    | Greater_than -> Value.Typed.BitVec.gt ~signed:true left right
    | Greater_than_or_equal -> Value.Typed.BitVec.geq ~signed:true left right
    | _ -> failwith "non-ordering operator passed to comparison"
  in
  let compare_float left right =
    match operator with
    | Php_ir.Less_than -> Value.Typed.Float.lt left right
    | Less_than_or_equal -> Value.Typed.Float.leq left right
    | Greater_than -> Value.Typed.Float.gt left right
    | Greater_than_or_equal -> Value.Typed.Float.geq left right
    | _ -> failwith "non-ordering operator passed to comparison"
  in
  match (left, right) with
  | Value.Int left, Value.Int right ->
      Phpsymex.Result.ok (Value.Bool (compare_int left right))
  | _ -> (
      match (numeric_float left, numeric_float right) with
      | Some left, Some right ->
          Phpsymex.Result.ok (Value.Bool (compare_float left right))
      | _ ->
          unsupported "ordering comparison for %s and %s" (Value.type_name left)
            (Value.type_name right))

let binary state operator left right =
  match operator with
  | Php_ir.Add when Value.kind left = `Array && Value.kind right = `Array ->
      let left = Option.get (Value.array_value left) in
      let right = Option.get (Value.array_value right) in
      Phpsymex.Result.ok (Value.array (Value.array_union left right))
  | Php_ir.Add ->
      arithmetic "+" Value.Typed.BitVec.add_checked Value.Typed.Float.add ( +. )
        left right
  | Subtract ->
      arithmetic "-" Value.Typed.BitVec.sub_checked Value.Typed.Float.sub ( -. )
        left right
  | Multiply ->
      arithmetic "*" Value.Typed.BitVec.mul_checked Value.Typed.Float.mul ( *. )
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
  | Numeric_identity -> (
      match value with
      | Value.Int _ | Value.Float _ -> Phpsymex.Result.ok value
      | _ -> unsupported "unary + for %s" (Value.type_name value))
  | Numeric_negation -> (
      match value with
      | Value.Int value ->
          let result, overflow = Value.Typed.BitVec.neg_checked value in
          Phpsymex.branch_on overflow ~left_branch_name:"Integer overflow"
            ~right_branch_name:"Integer result"
            ~then_:(fun () -> unsupported "integer overflow in unary -")
            ~else_:(fun () -> Phpsymex.Result.ok (Value.Int result))
      | Value.Float value ->
          Phpsymex.Result.ok (Value.Float (Value.Typed.Float.neg value))
      | _ -> unsupported "unary - for %s" (Value.type_name value))

module Function_map = Map.Make (String)

type functions = Php_ir.function_decl Function_map.t
type control = Normal | Return of Value.t
type access = Read | Write | Unset
type place = Variable of string | Array_element of place * Value.array_key

let rec read_place state = function
  | Variable name ->
      Option.value ~default:Value.undef (State.find_variable name state)
  | Array_element (array, key) -> (
      match read_place state array with
      | Value.Array array ->
          State.find_array_value key array state
          |> Option.value ~default:Value.undef
      | _ -> Value.undef)

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

let unset_place place state =
  match place with
  | Variable name -> Phpsymex.Result.ok (State.unset_variable name state)
  | Array_element (parent, key) -> (
      match read_place state parent with
      | Value.Array array ->
          write_place parent (Value.array (Value.array_remove key array)) state
      | Value.Undef | Value.Null -> Phpsymex.Result.ok state
      | value -> Phpsymex.error (Error.Cannot_use_as_array (Value.kind value)))

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

let rec eval_expressions functions state expressions =
  let open Phpsymex.Syntax in
  match expressions with
  | [] -> Phpsymex.Result.ok ([], state)
  | expression :: expressions ->
      let** value, state = eval_expression functions state expression in
      let** values, state = eval_expressions functions state expressions in
      Phpsymex.Result.ok (value :: values, state)

and eval_array_items functions state array items =
  let open Phpsymex.Syntax in
  match items with
  | [] -> Phpsymex.Result.ok (Value.array array, state)
  | (item : Php_ir.array_item) :: items ->
      let process =
        let** key, state =
          match item.key with
          | None -> (
              match Value.array_next_key array with
              | Some key -> Phpsymex.Result.ok (key, state)
              | None -> Phpsymex.error Error.Array_append_overflow)
          | Some expression ->
              let** value, state = eval_expression functions state expression in
              let** key = normalized_array_key value in
              let** key = resolve_array_key ~for_write:true array key in
              Phpsymex.Result.ok (key, state)
        in
        let** value, state = eval_expression functions state item.value in
        eval_array_items functions state (Value.array_set key value array) items
      in
      Phpsymex.with_location ~location:item.location process

and resolve_lvalue functions state ~access (lvalue : Php_ir.lvalue) =
  let process =
    let open Phpsymex.Syntax in
    match lvalue.desc with
    | Variable_lvalue name -> (
        match access with
        | Write | Unset -> Phpsymex.Result.ok (Variable name, state)
        | Read -> (
            match State.find_variable_cell name state with
            | Some _ -> Phpsymex.Result.ok (Variable name, state)
            | None -> unsupported "read of undefined variable $%s" name))
    | Array_element_lvalue (parent, key_expression) ->
        let** parent, state = resolve_lvalue functions state ~access parent in
        let** key, state =
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
                    Phpsymex.Result.ok (key, state)
                | None -> Phpsymex.error Error.Array_append_overflow)
          | Some expression ->
              let** value, state = eval_expression functions state expression in
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
              Phpsymex.Result.ok (key, state)
        in
        Phpsymex.Result.ok (Array_element (parent, key), state)
  in
  Phpsymex.with_location ~location:lvalue.location process

and eval_short_circuit functions state left operator right =
  let open Phpsymex.Syntax in
  let** left, state = eval_expression functions state left in
  let** guard = condition left in
  match operator with
  | Php_ir.Boolean_and ->
      Phpsymex.branch_on guard ~left_branch_name:"Evaluate right operand"
        ~right_branch_name:"Short-circuit false"
        ~then_:(fun () ->
          let** right, state = eval_expression functions state right in
          let** right = condition right in
          Phpsymex.Result.ok (Value.Bool right, state))
        ~else_:(fun () -> Phpsymex.Result.ok (Value.bool false, state))
  | Boolean_or ->
      Phpsymex.branch_on guard ~left_branch_name:"Short-circuit true"
        ~right_branch_name:"Evaluate right operand"
        ~then_:(fun () -> Phpsymex.Result.ok (Value.bool true, state))
        ~else_:(fun () ->
          let** right, state = eval_expression functions state right in
          let** right = condition right in
          Phpsymex.Result.ok (Value.Bool right, state))
  | _ -> failwith "non-short-circuit operator passed to evaluation"

and eval_expression functions state expression =
  let process =
    let open Phpsymex.Syntax in
    let* () = Phpsymex.consume_fuel_steps 1 in
    match expression.Php_ir.desc with
    | Literal literal -> Phpsymex.Result.ok (Value.of_literal literal, state)
    | Array items -> eval_array_items functions state Value.empty_array items
    | Variable name -> (
        match State.find_variable name state with
        | Some value -> Phpsymex.Result.ok (value, state)
        | None -> unsupported "read of undefined variable $%s" name)
    | Array_get target ->
        let** place, state =
          resolve_lvalue functions state ~access:Read target
        in
        let value = read_place state place in
        if Value.kind value = `Undefined then
          unsupported "read of an undefined array offset"
        else Phpsymex.Result.ok (value, state)
    | Assign (target, expression) ->
        let** place, state =
          resolve_lvalue functions state ~access:Write target
        in
        let** value, state = eval_expression functions state expression in
        let** state = write_place place value state in
        Phpsymex.Result.ok (value, state)
    | Assign_reference (target, source) ->
        let** target, state =
          resolve_lvalue functions state ~access:Write target
        in
        let** source, state =
          resolve_lvalue functions state ~access:Write source
        in
        let** cell, state = cell_for_reference source state in
        let** state = bind_place_reference target cell state in
        let value =
          State.find_cell cell state |> Option.value ~default:Value.undef
        in
        Phpsymex.Result.ok (value, state)
    | Unary (operator, expression) ->
        let** value, state = eval_expression functions state expression in
        let** value = unary operator value in
        Phpsymex.Result.ok (value, state)
    | Binary (left, ((Boolean_and | Boolean_or) as operator), right) ->
        eval_short_circuit functions state left operator right
    | Binary (left, operator, right) ->
        let** left, state = eval_expression functions state left in
        let** right, state = eval_expression functions state right in
        let** value = binary state operator left right in
        Phpsymex.Result.ok (value, state)
    | Cast (cast, expression) ->
        let** value, state = eval_expression functions state expression in
        let target =
          match cast with
          | Php_ir.To_boolean -> Coercion.Boolean
          | To_integer -> Coercion.Integer
          | To_float -> Coercion.Float
          | To_string -> Coercion.String
        in
        let** value = coerce target value in
        Phpsymex.Result.ok (value, state)
    | Call (name, arguments) -> (
        let** arguments, state = eval_expressions functions state arguments in
        match Builtins.find name with
        | Some implementation ->
            let** value = implementation ~args:arguments in
            Phpsymex.Result.ok (value, state)
        | None ->
            call_function functions state expression.location name arguments)
  in
  Phpsymex.with_location ~location:expression.location process

and call_function functions state location name arguments =
  let canonical_name = Builtins.canonical_name name in
  match Function_map.find_opt canonical_name functions with
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
          let value =
            match control with Normal -> Value.null | Return value -> value
          in
          Phpsymex.Result.ok (value, State.leave_scope local_state)
        in
        Phpsymex.with_call ~location
          ~message:("Call to " ^ function_.name)
          process

and emit_expressions functions state expressions =
  let open Phpsymex.Syntax in
  match expressions with
  | [] -> Phpsymex.Result.ok state
  | expression :: expressions ->
      let** value, state = eval_expression functions state expression in
      let* value = simplify_value value in
      let** value = coerce Coercion.String value in
      let output = Option.get (Value.string_value value) in
      emit_expressions functions (State.emit output state) expressions

and unset_lvalues functions state lvalues =
  let open Phpsymex.Syntax in
  match lvalues with
  | [] -> Phpsymex.Result.ok state
  | lvalue :: lvalues ->
      let** place, state =
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
      | Return _ -> Phpsymex.Result.ok (control, state))

and exec_while functions state condition_expression body =
  let open Phpsymex.Syntax in
  let** value, state = eval_expression functions state condition_expression in
  let** guard = condition value in
  Phpsymex.branch_on guard ~left_branch_name:"While body"
    ~right_branch_name:"While exit"
    ~then_:(fun () ->
      let** control, state = exec_statements functions state body in
      match control with
      | Normal -> exec_while functions state condition_expression body
      | Return _ -> Phpsymex.Result.ok (control, state))
    ~else_:(fun () -> Phpsymex.Result.ok (Normal, state))

and exec_statement functions state statement =
  let location =
    match statement with
    | Php_ir.Expression (_, location)
    | Echo (_, location)
    | If (_, _, _, location)
    | While (_, _, location)
    | Return (_, location)
    | Unset (_, location)
    | Nop location ->
        location
  in
  let process =
    let open Phpsymex.Syntax in
    let* () = Phpsymex.consume_fuel_steps 1 in
    match statement with
    | Php_ir.Expression (expression, _) ->
        let** _, state = eval_expression functions state expression in
        Phpsymex.Result.ok (Normal, state)
    | Echo (expressions, _) ->
        let** state = emit_expressions functions state expressions in
        Phpsymex.Result.ok (Normal, state)
    | If (condition_expression, then_, else_, _) ->
        let** value, state =
          eval_expression functions state condition_expression
        in
        let** guard = condition value in
        Phpsymex.branch_on guard ~left_branch_name:"If branch"
          ~right_branch_name:"Else branch"
          ~then_:(fun () -> exec_statements functions state then_)
          ~else_:(fun () -> exec_statements functions state else_)
    | While (condition_expression, body, _) ->
        exec_while functions state condition_expression body
    | Return (expression, _) ->
        let** value, state =
          match expression with
          | None -> Phpsymex.Result.ok (Value.null, state)
          | Some expression -> eval_expression functions state expression
        in
        Phpsymex.Result.ok (Return value, state)
    | Unset (lvalues, _) ->
        let** state = unset_lvalues functions state lvalues in
        Phpsymex.Result.ok (Normal, state)
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

let run program =
  let open Phpsymex.Syntax in
  let** functions = collect_functions program.Php_ir.functions in
  let** _, state = exec_statements functions State.empty program.statements in
  Phpsymex.Result.ok state
