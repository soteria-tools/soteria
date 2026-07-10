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
  | (Value.Undef | Value.Null | Value.String _) as value ->
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

let strict_equal left right =
  let result =
    match (left, right) with
    | Value.Undef, Value.Undef | Value.Null, Value.Null ->
        Value.Typed.Bool.v_true
    | Value.Bool left, Value.Bool right -> Value.Typed.sem_eq left right
    | Value.Int left, Value.Int right -> Value.Typed.sem_eq left right
    | Value.Float left, Value.Float right -> Value.Typed.Float.eq left right
    | Value.String left, Value.String right ->
        Value.Typed.Bool.of_bool (String.equal left right)
    | _ -> Value.Typed.Bool.v_false
  in
  Value.Bool result

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

let binary operator left right =
  match operator with
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
  | Identical -> Phpsymex.Result.ok (strict_equal left right)
  | Not_identical -> (
      match strict_equal left right with
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

let rec eval_expressions state expressions =
  let open Phpsymex.Syntax in
  match expressions with
  | [] -> Phpsymex.Result.ok ([], state)
  | expression :: expressions ->
      let** value, state = eval_expression state expression in
      let** values, state = eval_expressions state expressions in
      Phpsymex.Result.ok (value :: values, state)

and eval_short_circuit state left operator right =
  let open Phpsymex.Syntax in
  let** left, state = eval_expression state left in
  let** guard = condition left in
  match operator with
  | Php_ir.Boolean_and ->
      Phpsymex.branch_on guard ~left_branch_name:"Evaluate right operand"
        ~right_branch_name:"Short-circuit false"
        ~then_:(fun () ->
          let** right, state = eval_expression state right in
          let** right = condition right in
          Phpsymex.Result.ok (Value.Bool right, state))
        ~else_:(fun () -> Phpsymex.Result.ok (Value.bool false, state))
  | Boolean_or ->
      Phpsymex.branch_on guard ~left_branch_name:"Short-circuit true"
        ~right_branch_name:"Evaluate right operand"
        ~then_:(fun () -> Phpsymex.Result.ok (Value.bool true, state))
        ~else_:(fun () ->
          let** right, state = eval_expression state right in
          let** right = condition right in
          Phpsymex.Result.ok (Value.Bool right, state))
  | _ -> failwith "non-short-circuit operator passed to evaluation"

and eval_expression state expression =
  let process =
    let open Phpsymex.Syntax in
    let* () = Phpsymex.consume_fuel_steps 1 in
    match expression.Php_ir.desc with
    | Literal literal -> Phpsymex.Result.ok (Value.of_literal literal, state)
    | Variable name -> (
        match State.find_variable name state with
        | Some value -> Phpsymex.Result.ok (value, state)
        | None -> unsupported "read of undefined variable $%s" name)
    | Assign (name, expression) ->
        let** value, state = eval_expression state expression in
        Phpsymex.Result.ok (value, State.set_variable name value state)
    | Unary (operator, expression) ->
        let** value, state = eval_expression state expression in
        let** value = unary operator value in
        Phpsymex.Result.ok (value, state)
    | Binary (left, ((Boolean_and | Boolean_or) as operator), right) ->
        eval_short_circuit state left operator right
    | Binary (left, operator, right) ->
        let** left, state = eval_expression state left in
        let** right, state = eval_expression state right in
        let** value = binary operator left right in
        Phpsymex.Result.ok (value, state)
    | Cast (cast, expression) ->
        let** value, state = eval_expression state expression in
        let target =
          match cast with
          | Php_ir.To_boolean -> Coercion.Boolean
          | To_integer -> Coercion.Integer
          | To_float -> Coercion.Float
          | To_string -> Coercion.String
        in
        let** value = coerce target value in
        Phpsymex.Result.ok (value, state)
    | Call (name, arguments) ->
        let** arguments, state = eval_expressions state arguments in
        let** value = Builtins.call name arguments in
        Phpsymex.Result.ok (value, state)
  in
  Phpsymex.with_location ~location:expression.location process

let rec emit_expressions state expressions =
  let open Phpsymex.Syntax in
  match expressions with
  | [] -> Phpsymex.Result.ok state
  | expression :: expressions ->
      let** value, state = eval_expression state expression in
      let* value = simplify_value value in
      let** value = coerce Coercion.String value in
      let output = Option.get (Value.string_value value) in
      emit_expressions (State.emit output state) expressions

let rec exec_statements state statements =
  let open Phpsymex.Syntax in
  match statements with
  | [] -> Phpsymex.Result.ok state
  | statement :: statements ->
      let** state = exec_statement state statement in
      exec_statements state statements

and exec_while state condition_expression body =
  let open Phpsymex.Syntax in
  let** value, state = eval_expression state condition_expression in
  let** guard = condition value in
  Phpsymex.branch_on guard ~left_branch_name:"While body"
    ~right_branch_name:"While exit"
    ~then_:(fun () ->
      let** state = exec_statements state body in
      exec_while state condition_expression body)
    ~else_:(fun () -> Phpsymex.Result.ok state)

and exec_statement state statement =
  let location =
    match statement with
    | Php_ir.Expression (_, location)
    | Echo (_, location)
    | If (_, _, _, location)
    | While (_, _, location)
    | Nop location ->
        location
  in
  let process =
    let open Phpsymex.Syntax in
    let* () = Phpsymex.consume_fuel_steps 1 in
    match statement with
    | Php_ir.Expression (expression, _) ->
        let** _, state = eval_expression state expression in
        Phpsymex.Result.ok state
    | Echo (expressions, _) -> emit_expressions state expressions
    | If (condition_expression, then_, else_, _) ->
        let** value, state = eval_expression state condition_expression in
        let** guard = condition value in
        Phpsymex.branch_on guard ~left_branch_name:"If branch"
          ~right_branch_name:"Else branch"
          ~then_:(fun () -> exec_statements state then_)
          ~else_:(fun () -> exec_statements state else_)
    | While (condition_expression, body, _) ->
        exec_while state condition_expression body
    | Nop _ -> Phpsymex.Result.ok state
  in
  Phpsymex.with_location ~location process

let run program = exec_statements State.empty program.Php_ir.statements
