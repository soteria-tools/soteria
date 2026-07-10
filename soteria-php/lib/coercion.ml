type target = Boolean | Integer | Float | String

type error =
  | Undefined_value of target
  | Symbolic_conversion of { source : Value.kind; target : target }
  | Invalid_conversion of { source : Value.kind; target : target }
  | Invalid_numeric_operand of Value.kind
  | Leading_numeric_string of string
  | Invalid_comparison of { left : Value.kind; right : Value.kind }
  | Invalid_array_key of Value.kind
  | Symbolic_array_key of Value.kind

type number = Integer of int64 | Float of float

type numeric_string =
  | Numeric of number
  | Leading_numeric of number
  | Non_numeric

type comparison =
  | Equal
  | Less_than
  | Less_than_or_equal
  | Greater_than
  | Greater_than_or_equal

type array_key =
  | Concrete_key of Value.array_key
  | Symbolic_integer_key of Value.Typed.T.sint Value.Typed.t

let target_name = function
  | Boolean -> "bool"
  | Integer -> "int"
  | Float -> "float"
  | String -> "string"

let pp_error formatter = function
  | Undefined_value target ->
      Format.fprintf formatter "cannot coerce an undefined value to %s"
        (target_name target)
  | Symbolic_conversion { source; target } ->
      Format.fprintf formatter "symbolic %s-to-%s coercion is not supported"
        (Value.kind_name source) (target_name target)
  | Invalid_conversion { source; target } ->
      Format.fprintf formatter "cannot coerce %s to %s" (Value.kind_name source)
        (target_name target)
  | Invalid_numeric_operand source ->
      Format.fprintf formatter "%s is not a valid numeric operand"
        (Value.kind_name source)
  | Leading_numeric_string value ->
      Format.fprintf formatter
        "leading-numeric string %S requires an unmodelled PHP warning" value
  | Invalid_comparison { left; right } ->
      Format.fprintf formatter "cannot compare %s and %s" (Value.kind_name left)
        (Value.kind_name right)
  | Invalid_array_key source ->
      Format.fprintf formatter "%s cannot be used as an array key"
        (Value.kind_name source)
  | Symbolic_array_key source ->
      Format.fprintf formatter "symbolic %s array keys are not supported"
        (Value.kind_name source)

let undefined target = Error (Undefined_value target)

let symbolic source target =
  Error (Symbolic_conversion { source = Value.kind source; target })

let to_bool = function
  | Value.Undef -> undefined Boolean
  | Value.Null -> Ok (Value.bool false)
  | Value.Bool _ as value -> Ok value
  | Value.Int value -> Ok (Value.Bool (Value.Typed.BitVec.to_bool value))
  | Value.Float value as source -> (
      match Value.float_value source with
      | Some value -> Ok (Value.bool (value <> 0.0))
      | None ->
          let zero = Value.Typed.Float.f64 0.0 in
          Ok
            (Value.Bool (Value.Typed.Bool.not (Value.Typed.Float.eq value zero)))
      )
  | Value.String value ->
      Ok (Value.bool (not (String.equal value "" || String.equal value "0")))
  | Value.Array array -> Ok (Value.bool (not (Value.array_is_empty array)))
  | Value.Object _ -> Ok (Value.bool true)

let integer_min = Z.of_int64 Int64.min_int
let integer_max = Z.of_int64 Int64.max_int
let integer_modulus = Z.shift_left Z.one Value.integer_bits
let integer_sign_bit = Z.shift_left Z.one (Value.integer_bits - 1)
let float_integer_limit = Float.ldexp 1.0 (Value.integer_bits - 1)
let int_of_z value = Value.int (Z.to_int64 value)

let clamp_integer value =
  if Z.lt value integer_min then integer_min
  else if Z.gt value integer_max then integer_max
  else value

let wrap_integer value =
  let unsigned = Z.erem value integer_modulus in
  let unsigned =
    if Z.sign unsigned < 0 then Z.add unsigned integer_modulus else unsigned
  in
  if Z.geq unsigned integer_sign_bit then Z.sub unsigned integer_modulus
  else unsigned

let integer_of_float ~overflow value =
  match classify_float value with
  | FP_nan | FP_infinite ->
      if overflow = `Wrap then Z.zero
      else if value < 0.0 then integer_min
      else integer_max
  | FP_normal | FP_subnormal | FP_zero ->
      if overflow = `Clamp && value >= float_integer_limit then integer_max
      else if overflow = `Clamp && value <= -.float_integer_limit then
        integer_min
      else
        let value = Z.of_float value in
        if overflow = `Wrap then wrap_integer value else clamp_integer value

let numeric_prefix_re =
  Str.regexp
    "^[ \t\n\
     \r\011\012]*[+-]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][+-]?[0-9]+\\)?"

let numeric_prefix value =
  try
    ignore (Str.search_forward numeric_prefix_re value 0);
    Some (Str.matched_string value, Str.match_end ())
  with Not_found -> None

let is_php_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true
  | _ -> false

let trim_php_whitespace value =
  let first = ref 0 in
  let last = ref (String.length value - 1) in
  while !first <= !last && is_php_whitespace value.[!first] do
    incr first
  done;
  while !last >= !first && is_php_whitespace value.[!last] do
    decr last
  done;
  String.sub value !first (!last - !first + 1)

let has_float_syntax value =
  String.exists (function '.' | 'e' | 'E' -> true | _ -> false) value

let z_of_integer_prefix value =
  let value =
    if String.length value > 0 && value.[0] = '+' then
      String.sub value 1 (String.length value - 1)
    else value
  in
  Z.of_string value

let number_of_numeric_lexeme value =
  let value = trim_php_whitespace value in
  if has_float_syntax value then Float (float_of_string value)
  else
    let integer = z_of_integer_prefix value in
    if Z.lt integer integer_min || Z.gt integer integer_max then
      Float (float_of_string value)
    else Integer (Z.to_int64 integer)

let classify_numeric_string value =
  match numeric_prefix value with
  | None -> Non_numeric
  | Some (prefix, end_) ->
      let number = number_of_numeric_lexeme prefix in
      let rec only_whitespace index =
        index = String.length value
        || (is_php_whitespace value.[index] && only_whitespace (index + 1))
      in
      if only_whitespace end_ then Numeric number else Leading_numeric number

let integer_numeric_string value =
  match numeric_prefix value with
  | Some (prefix, end_)
    when (not (has_float_syntax prefix))
         && String.for_all is_php_whitespace
              (String.sub value end_ (String.length value - end_)) ->
      Some (trim_php_whitespace prefix |> z_of_integer_prefix)
  | Some _ | None -> None

let int_of_string value =
  match numeric_prefix value with
  | None -> Z.zero
  | Some (value, _) when has_float_syntax value ->
      float_of_string value |> integer_of_float ~overflow:`Clamp
  | Some (value, _) ->
      trim_php_whitespace value |> z_of_integer_prefix |> clamp_integer

let to_int = function
  | Value.Undef -> undefined Integer
  | Value.Null -> Ok (Value.int 0L)
  | Value.Bool value ->
      Ok
        (Value.Int
           (Value.Typed.BitVec.of_bool Value.integer_bits value
             :> Value.Typed.T.sint Value.Typed.t))
  | Value.Int _ as value -> Ok value
  | Value.Float value as source -> (
      match Value.float_value source with
      | Some value -> Ok (integer_of_float ~overflow:`Wrap value |> int_of_z)
      | None -> symbolic (Value.Float value) Integer)
  | Value.String value -> Ok (int_of_string value |> int_of_z)
  | Value.Array array ->
      Ok (Value.int (if Value.array_is_empty array then 0L else 1L))
  | Value.Object _ ->
      Error (Invalid_conversion { source = `Object; target = Integer })

let float_of_numeric_string value =
  match numeric_prefix value with
  | None -> 0.0
  | Some (value, _) -> float_of_string value

let to_float = function
  | Value.Undef -> undefined Float
  | Value.Null -> Ok (Value.float 0.0)
  | Value.Bool value -> (
      match Value.Typed.Bool.to_bool value with
      | Some value -> Ok (Value.float (if value then 1.0 else 0.0))
      | None ->
          let integer = Value.Typed.BitVec.of_bool Value.integer_bits value in
          Ok
            (Value.Float
               (Value.Typed.BitVec.to_float
                  ~rounding:Value.Typed.RoundingMode.NearestTiesToEven
                  ~signed:true ~fp:Value.Typed.FloatPrecision.F64 integer)))
  | Value.Int value as source -> (
      match Value.int_value source with
      | Some value -> Ok (Value.float (Int64.to_float value))
      | None ->
          Ok
            (Value.Float
               (Value.Typed.BitVec.to_float
                  ~rounding:Value.Typed.RoundingMode.NearestTiesToEven
                  ~signed:true ~fp:Value.Typed.FloatPrecision.F64 value)))
  | Value.Float _ as value -> Ok value
  | Value.String value -> Ok (Value.float (float_of_numeric_string value))
  | Value.Array array ->
      Ok (Value.float (if Value.array_is_empty array then 0.0 else 1.0))
  | Value.Object _ ->
      Error (Invalid_conversion { source = `Object; target = Float })

let add_decimal_to_exponent value =
  match String.index_opt value 'E' with
  | None -> value
  | Some index when String.contains (String.sub value 0 index) '.' -> value
  | Some index ->
      String.sub value 0 index
      ^ ".0"
      ^ String.sub value index (String.length value - index)

let string_of_float value =
  match classify_float value with
  | FP_nan -> "NAN"
  | FP_infinite -> if value < 0.0 then "-INF" else "INF"
  | FP_normal | FP_subnormal | FP_zero ->
      Printf.sprintf "%.14G" value |> add_decimal_to_exponent

let to_string = function
  | Value.Undef -> undefined String
  | Value.Null -> Ok (Value.string "")
  | Value.Bool value as source -> (
      match Value.Typed.Bool.to_bool value with
      | Some false -> Ok (Value.string "")
      | Some true -> Ok (Value.string "1")
      | None -> symbolic source String)
  | Value.Int _ as source -> (
      match Value.int_value source with
      | Some value -> Ok (Value.string (Int64.to_string value))
      | None -> symbolic source String)
  | Value.Float _ as source -> (
      match Value.float_value source with
      | Some value -> Ok (Value.string (string_of_float value))
      | None -> symbolic source String)
  | Value.String _ as value -> Ok value
  | Value.Array _ ->
      Error (Invalid_conversion { source = `Array; target = String })
  | Value.Object _ ->
      Error (Invalid_conversion { source = `Object; target = String })

let integer_string_array_key value =
  let length = String.length value in
  let first_digit = if length > 0 && value.[0] = '-' then 1 else 0 in
  let rec all_digits index =
    if index = length then true
    else
      match value.[index] with
      | '0' .. '9' -> all_digits (index + 1)
      | _ -> false
  in
  if first_digit = length || not (all_digits first_digit) then None
  else if value.[first_digit] = '0' && length - first_digit > 1 then None
  else if String.equal value "-0" then None
  else Int64.of_string_opt value

let to_array_key = function
  | Value.Undef -> Error (Invalid_array_key `Undefined)
  | Value.Null -> Ok (Concrete_key (Value.String_key ""))
  | Value.Bool value -> (
      match Value.Typed.Bool.to_bool value with
      | Some value ->
          Ok (Concrete_key (Value.Integer_key (if value then 1L else 0L)))
      | None ->
          Ok
            (Symbolic_integer_key
               (Value.Typed.BitVec.of_bool Value.integer_bits value
                 :> Value.Typed.T.sint Value.Typed.t)))
  | Value.Int value as source -> (
      match Value.int_value source with
      | Some value -> Ok (Concrete_key (Value.Integer_key value))
      | None -> Ok (Symbolic_integer_key value))
  | Value.Float _ as source -> (
      match Value.float_value source with
      | Some value ->
          let value = integer_of_float ~overflow:`Wrap value |> Z.to_int64 in
          Ok (Concrete_key (Value.Integer_key value))
      | None -> Error (Symbolic_array_key `Float))
  | Value.String value -> (
      match integer_string_array_key value with
      | Some value -> Ok (Concrete_key (Value.Integer_key value))
      | None -> Ok (Concrete_key (Value.String_key value)))
  | Value.Array _ -> Error (Invalid_array_key `Array)
  | Value.Object _ -> Error (Invalid_array_key `Object)

let coerce target value =
  match target with
  | Boolean -> to_bool value
  | Integer -> to_int value
  | Float -> to_float value
  | String -> to_string value

let value_of_number = function
  | Integer value -> Value.int value
  | Float value -> Value.float value

let to_number = function
  | Value.Undef -> Error (Invalid_numeric_operand `Undefined)
  | Value.Null -> Ok (Value.int 0L)
  | Value.Bool _ as value -> to_int value
  | (Value.Int _ | Value.Float _) as value -> Ok value
  | Value.String value -> (
      match classify_numeric_string value with
      | Numeric number -> Ok (value_of_number number)
      | Leading_numeric _ -> Error (Leading_numeric_string value)
      | Non_numeric -> Error (Invalid_numeric_operand `String))
  | Value.Array _ -> Error (Invalid_numeric_operand `Array)
  | Value.Object _ -> Error (Invalid_numeric_operand `Object)

let compare_boolean operator left right =
  let open Value.Typed.Bool in
  match operator with
  | Equal -> Value.Typed.sem_eq left right
  | Less_than -> and_ (not left) right
  | Less_than_or_equal -> or_ (not left) right
  | Greater_than -> and_ left (not right)
  | Greater_than_or_equal -> or_ left (not right)

let compare_integer operator left right =
  match operator with
  | Equal -> Value.Typed.sem_eq left right
  | Less_than -> Value.Typed.BitVec.lt ~signed:true left right
  | Less_than_or_equal -> Value.Typed.BitVec.leq ~signed:true left right
  | Greater_than -> Value.Typed.BitVec.gt ~signed:true left right
  | Greater_than_or_equal -> Value.Typed.BitVec.geq ~signed:true left right

let compare_float operator left right =
  match operator with
  | Equal -> Value.Typed.Float.eq left right
  | Less_than -> Value.Typed.Float.lt left right
  | Less_than_or_equal -> Value.Typed.Float.leq left right
  | Greater_than -> Value.Typed.Float.gt left right
  | Greater_than_or_equal -> Value.Typed.Float.geq left right

let compare_ordering operator ordering =
  match operator with
  | Equal -> ordering = 0
  | Less_than -> ordering < 0
  | Less_than_or_equal -> ordering <= 0
  | Greater_than -> ordering > 0
  | Greater_than_or_equal -> ordering >= 0

let concrete_number = function
  | Value.Int _ as value ->
      Option.map (fun value -> Integer value) (Value.int_value value)
  | Value.Float _ as value ->
      Option.map (fun value -> Float value) (Value.float_value value)
  | _ -> None

let compare_concrete_numbers operator left right =
  match (left, right) with
  | Integer left, Integer right ->
      Value.Typed.Bool.of_bool
        (compare_ordering operator (Int64.compare left right))
  | _ ->
      let to_float = function
        | Integer value -> Int64.to_float value
        | Float value -> value
      in
      let left = to_float left in
      let right = to_float right in
      Value.Typed.Bool.of_bool
        (if Float.is_nan left || Float.is_nan right then false
         else compare_ordering operator (Float.compare left right))

let compare_numbers operator left right =
  match (concrete_number left, concrete_number right) with
  | Some left, Some right -> Ok (compare_concrete_numbers operator left right)
  | _ -> (
      match (left, right) with
      | Value.Int left, Value.Int right ->
          Ok (compare_integer operator left right)
      | Value.Float left, Value.Float right ->
          Ok (compare_float operator left right)
      | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
          let to_float = function
            | Value.Int value ->
                Value.Typed.BitVec.to_float
                  ~rounding:Value.Typed.RoundingMode.NearestTiesToEven
                  ~signed:true ~fp:Value.Typed.FloatPrecision.F64 value
            | Value.Float value -> value
            | _ -> assert false
          in
          Ok (compare_float operator (to_float left) (to_float right))
      | _ -> assert false)

let compare_strings operator left right =
  match (integer_numeric_string left, integer_numeric_string right) with
  | Some left, Some right ->
      Ok
        (Value.Typed.Bool.of_bool
           (compare_ordering operator (Z.compare left right)))
  | _ -> (
      match (classify_numeric_string left, classify_numeric_string right) with
      | Numeric left, Numeric right ->
          compare_numbers operator (value_of_number left)
            (value_of_number right)
      | _ ->
          Ok
            (Value.Typed.Bool.of_bool
               (compare_ordering operator (String.compare left right))))

let compare_number_and_string operator number string =
  match classify_numeric_string string with
  | Numeric string -> compare_numbers operator number (value_of_number string)
  | Leading_numeric _ | Non_numeric -> (
      match concrete_number number with
      | Some (Float value) when Float.is_nan value ->
          Ok Value.Typed.Bool.v_false
      | Some _ -> (
          match to_string number with
          | Ok (Value.String number) ->
              Ok
                (Value.Typed.Bool.of_bool
                   (compare_ordering operator (String.compare number string)))
          | Ok _ -> assert false
          | Error error -> Error error)
      | None ->
          Error
            (Symbolic_conversion { source = Value.kind number; target = String })
      )

let compare_scalar operator left right =
  match (left, right) with
  | Value.Bool left, right -> (
      match to_bool right with
      | Ok (Value.Bool right) -> Ok (compare_boolean operator left right)
      | Ok _ -> assert false
      | Error error -> Error error)
  | left, Value.Bool right -> (
      match to_bool left with
      | Ok (Value.Bool left) -> Ok (compare_boolean operator left right)
      | Ok _ -> assert false
      | Error error -> Error error)
  | Value.Null, Value.String right -> compare_strings operator "" right
  | Value.String left, Value.Null -> compare_strings operator left ""
  | Value.Null, right -> (
      match to_bool right with
      | Ok (Value.Bool right) ->
          Ok (compare_boolean operator Value.Typed.Bool.v_false right)
      | Ok _ -> assert false
      | Error error -> Error error)
  | left, Value.Null -> (
      match to_bool left with
      | Ok (Value.Bool left) ->
          Ok (compare_boolean operator left Value.Typed.Bool.v_false)
      | Ok _ -> assert false
      | Error error -> Error error)
  | (Value.Int _ | Value.Float _), (Value.Int _ | Value.Float _) ->
      compare_numbers operator left right
  | Value.String left, Value.String right -> compare_strings operator left right
  | (Value.Int _ | Value.Float _), Value.String right ->
      compare_number_and_string operator left right
  | Value.String left, ((Value.Int _ | Value.Float _) as right) ->
      let reverse = function
        | Equal -> Equal
        | Less_than -> Greater_than
        | Less_than_or_equal -> Greater_than_or_equal
        | Greater_than -> Less_than
        | Greater_than_or_equal -> Less_than_or_equal
      in
      compare_number_and_string (reverse operator) right left
  | _ ->
      Error
        (Invalid_comparison { left = Value.kind left; right = Value.kind right })
