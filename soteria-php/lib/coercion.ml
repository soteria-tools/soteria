type target = Boolean | Integer | Float | String

type error =
  | Undefined_value of target
  | Symbolic_conversion of { source : Value.kind; target : target }
  | Invalid_conversion of { source : Value.kind; target : target }
  | Invalid_array_key of Value.kind
  | Symbolic_array_key of Value.kind

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
    Some (Str.matched_string value |> String.trim)
  with Not_found -> None

let has_float_syntax value =
  String.exists (function '.' | 'e' | 'E' -> true | _ -> false) value

let z_of_integer_prefix value =
  let value =
    if String.length value > 0 && value.[0] = '+' then
      String.sub value 1 (String.length value - 1)
    else value
  in
  Z.of_string value

let int_of_string value =
  match numeric_prefix value with
  | None -> Z.zero
  | Some value when has_float_syntax value ->
      float_of_string value |> integer_of_float ~overflow:`Clamp
  | Some value -> z_of_integer_prefix value |> clamp_integer

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

let float_of_numeric_string value =
  match numeric_prefix value with
  | None -> 0.0
  | Some value -> float_of_string value

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

let coerce target value =
  match target with
  | Boolean -> to_bool value
  | Integer -> to_int value
  | Float -> to_float value
  | String -> to_string value
