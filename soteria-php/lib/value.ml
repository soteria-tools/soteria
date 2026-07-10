module Typed =
  Soteria.Bv_values.Typed.Make (Soteria.Bv_values.Svalue.Dummy_ext) ()

let integer_bits = 64

type t =
  | Undef
  | Null
  | Bool of Typed.T.sbool Typed.t
  | Int of Typed.T.sint Typed.t
  | Float of Typed.T.sfloat Typed.t
  | String of string

type kind = [ `Undefined | `Null | `Boolean | `Integer | `Float | `String ]

let undef = Undef
let null = Null
let bool value = Bool (Typed.Bool.of_bool value)
let int value = Int (Typed.BitVec.mk_masked integer_bits (Z.of_int64 value))

let float value =
  Float (Typed.Float.mk Typed.FloatPrecision.F64 (Printf.sprintf "%.17g" value))

let string value = String value

let of_literal = function
  | Php_ir.Null -> null
  | Php_ir.Bool value -> bool value
  | Php_ir.Int value -> int value
  | Php_ir.Float value -> float value
  | Php_ir.String value -> string value

let kind = function
  | Undef -> `Undefined
  | Null -> `Null
  | Bool _ -> `Boolean
  | Int _ -> `Integer
  | Float _ -> `Float
  | String _ -> `String

let kind_name = function
  | `Undefined -> "undefined"
  | `Null -> "null"
  | `Boolean -> "bool"
  | `Integer -> "int"
  | `Float -> "float"
  | `String -> "string"

let type_name value = kind_name (kind value)
let bool_value = function Bool value -> Typed.Bool.to_bool value | _ -> None

let int_value = function
  | Int value ->
      Option.map
        (fun value ->
          Typed.BitVec.bv_to_z true integer_bits value |> Z.to_int64)
        (Typed.BitVec.to_z value)
  | _ -> None

let float_value = function
  | Float value -> (
      match Typed.kind value with
      | Soteria.Bv_values.Svalue.Float value -> Some (float_of_string value)
      | _ -> None)
  | _ -> None

let string_value = function String value -> Some value | _ -> None

let pp formatter = function
  | Undef -> Format.pp_print_string formatter "undefined"
  | Null -> Format.pp_print_string formatter "null"
  | Bool value -> (
      match Typed.Bool.to_bool value with
      | Some value -> Format.pp_print_bool formatter value
      | None -> Format.fprintf formatter "bool(%a)" Typed.ppa value)
  | Int value -> (
      match Typed.BitVec.to_z value with
      | Some value ->
          let value = Typed.BitVec.bv_to_z true integer_bits value in
          Format.pp_print_string formatter (Z.to_string value)
      | None -> Format.fprintf formatter "int(%a)" Typed.ppa value)
  | Float value -> (
      match Typed.kind value with
      | Soteria.Bv_values.Svalue.Float value ->
          Format.pp_print_string formatter value
      | _ -> Format.fprintf formatter "float(%a)" Typed.ppa value)
  | String value -> Format.fprintf formatter "%S" value
