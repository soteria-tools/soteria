module Typed =
  Soteria.Bv_values.Typed.Make (Soteria.Bv_values.Svalue.Dummy_ext) ()

let integer_bits = 64

type array_key =
  | Integer_key of int64
  | String_key of string
  | Symbolic_integer_key of Typed.T.sint Typed.t

let compare_array_key left right =
  match (left, right) with
  | Integer_key left, Integer_key right -> Int64.compare left right
  | String_key left, String_key right -> String.compare left right
  | Symbolic_integer_key left, Symbolic_integer_key right ->
      Typed.compare left right
  | Integer_key _, (String_key _ | Symbolic_integer_key _) -> -1
  | String_key _, Integer_key _ -> 1
  | String_key _, Symbolic_integer_key _ -> -1
  | Symbolic_integer_key _, (Integer_key _ | String_key _) -> 1

let same_array_key left right = compare_array_key left right = 0

module Array_key_map = Map.Make (struct
  type t = array_key

  let compare = compare_array_key
end)

type t =
  | Undef
  | Null
  | Bool of Typed.T.sbool Typed.t
  | Int of Typed.T.sint Typed.t
  | Float of Typed.T.sfloat Typed.t
  | String of string
  | Array of php_array
  | Object of int

and php_array = {
  entries : array_entry Array_key_map.t;
  order_rev : array_key list;
  max_integer_key : int64 option;
  symbolic_integer_history : bool;
}

and array_entry = Inline of t | Reference of int

type kind =
  [ `Undefined
  | `Null
  | `Boolean
  | `Integer
  | `Float
  | `String
  | `Array
  | `Object ]

let undef = Undef
let null = Null
let bool value = Bool (Typed.Bool.of_bool value)
let int value = Int (Typed.BitVec.mk_masked integer_bits (Z.of_int64 value))

let float value =
  Float (Typed.Float.mk Typed.FloatPrecision.F64 (Printf.sprintf "%.17g" value))

let string value = String value

let empty_array =
  {
    entries = Array_key_map.empty;
    order_rev = [];
    max_integer_key = None;
    symbolic_integer_history = false;
  }

let array value = Array value
let object_ id = Object id

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
  | Array _ -> `Array
  | Object _ -> `Object

let kind_name = function
  | `Undefined -> "undefined"
  | `Null -> "null"
  | `Boolean -> "bool"
  | `Integer -> "int"
  | `Float -> "float"
  | `String -> "string"
  | `Array -> "array"
  | `Object -> "object"

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
let array_value = function Array value -> Some value | _ -> None
let object_value = function Object id -> Some id | _ -> None
let array_is_empty array = Array_key_map.is_empty array.entries
let array_length array = Array_key_map.cardinal array.entries
let array_find key array = Array_key_map.find_opt key array.entries

let array_bindings array =
  array.order_rev
  |> List.rev
  |> List.filter_map (fun key ->
      Option.map
        (fun entry -> (key, entry))
        (Array_key_map.find_opt key array.entries))

let array_integer_keys array =
  array_bindings array
  |> List.filter_map (function
    | ((Integer_key _ | Symbolic_integer_key _) as key), _ -> Some key
    | String_key _, _ -> None)

let array_next_key_is_symbolic array = array.symbolic_integer_history

let array_set_entry key entry array =
  let ordered = List.exists (same_array_key key) array.order_rev in
  let symbolic_integer_key =
    match key with Symbolic_integer_key _ -> true | _ -> false
  in
  let max_integer_key =
    match (key, array.max_integer_key) with
    | Integer_key key, None -> Some key
    | Integer_key key, Some maximum when Int64.compare key maximum > 0 ->
        Some key
    | _ -> array.max_integer_key
  in
  {
    entries = Array_key_map.add key entry array.entries;
    order_rev = (if ordered then array.order_rev else key :: array.order_rev);
    max_integer_key;
    symbolic_integer_history =
      array.symbolic_integer_history || symbolic_integer_key;
  }

let array_set key value array = array_set_entry key (Inline value) array

let array_set_reference key cell array =
  array_set_entry key (Reference cell) array

let array_remove key array =
  {
    array with
    entries = Array_key_map.remove key array.entries;
    order_rev =
      List.filter
        (fun existing -> not (same_array_key existing key))
        array.order_rev;
  }

let array_next_key array =
  match array.max_integer_key with
  | None -> Some (Integer_key 0L)
  | Some key when Int64.equal key Int64.max_int -> None
  | Some key -> Some (Integer_key (Int64.succ key))

let array_reserve_next array =
  match array_next_key array with
  | None -> None
  | Some (String_key _ | Symbolic_integer_key _) -> assert false
  | Some (Integer_key key as array_key) ->
      Some
        ( array_key,
          {
            array with
            order_rev = array_key :: array.order_rev;
            max_integer_key = Some key;
          } )

let array_union left right =
  List.fold_left
    (fun result (key, entry) ->
      if Array_key_map.mem key result.entries then result
      else array_set_entry key entry result)
    left (array_bindings right)

let pp_array_key formatter = function
  | Integer_key key -> Format.pp_print_string formatter (Int64.to_string key)
  | String_key key -> Format.fprintf formatter "%S" key
  | Symbolic_integer_key key -> Format.fprintf formatter "int(%a)" Typed.ppa key

let rec pp formatter = function
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
  | Array array ->
      let pp_binding formatter (key, entry) =
        match entry with
        | Inline value ->
            Format.fprintf formatter "%a => %a" pp_array_key key pp value
        | Reference cell ->
            Format.fprintf formatter "%a => &cell(%d)" pp_array_key key cell
      in
      Format.fprintf formatter "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun formatter () -> Format.pp_print_string formatter ", ")
           pp_binding)
        (array_bindings array)
  | Object id -> Format.fprintf formatter "object(%d)" id
