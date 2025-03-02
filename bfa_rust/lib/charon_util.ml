open Charon
open Typed

type rust_val =
  | Base of T.cval Typed.t
  | Enum of T.cval Typed.t * rust_val list  (** discriminant * values *)
  | Tuple of rust_val list
  | Never  (** Useful for base cases -- should be ignored *)
[@@deriving show]

let unit_ = Tuple []

let value_of_scalar : Values.scalar_value -> T.cval Typed.t = function
  | { value = v; _ } -> int_z v

let value_of_constant : Expressions.constant_expr -> T.cval Typed.t = function
  | { value = CLiteral (VScalar scalar); _ } -> value_of_scalar scalar
  | { value = CLiteral (VBool b); _ } -> int (if b then 1 else 0)
  | e ->
      Fmt.failwith "TODO: value_of_constant %a" Expressions.pp_constant_expr e

let type_of_operand : Expressions.operand -> Types.ty = function
  | Constant c -> c.ty
  | Copy p | Move p -> p.ty

let lit_to_string : Values.literal_type -> string = function
  | TInteger Isize -> "isize"
  | TInteger I8 -> "i8"
  | TInteger I16 -> "i16"
  | TInteger I32 -> "i32"
  | TInteger I64 -> "i64"
  | TInteger I128 -> "i128"
  | TInteger Usize -> "usize"
  | TInteger U8 -> "u8"
  | TInteger U16 -> "u16"
  | TInteger U32 -> "u32"
  | TInteger U64 -> "u64"
  | TInteger U128 -> "u128"
  | TFloat F16 -> "f16"
  | TFloat F32 -> "f32"
  | TFloat F64 -> "f64"
  | TFloat F128 -> "f128"
  | TChar -> "char"
  | TBool -> "bool"
