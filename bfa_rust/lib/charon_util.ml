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
