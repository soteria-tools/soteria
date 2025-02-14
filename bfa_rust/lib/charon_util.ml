open Charon
open Typed

let type_operand : Expressions.operand -> Types.ty = function
  | Constant { ty; _ } | Copy { ty; _ } | Move { ty; _ } -> ty

let type_of : Expressions.rvalue -> Types.ty = function
  | BinaryOp ((Eq | Lt | Le | Ne | Ge | Gt), _, _) -> Types.TLiteral TBool
  | BinaryOp
      ((BitXor | BitAnd | BitOr | Div | Rem | Add | Sub | Mul | Shl | Shr), e, _)
    ->
      type_operand e
  | UnaryOp (Not, _) -> Types.TLiteral TBool
  | UnaryOp (Neg, e) -> type_operand e
  | Use e -> type_operand e
  | v -> Fmt.failwith "TODO: type_of %a" Expressions.pp_rvalue v

let value_of_constant : Expressions.constant_expr -> T.cval Typed.t Rustsymex.t
    = function
  | { value = CLiteral (VScalar { value = v; int_ty = I64 }); _ } ->
      Rustsymex.return (int_z v)
  | e ->
      Fmt.kstr Rustsymex.not_impl "TODO: value_of_constant %a"
        Expressions.pp_constant_expr e
