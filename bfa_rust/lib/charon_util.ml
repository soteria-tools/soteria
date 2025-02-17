open Charon
open Typed

let value_of_constant : Expressions.constant_expr -> T.cval Typed.t Rustsymex.t
    = function
  | { value = CLiteral (VScalar { value = v; _ }); _ } ->
      (* TODO: add constraints depending on the integer type *)
      Rustsymex.return (int_z v)
  | e ->
      Fmt.kstr Rustsymex.not_impl "TODO: value_of_constant %a"
        Expressions.pp_constant_expr e
