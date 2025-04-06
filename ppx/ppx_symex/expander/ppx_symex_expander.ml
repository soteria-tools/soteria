open Ppxlib

module If_sat = struct
  module Extension_name = struct
    type t = Sat | Sat1

    let to_string = function Sat -> "sat" | Sat1 -> "sat1"
  end

  let associated_fn ~loc = function
    | Extension_name.Sat -> [%expr Symex_syntax.branch_on]
    | Sat1 -> [%expr Symex_syntax.branch_on_take_one]

  let expand_if ~loc ~ext expr then_ else_ =
    let associated_fn = associated_fn ~loc ext in
    [%expr
      [%e associated_fn] [%e expr]
        ~then_:(fun () -> [%e then_])
        ~else_:(fun () -> [%e else_])]

  let expand ~ext expr =
    let loc = { expr.pexp_loc with loc_ghost = true } in
    let expansion =
      match expr with
      | [%expr if [%e? expr] then [%e? then_] else [%e? else_]] ->
          expand_if ~loc ~ext expr then_ else_
      | [%expr if [%e? _] then [%e? _]] ->
          Location.raise_errorf ~loc "'if%%%s' must include an else branch"
            (Extension_name.to_string ext)
      | _ -> Location.raise_errorf ~loc "%%sat can only be used with 'if'"
    in
    {
      expansion with
      pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes;
    }
end

module Sym_int_constants = struct
  let rewriter loc s =
    let i = Ast_builder.Default.eint ~loc (int_of_string s) in
    [%expr Sym_int_syntax.mk_int [%e i]]

  let suffix = 's'
end

module Sym_float_constants = struct
  let rewriter loc s =
    let f = Ast_builder.Default.efloat ~loc s in
    [%expr Sym_float_syntax.mk_float [%e f]]

  let suffix = 's'
end
