open Ppxlib
open Ast_builder.Default

module Extension_name = struct
  type t = Sat

  let to_string = function Sat -> "sat"
end

let symex_syntax = Lident "Symex_syntax"
let if_then_else = Ldot (symex_syntax, "branch_on")
let if_fexpr loc = pexp_ident ~loc (Located.mk ~loc if_then_else)

let expand_if ~loc expr then_ else_ =
  let fexpr = if_fexpr loc in
  pexp_apply ~loc fexpr
    [ (Nolabel, expr); (Labelled "then_", then_); (Labelled "else_", else_) ]

let expand ~ext expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let expansion =
    match expr.pexp_desc with
    | Pexp_ifthenelse (expr, then_, else_) ->
        let else_ =
          match else_ with
          | Some else_ -> else_
          | None ->
              Location.raise_errorf ~loc "'if%%%s' must include an else branch"
                (Extension_name.to_string ext)
        in
        expand_if ~loc expr then_ else_
    | _ -> Location.raise_errorf ~loc "%%sat can only be used with 'if'"
  in
  {
    expansion with
    pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes;
  }
