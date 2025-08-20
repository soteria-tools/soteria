open Soteria_c_values
open Soteria_symex;;

let pp_stats _pp_inner ft (_t: 'a Soteria_stats.stats) =
  Fmt.pf ft "...";;

#install_printer pp_stats;;
#install_printer pp_run_results;;
#install_printer Soteria_c_values.Typed.ppa;;
#require "soteria.ppx";;


module Ast = struct
  module Const = struct
    type t = Int of Z.t | Bool of bool

    let pp ft = function
      | Int i -> Fmt.pf ft "%a" Z.pp_print i
      | Bool b -> Fmt.pf ft "%b" b
  end

  module Binop = struct
    type t = Add | And | Div | Eq | Geq

    let to_str = function
      | Add -> "+"
      | And -> "&&"
      | Div -> "/"
      | Eq -> "=="
      | Geq -> ">="

    let pp ft op = Fmt.pf ft "%s" (to_str op)
  end

  module Expr = struct
    type t =
      | Const of Const.t
      | Var of string
      | BinOp of Binop.t * t * t
      | Let of string * t * t
      | If of t * t * t
      | NondetInt
      | Assert of t

    let rec pp ft t =
      match t with
      | Const c -> Const.pp ft c
      | Var v -> Fmt.pf ft "%s" v
      | Let (v, e1, e2) -> Fmt.pf ft "@[<v 2>let %s = %a in@ %a@]" v pp e1 pp e2
      | If (cond, then_branch, else_branch) ->
          Fmt.pf ft "@[<v 0>if %a@ then %a@ else %a@]" pp cond pp then_branch pp
            else_branch
      | NondetInt -> Fmt.pf ft "nondet_int"
      | Assert e -> Fmt.pf ft "assert %a" pp e
      | BinOp (op, e1, e2) -> Fmt.pf ft "(%a %a %a)" pp e1 Binop.pp op pp e2
  end
end
