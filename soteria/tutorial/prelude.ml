open Soteria;;
open Tiny_values;;

let pp_stats _pp_inner ft (_t: Soteria.Stats.t) =
  Fmt.pf ft "...";;

#install_printer pp_stats;;
#install_printer pp_run_results;;
#install_printer Soteria.Tiny_values.Typed.ppa;;
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

(* for the PPX sym_states tutorial: *)

module My_symex = Symex.Make (Tiny_solver.Z3_solver)

module Heap = struct
  type t = unit
  type serialized = unit

  module SM = Soteria.Sym_states.State_monad.Make (My_symex) (struct
    type nonrec t = t option
  end)

  let pp _ _ = ()
  let pp_serialized _ _ = ()
  let serialize _ = []
  let subst_serialized _ x = x
  let iter_vars_serialized _ _ = ()
  let produce (_ : serialized) (st : t option) : (unit * t option) My_symex.t =
    My_symex.return ((), st)
end

module Globs = Heap
module FunBiMap = struct
  type t = unit
  let empty = ()
end

module DecayedPointers = struct
  type t = unit
  let empty = ()
end

module PointerMonad =
  Soteria.Sym_states.State_monad.Make (My_symex) (DecayedPointers)

module FancyHeap = struct
  type t = unit
  type serialized = unit

  module SM = Soteria.Sym_states.State_monad.Make (PointerMonad) (struct
    type nonrec t = t option
  end)

  let pp _ _ = ()
  let pp_serialized _ _ = ()
  let serialize _ = []
  let subst_serialized _ x = x
  let iter_vars_serialized _ _ = ()

  let produce (_ : serialized) (heap : t option) :
      (unit * t option) PointerMonad.t =
   fun pointers -> My_symex.return (((), heap), pointers)

  let load _ _ = SM.Result.ok ()
end
