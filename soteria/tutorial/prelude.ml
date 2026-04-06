open Soteria;;
open Tiny_values;;

let pp_stats _pp_inner ft (_t: Soteria.Stats.t) =
  Fmt.pf ft "...";;

let pp_with_stats pp_res ft ({ res; stats = _ }: 'a Soteria.Stats.with_stats) =
  Fmt.pf ft "{res = %a; stats = <stats>}" pp_res res;;

let pp_hstring pp_v ft m =
  let pp_pair = Fmt.(pair ~sep:(any " -> ") Fmt.string pp_v) in
  Fmt.pf ft "@.  @[<v>%a@]"
    (Fmt.iter_bindings  Soteria_std.Hashtbl.Hstring.iter pp_pair) m;;


#install_printer pp_stats;;
#install_printer pp_with_stats;;
#install_printer pp_run_results;;
#install_printer Soteria.Tiny_values.Typed.ppa;;
#install_printer Soteria.Soteria_std.Dynarray.pp;;
#install_printer pp_hstring;;
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

module Dummy_state = struct
  type t = unit
  type syn = unit

  module SM = Soteria.Sym_states.State_monad.Make (My_symex) (struct
    type nonrec t = t option
  end)

  let pp _ _ = ()
  let pp_syn _ _ = ()
  let to_syn _ = []
  let ins_outs _ = [], []
  let produce _ s = My_symex.Producer.return (s)
  let consume _ s = My_symex.Consumer.ok (s)
end
module Heap = Dummy_state
module Globs = Dummy_state
module FunBiMap = struct
  type t = unit
  let empty = ()
  let is_empty () = true
  let pp _ _ = ()
end
module DecayedPointers = Dummy_state
module FancyHeap = struct
  type t = unit
  type syn = unit

  module SM = Soteria.Sym_states.State_monad.Make (DecayedPointers.SM) (struct
    type nonrec t = t option
  end)

  let pp _ _ = ()
  let pp_syn _ _ = ()
  let to_syn _ = []
  let ins_outs _ = [], []
  let produce _ s = DecayedPointers.SM.Producer.return (s)
  let consume _ s = DecayedPointers.SM.Consumer.ok (s)

  let load _ _ = SM.Result.ok ()
end
