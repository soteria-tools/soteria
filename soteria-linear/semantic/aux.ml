open Soteria_linear_ast.Lang
module Typed = Soteria.Tiny_values.Typed
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Data = Soteria.Data.M (Symex)
module Logic = Soteria.Logic.Make (Symex)

module Error = struct
  type t = [ `Interp of string | `UseAfterFree | Symex.cons_fail ]
  [@@deriving show { with_path = false }]
end

module S_int = struct
  include Typed

  type t = Typed.T.sint Typed.t
  type syn = Symex.Value.Expr.t

  let simplify = Symex.simplify
  let fresh () = Symex.nondet Typed.t_int
  let pp = Typed.ppa
  let show x = (Fmt.to_to_string pp) x
  let pp_syn = Symex.Value.Expr.pp
  let show_syn x = (Fmt.to_to_string pp_syn) x
  let learn_eq (s : syn) (t : t) = Symex.Consumer.learn_eq s t
  let to_syn (x : t) = Symex.Value.Expr.of_value x
  let exprs_syn (x : syn) = [ x ]
  let subst = Symex.Value.Expr.subst
end

module S_val = struct
  include Typed

  type t = T.any Typed.t [@@deriving show { with_path = false }]
  type syn = Symex.Value.Expr.t

  let pp_syn = Symex.Value.Expr.pp
  let show_syn = Fmt.to_to_string pp_syn
  let to_syn : t -> syn = Expr.of_value
  let subst = Symex.Value.Expr.subst
  let learn_eq (s : syn) (t : t) = Symex.Consumer.learn_eq s t
  let exprs_syn (x : syn) = [ x ]
  let sem_eq = sem_eq_untyped

  let fresh () : t Symex.t =
    Symex.branches
      [
        (fun () -> Symex.nondet Typed.t_int);
        (fun () -> Symex.nondet Typed.t_bool);
      ]

  let check_nonzero (v : T.sint Typed.t) :
      (T.nonzero Typed.t, string, 'a) Symex.Result.t =
    let open Symex.Syntax in
    let open Typed.Infix in
    let open Typed.Syntax in
    if%sat v ==@ 0s then Symex.Result.error "ZeroException"
    else Symex.Result.ok (Typed.cast v)
end

type subst = S_val.t String_map.t [@@deriving show { with_path = false }]
