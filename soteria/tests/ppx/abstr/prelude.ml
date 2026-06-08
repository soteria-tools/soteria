module Typed = struct
  include Soteria.Tiny_values.Typed

  (* The bv_values [Typed] exposes [Bool.and_]; the tiny one only has a
     top-level [and_], so we provide the submodule the deriver expects. *)
  module Bool = struct
    let and_ = and_
  end
end

module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)

(* A module implementing [Data.Abstr.M(Symex).S_with_syn] (plus [sem_eq] and
   [simplify]), used to exercise the "module field" case of the deriver. *)
module S_int = struct
  type t = Typed.T.sint Typed.t
  type syn = Typed.Expr.t

  let pp = Typed.ppa
  let show x = Format.asprintf "%a" pp x
  let pp_syn = Symex.Value.Expr.pp
  let show_syn x = Format.asprintf "%a" pp_syn x
  let fresh () = Symex.nondet Typed.t_int
  let to_syn (x : t) : syn = Symex.Value.Expr.of_value x
  let subst sub (x : syn) : t = Symex.Value.Expr.subst sub x
  let learn_eq (s : syn) (t : t) = Symex.Consumer.learn_eq s t
  let exprs_syn (x : syn) : Symex.Value.Expr.t list = [ x ]
  let sem_eq (a : t) (b : t) = Typed.sem_eq_untyped a b
  let simplify = Symex.simplify
end
