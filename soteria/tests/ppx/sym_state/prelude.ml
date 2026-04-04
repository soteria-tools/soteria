module Typed = Soteria.Tiny_values.Typed
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)

module S_int = struct
  include Typed

  type t = Typed.T.sint Typed.t
  type syn = Typed.Expr.t

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

module Excl_int = Soteria.Sym_states.Excl.Make (Symex) (S_int)
module Heap = Soteria.Sym_states.Pmap.Make (Symex) (S_int) (Excl_int)

module S_int_in_int = struct
  include Typed

  type t = Typed.T.sint Typed.t
  type syn = Typed.Expr.t

  let simplify = Excl_int.SM.simplify
  let fresh () = Excl_int.SM.nondet Typed.t_int
  let pp = Typed.ppa
  let show x = (Fmt.to_to_string pp) x
  let pp_syn = Excl_int.SM.Value.Expr.pp
  let show_syn x = (Fmt.to_to_string pp_syn) x
  let learn_eq (s : syn) (t : t) = Excl_int.SM.Consumer.learn_eq s t
  let to_syn (x : t) = Excl_int.SM.Value.Expr.of_value x
  let exprs_syn (x : syn) = [ x ]
  let subst = Excl_int.SM.Value.Expr.subst
end

module Excl_int_in_int =
  Soteria.Sym_states.Excl.Make (Excl_int.SM) (S_int_in_int)
