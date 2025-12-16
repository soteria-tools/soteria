open Soteria_rust_lib
open Rustsymex.Syntax
module M = Map.Make (Svalue.Var)

type 'a t = 'a M.t

let empty = M.empty
let find_opt = M.find_opt

let add_vars subst iter_vars : Svalue.Var.t t Rustsymex.t =
  Rustsymex.fold_iter iter_vars ~init:subst ~f:(fun subst (var, ty) ->
      if M.mem var subst then Rustsymex.return subst
      else
        let+ var' = Rustsymex.fresh_var ty in
        M.add var var' subst)

let to_fn subst x = M.find x subst
