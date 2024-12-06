(** Bi-directional substitution where there is a 1-1 correspondance between variables. *)

module Var = Svalue.Var
module Subst = Map.Make (Var)
open Csymex.Syntax

type t = { forward : Var.t Subst.t; backward : Var.t Subst.t }

let empty = { forward = Subst.empty; backward = Subst.empty }

let create iter_vars =
  Csymex.fold_iter iter_vars ~init:empty ~f:(fun bi_subst (var, ty) ->
      if Subst.mem var bi_subst.forward then Csymex.return bi_subst
      else
        let+ var' = Csymex.fresh_var ty in
        let forward = Subst.add var var' bi_subst.forward in
        let backward = Subst.add var' var bi_subst.backward in
        { forward; backward })

let is_empty bi_subst = Subst.is_empty bi_subst.forward
let forward bi_subst v_id = Subst.find v_id bi_subst.forward

let backward bi_subst =
  (* The backward function will create new bindings when necessary.
     The function should be instantiated only once, since it is mutable. *)
  let next =
    let next =
      match Subst.max_binding_opt bi_subst.forward with
      | None -> 0
      | Some (i, _) -> i + 1
    in
    ref next
  in
  let current_back_subst = ref bi_subst.backward in
  fun v_id ->
    match Subst.find_opt v_id !current_back_subst with
    | Some v -> v
    | None ->
        let n = !next in
        current_back_subst := Subst.add v_id n !current_back_subst;
        n
