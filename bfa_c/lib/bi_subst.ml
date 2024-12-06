(** Bi-directional substitution where there is a 1-1 correspondance between variables. *)

module Var = Svalue.Var
module Subst = Map.Make (Var)
module SubstMut = Hashtbl.Make (Var)
open Csymex.Syntax

type t = {
  forward : Var.t Subst.t;
  backward : Var.t SubstMut.t;
  mutable next_backward : int;
}

let empty () =
  { forward = Subst.empty; backward = SubstMut.create 0; next_backward = 0 }

let create iter_vars =
  Csymex.fold_iter iter_vars ~init:(empty ()) ~f:(fun bi_subst (var, ty) ->
      if Subst.mem var bi_subst.forward then Csymex.return bi_subst
      else
        let+ var' = Csymex.fresh_var ty in
        let forward = Subst.add var var' bi_subst.forward in
        SubstMut.replace bi_subst.backward var' var;
        let next_backward = max bi_subst.next_backward (var + 1) in
        { forward; backward = bi_subst.backward; next_backward })

let is_empty bi_subst = Subst.is_empty bi_subst.forward
let forward bi_subst v_id = Subst.find v_id bi_subst.forward

let backward bi_subst v_id =
  match SubstMut.find_opt bi_subst.backward v_id with
  | Some v -> v
  | None ->
      let v = bi_subst.next_backward in
      bi_subst.next_backward <- v + 1;
      SubstMut.add bi_subst.backward v_id v;
      v
