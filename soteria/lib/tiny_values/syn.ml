open Svalue
open Soteria_std

type t = Svalue.t [@@deriving show { with_path = false }]

let ty (s : t) : ty = s.node.ty
let[@inline] of_value v = v

module Subst = struct
  (* TODO: make this a Patricia Tree *)
  module Raw_map = Map.MakePp (Svalue)

  type t = Svalue.t Raw_map.t

  let extend s v subst = Raw_map.add_if_not_exists s v subst
  let find_opt s subst = Raw_map.find_opt s subst
  let empty = Raw_map.empty
end

let rec subst ~missing_var (s : Subst.t) (v : Svalue.t) =
  match Subst.find_opt v s with
  | Some v -> (v, s)
  | None -> (
      match v.node.kind with
      | Var x ->
          let v' = missing_var x v.node.ty in
          let s = Subst.extend v v' s in
          (v', s)
      | Bool _ | Int _ -> (v, s)
      | Unop (unop, v1) ->
          let v1, s = subst ~missing_var s v1 in
          (Svalue.mk_unop unop v1, s)
      | Binop (binop, v1, v2) ->
          let v1, s = subst ~missing_var s v1 in
          let v2, s = subst ~missing_var s v2 in
          (Svalue.mk_binop binop v1 v2, s)
      | Ite (cond, v1, v2) ->
          let cond, s = subst ~missing_var s cond in
          let v1, s = subst ~missing_var s v1 in
          let v2, s = subst ~missing_var s v2 in
          (Svalue.ite cond v1 v2, s)
      | Nop (nop, vs) ->
          let vs, s = subst_list ~missing_var s vs in
          (Svalue.mk_nop nop vs, s))

and subst_list ~missing_var s vs =
  match vs with
  | [] -> ([], s)
  | v :: vs ->
      let v, s = subst ~missing_var s v in
      let vs, s = subst_list ~missing_var s vs in
      (v :: vs, s)

let rec learn (s : Subst.t) (e : t) (v : t) : Subst.t option =
  match e.node.kind with
  | Var _ -> if Subst.Raw_map.mem e s then Some s else Some (Subst.extend e v s)
  | Unop (Unop.Not, e') -> learn s e' (not v)
  (* This pattern matching can be extended for any reversible operation. *)
  | _ -> None
