open Svalue

let[@inline] of_value v = v

module Subst = struct
  module Raw_map = Map.Make (Svalue)

  type t = Svalue.t Raw_map.t

  let add s v subst = Raw_map.add s v subst
  let find_opt s subst = Raw_map.find_opt s subst
end

let rec subst ~fresh (s : Subst.t) (v : Svalue.t) =
  match Subst.find_opt v s with
  | Some v -> (v, s)
  | None -> (
      match v.node.kind with
      | Var _ ->
          let v' = fresh v.node.ty in
          let s = Subst.add v v' s in
          (v', s)
      | Bool _ | Float _ | BitVec _ -> (v, s)
      | Seq elements ->
          let elements, s = subst_list ~fresh s elements in
          (Svalue.SSeq.mk ~seq_ty:v.node.ty elements, s)
      | Ptr (loc, ofs) ->
          let loc, s = subst ~fresh s loc in
          let ofs, s = subst ~fresh s ofs in
          (Ptr.mk loc ofs, s)
      | Unop (unop, v1) ->
          let v1, s = subst ~fresh s v1 in
          (Svalue.mk_unop unop v1, s)
      | Binop (binop, v1, v2) ->
          let v1, s = subst ~fresh s v1 in
          let v2, s = subst ~fresh s v2 in
          (Svalue.mk_binop binop v1 v2, s)
      | Ite (cond, v1, v2) ->
          let cond, s = subst ~fresh s cond in
          let v1, s = subst ~fresh s v1 in
          let v2, s = subst ~fresh s v2 in
          (Svalue.Bool.ite cond v1 v2, s)
      | Nop (nop, vs) ->
          let vs, s = subst_list ~fresh s vs in
          (Svalue.mk_nop nop vs, s))

and subst_list ~fresh s vs =
  match vs with
  | [] -> ([], s)
  | v :: vs ->
      let v, s = subst ~fresh s v in
      let vs, s = subst_list ~fresh s vs in
      (v :: vs, s)
