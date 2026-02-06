open Svalue

type t = Svalue.t [@@deriving show { with_path = false }]
type ty = Svalue.ty [@@deriving show { with_path = false }]

let ty (s : t) : ty = s.node.ty
let[@inline] of_value v = v

module Subst = struct
  module Raw_map = Map.Make (Svalue)

  type t = Svalue.t Raw_map.t

  let empty = Raw_map.empty

  let rec apply ~missing_var (s : t) (v : Svalue.t) =
    match Raw_map.find_opt v s with
    | Some v -> (v, s)
    | None -> (
        match v.node.kind with
        | Var x ->
            let v' = missing_var x v.node.ty in
            let s = Raw_map.add v v' s in
            (v', s)
        | Bool _ | Float _ | BitVec _ -> (v, s)
        | Seq elements ->
            let elements, s = apply_list ~missing_var s elements in
            (Svalue.SSeq.mk ~seq_ty:v.node.ty elements, s)
        | Ptr (loc, ofs) ->
            let loc, s = apply ~missing_var s loc in
            let ofs, s = apply ~missing_var s ofs in
            (Ptr.mk loc ofs, s)
        | Unop (unop, v1) ->
            let v1, s = apply ~missing_var s v1 in
            (Svalue.mk_unop unop v1, s)
        | Binop (binop, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Svalue.mk_binop binop v1 v2, s)
        | Ite (cond, v1, v2) ->
            let cond, s = apply ~missing_var s cond in
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Svalue.Bool.ite cond v1 v2, s)
        | Nop (nop, vs) ->
            let vs, s = apply_list ~missing_var s vs in
            (Svalue.mk_nop nop vs, s))

  and apply_list ~missing_var s vs =
    match vs with
    | [] -> ([], s)
    | v :: vs ->
        let v, s = apply ~missing_var s v in
        let vs, s = apply_list ~missing_var s vs in
        (v :: vs, s)

  let learn (_s : t) (_syn : Svalue.t) (_v : Svalue.t) : t option =
    failwith "TODO: bv_values.syn.learn"
end
