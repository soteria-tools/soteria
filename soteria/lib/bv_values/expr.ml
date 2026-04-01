open Soteria_std
open Svalue

type t = Svalue.t [@@deriving show { with_path = false }]
type ty = Svalue.ty [@@deriving show { with_path = false }]

let ty (s : t) : ty = s.node.ty
let[@inline] of_value v = v

module Subst = struct
  module Raw_map = PatriciaTree.MakeMap (struct
    type t = Svalue.t [@@deriving show]

    let to_int = Svalue.unique_tag
  end)

  type t = Svalue.t Raw_map.t

  let extend s v subst = Raw_map.add_assert_new s v subst
  let pp = Raw_map.pp Svalue.pp
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
            (Eval.eval_unop unop v1, s)
        | Binop (binop, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Eval.eval_binop binop v1 v2, s)
        | Ite (cond, v1, v2) ->
            let cond, s = apply ~missing_var s cond in
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Svalue.Bool.ite cond v1 v2, s)
        | Nop (nop, vs) ->
            let vs, s = apply_list ~missing_var s vs in
            (Eval.eval_nop nop vs, s))

  and apply_list ~missing_var s vs =
    match vs with
    | [] -> ([], s)
    | v :: vs ->
        let v, s = apply ~missing_var s v in
        let vs, s = apply_list ~missing_var s vs in
        (v :: vs, s)

  let rec learn (s : t) (e : Svalue.t) (v : Svalue.t) : t option =
    match e.node.kind with
    | Var _ -> if Raw_map.mem e s then Some s else Some (extend e v s)
    | Unop (Unop.Not, e') -> learn s e' (Bool.not v)
    (* This pattern matching can be extended for any reversible operation. *)
    | _ -> None
end

let subst (f : t -> t) (s : t) : t = f s
