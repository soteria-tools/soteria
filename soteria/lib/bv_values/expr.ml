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

  let is_known (s : t) (e : Svalue.t) : bool =
    let exception Not_covered in
    try
      let _ = apply ~missing_var:(fun _ _ -> raise_notrace Not_covered) s e in
      true
    with Not_covered -> false

  let rec learn (s : t) (e : Svalue.t) (v : Svalue.t) : t option =
    let open Syntaxes.Option in
    let/ () = if is_known s e then Some s else None in
    match e.node.kind with
    | Var _ -> if Raw_map.mem e s then Some s else Some (extend e v s)
    (* Boolean negation: Not(e') = v => e' = Not(v) *)
    | Unop (Unop.Not, e') -> learn s e' (Bool.not v)
    (* Bitwise NOT is self-inverse: BvNot(e') = v => e' = BvNot(v) *)
    | Unop (Unop.BvNot, e') -> learn s e' (BitVec.not v)
    (* Arithmetic negation is self-inverse mod 2^n: Neg(e') = v => e' =
       Neg(v) *)
    | Unop (Unop.Neg, e') -> learn s e' (BitVec.neg v)
    (* Bit extension: BvExtend(false, by)(e') = v => e' = extract(lower bits of
       v). This is valid whether the extension is signed or unsigned. *)
    | Unop (Unop.BvExtend (_, _by), e') ->
        let size = size_of e'.node.ty in
        learn s e' (BitVec.extract 0 (size - 1) v)
    (* BvOfBool maps false->0x0 and true->0x1. Only concrete cases are handled;
       any other value has no valid pre-image. *)
    | Unop (Unop.BvOfBool _, e') -> (
        match v.node.kind with
        | BitVec z when Z.equal z Z.zero -> learn s e' Bool.v_false
        | BitVec z when Z.equal z Z.one -> learn s e' Bool.v_true
        | _ -> None)
    (* Addition: e1 + c = v => e1 = v - c (or symmetrically) *)
    | Binop (Binop.Add _, e1, e2) ->
        if is_known s e1 then learn s e2 (BitVec.sub v e1)
        else if is_known s e2 then learn s e1 (BitVec.sub v e2)
        else None
    (* Subtraction: e1 - c = v => e1 = v + c, c - e2 = v => e2 = c - v *)
    | Binop (Binop.Sub _, e1, e2) ->
        if is_known s e1 then learn s e2 (BitVec.sub e1 v)
        else if is_known s e2 then learn s e1 (BitVec.add v e2)
        else None
    (* XOR with a constant is self-inverse: e1 ^ c = v => e1 = v ^ c *)
    | Binop (Binop.BitXor, e1, e2) ->
        if is_known s e1 then learn s e2 (BitVec.xor v e1)
        else if is_known s e2 then learn s e1 (BitVec.xor v e2)
        else None
    (* Concatenation: e1 ++ e2 = v => split v into high and low parts *)
    | Binop (Binop.BvConcat, e1, e2) ->
        let size_e2 = size_of e2.node.ty in
        let size_e1 = size_of e1.node.ty in
        let v_e2 = BitVec.extract 0 (size_e2 - 1) v in
        let v_e1 = BitVec.extract size_e2 (size_e2 + size_e1 - 1) v in
        let* s = learn s e1 v_e1 in
        learn s e2 v_e2
    (* Pointer: Ptr(loc_e, ofs_e) = Ptr(loc_v, ofs_v) => learn each component *)
    | Ptr (loc_e, ofs_e) ->
        let* s = learn s loc_e (Ptr.loc v) in
        learn s ofs_e (Ptr.ofs v)
    (* All other operations are not (safely) invertible. *)
    | _ -> None
end

let subst (f : t -> t) (s : t) : t = f s
