open Soteria_std
open Svalue

(* The syntactic representation and the substitution result are both an
   existentially-wrapped semantic value. *)
type t = Svalue.packed
type packed_v = Svalue.packed = Packed : 'a Svalue.t -> packed_v
type packed_ty = Svalue.packed_ty = PackedTy : 'a Svalue.ty -> packed_ty

let pp ft (Packed v) = Svalue.pp ft v
let show v = Fmt.to_to_string pp v
let[@inline] of_value v = Packed v
let ty (Packed v) = PackedTy v.node.ty

module Subst = struct
  module Raw_map = PatriciaTree.MakeMap (struct
    type t = Svalue.packed

    let to_int (Packed v) = Svalue.unique_tag v
    let pp ft (Packed v) = Svalue.pp ft v
  end)

  type t = Svalue.packed Raw_map.t

  let pp = Raw_map.pp (fun ft (Packed v) -> Svalue.pp ft v)
  let empty = Raw_map.empty

  type missing = { missing : 'a. Var.t -> 'a Svalue.ty -> 'a Svalue.t }

  (* Look up [v] in the substitution. The map preserves types (a value is bound
     to its substitution, which has the same type), so a hit is recovered at
     [v]'s type via a type-equality witness. *)
  let find_typed : type a. t -> a Svalue.t -> a Svalue.t option =
   fun s v ->
    match Raw_map.find_opt (Packed v) s with
    | Some (Packed v') -> (
        match eq_ty v'.node.ty v.node.ty with
        | Some Equal -> Some v'
        | None -> None)
    | None -> None

  let extend (Packed v) (Packed v') subst =
    Raw_map.add_assert_new (Packed v) (Packed v') subst

  let rec apply : type a.
      missing_var:missing -> t -> a Svalue.t -> a Svalue.t * t =
   fun ~missing_var s v ->
    match find_typed s v with
    | Some v -> (v, s)
    | None -> (
        match v.node.kind with
        | Var x ->
            let v' = missing_var.missing x v.node.ty in
            let s = Raw_map.add (Packed v) (Packed v') s in
            (v', s)
        | Bool _ | Float _ | BitVec _ | LocLit _ -> (v, s)
        | Seq elements ->
            let elements, s = apply_list ~missing_var s elements in
            (Svalue.SSeq.mk ~seq_ty:v.node.ty elements, s)
        | Ptr (loc, ofs) ->
            let loc, s = apply ~missing_var s loc in
            let ofs, s = apply ~missing_var s ofs in
            (Ptr.mk loc ofs, s)
        | UnBv (op, v1) ->
            let v1, s = apply ~missing_var s v1 in
            (Eval.eval_on_bv op v1, s)
        | UnBool (op, v1) ->
            let v1, s = apply ~missing_var s v1 in
            (Eval.eval_on_bool op v1, s)
        | UnFloat (op, v1) ->
            let v1, s = apply ~missing_var s v1 in
            (Eval.eval_on_float op v1, s)
        | UnPtr (op, v1) ->
            let v1, s = apply ~missing_var s v1 in
            (Eval.eval_on_ptr op v1, s)
        | BvArith (op, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Eval.eval_arith op v1 v2, s)
        | BvCmp (op, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Eval.eval_cmp op v1 v2, s)
        | FArith (op, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Eval.eval_farith op v1 v2, s)
        | FCmp (op, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Eval.eval_fcmp op v1 v2, s)
        | BoolBin (op, v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Eval.eval_boolean op v1 v2, s)
        | Eq (v1, v2) ->
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Svalue.Bool.sem_eq v1 v2, s)
        | Ite (cond, v1, v2) ->
            let cond, s = apply ~missing_var s cond in
            let v1, s = apply ~missing_var s v1 in
            let v2, s = apply ~missing_var s v2 in
            (Svalue.Bool.ite cond v1 v2, s)
        | Nop (Distinct, vs) ->
            let vs, s = apply_list ~missing_var s vs in
            (Svalue.Bool.distinct vs, s)
        | Exists (vs, sv) ->
            let (vs, sv), s = apply_bound ~missing_var s vs sv in
            (Svalue.Bool.mk_exists vs sv, s))

  and apply_list : type a.
      missing_var:missing -> t -> a Svalue.t list -> a Svalue.t list * t =
   fun ~missing_var s vs ->
    match vs with
    | [] -> ([], s)
    | v :: vs ->
        let v, s = apply ~missing_var s v in
        let vs, s = apply_list ~missing_var s vs in
        (v :: vs, s)

  and apply_bound ~missing_var s vs sv =
    (* [max_var_ind] returns the index of the freshest semantic variable used in
       [sv], which we use to create new fresh variables for the existential. *)
    let max_var_ind : type b. b Svalue.t -> int -> int =
     fun sv init ->
      Svalue.iter_vars sv
      |> IterLabels.fold ~init ~f:(fun curr (var, _) ->
          let var = Var.to_int var in
          if var > curr then var else curr)
    in
    let bound var = List.exists (fun (v', _) -> Var.equal var v') vs in
    let max_var_ind, s =
      Svalue.iter_vars sv
      |> Iter.filter (fun (var, _) -> not (bound var))
      |> IterLabels.fold ~init:(0, s) ~f:(fun (curr, s) (var, PackedTy ty) ->
          let syn_var = mk_var var ty in
          match Raw_map.find_opt (Packed syn_var) s with
          | Some (Packed sv) -> (max_var_ind sv curr, s)
          | None ->
              (* We call [missing_var] here to ensure that our fresh variables
                 do not clash with other unknown variables and then update [s]
                 so that [missing_var] is only called once. *)
              let sv = missing_var.missing var ty in
              (max_var_ind sv curr, Raw_map.add (Packed syn_var) (Packed sv) s))
    in
    (* [subst_with_fresh] is the new substitution which extends [s] by binding
       each variable in [vs] to a fresh semantic variable. [fresh_vs] is a list
       with all the freshly created variables. [old_bindings] is a list of pairs
       [(syn_var, sem_var_opt)] where [sem_var_opt] is an optional variable: if
       [sem_var_opt] holds a value [sem_var], then the binding [(syn_var,
       sem_var)] existed in the original substitution and must be reverted at
       the end. *)
    let _, subst_with_fresh, fresh_vs, old_bindings =
      ListLabels.fold_left vs
        ~init:(max_var_ind + 1, s, [], [])
        ~f:(fun (curr_var_ind, s, vs, bs) (var, PackedTy ty) ->
          let new_var = Var.of_int curr_var_ind in
          let syn_var = mk_var var ty and sem_var = mk_var new_var ty in
          let bs =
            (Packed syn_var, Raw_map.find_opt (Packed syn_var) s) :: bs
          in
          let s = Raw_map.add (Packed syn_var) (Packed sem_var) s in
          (curr_var_ind + 1, s, (new_var, PackedTy ty) :: vs, bs))
    in
    (* Actually perform substitution *)
    let sv, subst_with_fresh = apply ~missing_var subst_with_fresh sv in
    (* Revert the dummy bindings *)
    let subst_after_pass =
      ListLabels.fold_left old_bindings ~init:subst_with_fresh
        ~f:(fun s -> function
        | syn_var, None -> Raw_map.remove syn_var s
        | syn_var, Some sem_var -> Raw_map.add syn_var sem_var s)
    in
    ((fresh_vs, sv), subst_after_pass)

  let apply ~missing_var s (Packed e) =
    let v, s = apply ~missing_var s e in
    (Packed v, s)

  let is_known : type a. t -> a Svalue.t -> bool =
   fun s e ->
    let exception Not_covered in
    try
      let _ =
        apply
          ~missing_var:{ missing = (fun _ _ -> raise_notrace Not_covered) }
          s (Packed e)
      in
      true
    with Not_covered -> false

  let rec learn_typed : type a. t -> a Svalue.t -> a Svalue.t -> t option =
   fun s e v ->
    let open Syntaxes.Option in
    let/ () = if is_known s e then Some s else None in
    match e.node.kind with
    | Var _ ->
        if Raw_map.mem (Packed e) s then Some s
        else Some (extend (Packed e) (Packed v) s)
    (* Boolean negation: Not(e') = v => e' = Not(v) *)
    | UnBool (Not, e') -> learn_typed s e' (Bool.not v)
    (* Bitwise NOT is self-inverse: BvNot(e') = v => e' = BvNot(v) *)
    | UnBv (BvNot, e') -> learn_typed s e' (BitVec.not v)
    (* Arithmetic negation is self-inverse mod 2^n: Neg(e') = v => e' =
       Neg(v) *)
    | UnBv (Neg _, e') -> learn_typed s e' (BitVec.neg v)
    (* Bit extension: BvExtend(false, by)(e') = v => e' = extract(lower bits of
       v). This is valid whether the extension is signed or unsigned. *)
    | UnBv (BvExtend (_, _by), e') ->
        let size = size_of e'.node.ty in
        learn_typed s e' (BitVec.extract 0 (size - 1) v)
    (* BvOfBool maps false->0x0 and true->0x1. Only concrete cases are handled;
       any other value has no valid pre-image. *)
    | UnBool (BvOfBool _, e') -> (
        match v.node.kind with
        | BitVec z when Z.equal z Z.zero -> learn_typed s e' Bool.v_false
        | BitVec z when Z.equal z Z.one -> learn_typed s e' Bool.v_true
        | _ -> None)
    (* Addition: e1 + c = v => e1 = v - c (or symmetrically) *)
    | BvArith (Add _, e1, e2) ->
        if is_known s e1 then learn_typed s e2 (BitVec.sub v e1)
        else if is_known s e2 then learn_typed s e1 (BitVec.sub v e2)
        else None
    (* Subtraction: e1 - c = v => e1 = v + c, c - e2 = v => e2 = c - v *)
    | BvArith (Sub _, e1, e2) ->
        if is_known s e1 then learn_typed s e2 (BitVec.sub e1 v)
        else if is_known s e2 then learn_typed s e1 (BitVec.add v e2)
        else None
    (* XOR with a constant is self-inverse: e1 ^ c = v => e1 = v ^ c *)
    | BvArith (BitXor, e1, e2) ->
        if is_known s e1 then learn_typed s e2 (BitVec.xor v e1)
        else if is_known s e2 then learn_typed s e1 (BitVec.xor v e2)
        else None
    (* Concatenation: e1 ++ e2 = v => split v into high and low parts *)
    | BvArith (BvConcat, e1, e2) ->
        let size_e2 = size_of e2.node.ty in
        let size_e1 = size_of e1.node.ty in
        let v_e2 = BitVec.extract 0 (size_e2 - 1) v in
        let v_e1 = BitVec.extract size_e2 (size_e2 + size_e1 - 1) v in
        let* s = learn_typed s e1 v_e1 in
        learn_typed s e2 v_e2
    (* Pointer: Ptr(loc_e, ofs_e) = Ptr(loc_v, ofs_v) => learn each component *)
    | Ptr (loc_e, ofs_e) ->
        let* s = learn_typed s loc_e (Ptr.loc v) in
        learn_typed s ofs_e (Ptr.ofs v)
    (* All other operations are not (safely) invertible. *)
    | _ -> None

  let learn s (Packed e) v =
    (* re-pack [v] so both sides are existential: this lets the witness unify
       them without [v]'s (rigid) type escaping. *)
    let (Packed v) = (Svalue.Packed v : Svalue.packed) in
    match eq_ty e.node.ty v.node.ty with
    | Some Equal -> learn_typed s e v
    | None -> None
end

let subst (f : t -> packed_v) (s : t) : packed_v = f s
