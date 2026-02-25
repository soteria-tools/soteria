(** Non-simplifying ("direct") constructors for Svalue expressions.

    Each function here corresponds 1:1 to a smart constructor in
    {!Soteria.Bv_values.Svalue}, but builds the raw AST node without any
    pattern-matching simplification. This serves as the semantic ground truth
    when fuzz-testing that simplifications are correct. *)

open Soteria.Bv_values.Svalue

(* ------------------------------------------------------------------ *)
(* Boolean operations                                                  *)
(* ------------------------------------------------------------------ *)

module Var_pool = struct
  let max_var_per_ty = 5

  let get_next_name =
    let name_counter = ref 0 in
    fun () ->
      let res = !name_counter in
      incr name_counter;
      Var.of_int res

  (* We want to generate at most [max_var_per_ty] variables per type. This
     avoids generating expressions with only different variables, which cannot
     be reduced interestingly. *)
  let var_pool : (int * ty, t) Hashtbl.t = Hashtbl.create 1024

  let get_from_pool idx ty =
    let key = (idx, ty) in
    match Hashtbl.find_opt var_pool key with
    | None ->
        let name = get_next_name () in
        let v = mk_var name ty in
        Hashtbl.replace var_pool key v;
        v
    | Some v -> v
end

module Bool = struct
  let v_true = Bool true <| TBool
  let v_false = Bool false <| TBool
  let bool b = if b then v_true else v_false
  let not_ v = Unop (Not, v) <| TBool
  let and_ v1 v2 = Binop (And, v1, v2) <| TBool
  let or_ v1 v2 = Binop (Or, v1, v2) <| TBool
  let ite c t e = Ite (c, t, e) <| t.Hc.node.ty

  let eq v1 v2 =
    (* Eq always returns TBool *)
    Binop (Eq, v1, v2) <| TBool

  let distinct vs = Nop (Distinct, vs) <| TBool
end

(* ------------------------------------------------------------------ *)
(* BitVector operations                                                *)
(* ------------------------------------------------------------------ *)

module BitVec = struct
  (* Constructors *)
  let mk n z = BitVec z <| t_bv n
  let zero n = BitVec Z.zero <| t_bv n
  let one n = BitVec Z.one <| t_bv n

  (* Arithmetic *)
  let add ?(checked = false) v1 v2 =
    Binop (Add { checked }, v1, v2) <| v1.Hc.node.ty

  let sub ?(checked = false) v1 v2 =
    Binop (Sub { checked }, v1, v2) <| v1.Hc.node.ty

  let mul ?(checked = false) v1 v2 =
    Binop (Mul { checked }, v1, v2) <| v1.Hc.node.ty

  let div ~signed v1 v2 = Binop (Div signed, v1, v2) <| v1.Hc.node.ty
  let rem ~signed v1 v2 = Binop (Rem signed, v1, v2) <| v1.Hc.node.ty
  let mod_ v1 v2 = Binop (Mod, v1, v2) <| v1.Hc.node.ty
  let neg v = Unop (Neg, v) <| v.Hc.node.ty

  (* Bitwise *)
  let not_ v = Unop (BvNot, v) <| v.Hc.node.ty
  let and_ v1 v2 = Binop (BitAnd, v1, v2) <| v1.Hc.node.ty
  let or_ v1 v2 = Binop (BitOr, v1, v2) <| v1.Hc.node.ty
  let xor v1 v2 = Binop (BitXor, v1, v2) <| v1.Hc.node.ty
  let shl v1 v2 = Binop (Shl, v1, v2) <| v1.Hc.node.ty
  let lshr v1 v2 = Binop (LShr, v1, v2) <| v1.Hc.node.ty
  let ashr v1 v2 = Binop (AShr, v1, v2) <| v1.Hc.node.ty

  (* Bitvector manipulation *)
  let concat v1 v2 =
    let n1 = size_of v1.Hc.node.ty in
    let n2 = size_of v2.Hc.node.ty in
    Binop (BvConcat, v1, v2) <| t_bv (n1 + n2)

  let extract from_ to_ v =
    Unop (BvExtract (from_, to_), v) <| t_bv (to_ - from_ + 1)

  let extend ~signed by v =
    let n = size_of v.Hc.node.ty in
    Unop (BvExtend (signed, by), v) <| t_bv (n + by)

  (* Comparisons *)
  let lt ~signed v1 v2 = Binop (Lt signed, v1, v2) <| TBool
  let leq ~signed v1 v2 = Binop (Leq signed, v1, v2) <| TBool
  let gt ~signed v1 v2 = lt ~signed v2 v1
  let geq ~signed v1 v2 = leq ~signed v2 v1

  (* Overflow checks *)
  let add_overflows ~signed v1 v2 = Binop (AddOvf signed, v1, v2) <| TBool
  let mul_overflows ~signed v1 v2 = Binop (MulOvf signed, v1, v2) <| TBool

  let neg_overflows (v : t) =
    let n = size_of v.node.ty in
    let min = Z.(neg (one lsl Stdlib.( - ) n 1)) in
    Binop (Binop.Eq, v, BitVec.mk_masked n min) <| TBool

  let sub_overflows ~signed v1 v2 =
    if Stdlib.not signed then lt ~signed v1 v2
    else
      let neg_ovf = neg_overflows v2 in
      let neg_v2 = neg v2 in
      let add_ovf = add_overflows ~signed v1 neg_v2 in
      Bool.or_ neg_ovf add_ovf

  (* Bool-bv conversions *)
  let of_bool n v = Unop (BvOfBool n, v) <| t_bv n

  let to_bool v =
    (* not(v == 0) *)
    let n = size_of v.Hc.node.ty in
    Bool.not_ (Bool.eq v (zero n))

  let not_bool v =
    (* v == 0 *)
    let n = size_of v.Hc.node.ty in
    Bool.eq v (zero n)

  (* Float-bv conversions *)
  let of_float ~rounding ~signed ~size v =
    Unop (BvOfFloat (rounding, signed, size), v) <| t_bv size

  let to_float ~rounding ~signed ~fp v =
    Unop (FloatOfBv (rounding, signed, fp), v) <| TFloat fp

  let to_float_raw v =
    let n = size_of v.Hc.node.ty in
    let fp = FloatPrecision.of_size n in
    Unop (FloatOfBvRaw fp, v) <| TFloat fp
end

let collect_checked_assumptions (v : t) : t list =
  let assumptions = Dynarray.create () in

  let rec go v =
    match v.Hc.node.kind with
    | Unop (_, v) -> go v
    | Ptr (a, b) ->
        go a;
        go b
    | Binop (Add { checked = true }, v1, v2) ->
        Dynarray.append_list assumptions
          [
            Bool.not_ (BitVec.add_overflows ~signed:false v1 v2);
            Bool.not_ (BitVec.add_overflows ~signed:true v1 v2);
          ];
        go v1;
        go v2
    | Binop (Sub { checked = true }, v1, v2) ->
        Dynarray.append_list assumptions
          [
            Bool.not_ (BitVec.sub_overflows ~signed:false v1 v2);
            Bool.not_ (BitVec.sub_overflows ~signed:true v1 v2);
          ];
        go v1;
        go v2
    | Binop (Mul { checked = true }, v1, v2) ->
        Dynarray.append_list assumptions
          [
            Bool.not_ (BitVec.mul_overflows ~signed:false v1 v2);
            Bool.not_ (BitVec.mul_overflows ~signed:true v1 v2);
          ];
        go v1;
        go v2
    | Binop (_, v1, v2) ->
        go v1;
        go v2
    | Ite (c, t, e) ->
        go c;
        go t;
        go e
    | Seq vs | Nop (_, vs) -> List.iter go vs
    | Var _ | Bool _ | BitVec _ | Float _ -> ()
  in
  go v;
  Dynarray.to_list assumptions |> List.sort_uniq compare

module Shrink = struct
  let ( @ ) = Seq.append
  let ( ++ ) = Seq.cons

  let small_of_ty ty =
    match ty with
    | TBool -> Seq.cons Bool.v_false (Seq.singleton Bool.v_true)
    | TBitVector n -> Seq.cons (BitVec.zero n) (Seq.singleton (BitVec.one n))
    | _ -> Seq.empty

  let just_var ty = Var_pool.get_from_pool 0 ty

  let rec children_of_ty ~ty v =
    let return v = v ++ shrink v in
    let if_ty v =
      if equal_ty ty v.Hc.node.ty then return v else children_of_ty ~ty v
    in
    match v.Hc.node.kind with
    | Unop (_, v) -> if_ty v
    | Binop (_, v1, v2) ->
        let left = if_ty v1 in
        let right = if_ty v2 in
        left @ right
    | Ite (c, t, e) ->
        if equal c Bool.v_true then return t
        else if equal c Bool.v_false then return e
        else return t @ return e
    | _ -> Seq.empty

  and shrink v =
    match v.Hc.node.kind with
    | Var _ -> small_of_ty v.Hc.node.ty
    | Bool _ -> Seq.empty
    | BitVec n ->
        let size = size_of v.Hc.node.ty in
        if Z.gt n Z.one then List.to_seq [ BitVec.zero size; BitVec.one size ]
        else if Z.gt n Z.zero then Seq.singleton (BitVec.zero size)
        else Seq.empty
    | _ ->
        let ty = v.Hc.node.ty in
        (just_var ty ++ small_of_ty ty) @ children_of_ty ~ty:v.Hc.node.ty v
end
