(** Non-simplifying ("direct") constructors for Svalue expressions.

    Each function here corresponds 1:1 to a smart constructor in
    {!Soteria.Bv_values.Svalue}, but builds the raw AST node without any
    pattern-matching simplification. This serves as the semantic ground truth
    when fuzz-testing that simplifications are correct. *)

open Soteria.Bv_values.Svalue

(* ------------------------------------------------------------------ *)
(* Boolean operations                                                  *)
(* ------------------------------------------------------------------ *)

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
