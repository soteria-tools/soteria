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
  let var_pool : (int * packed_ty, packed) Hashtbl.t = Hashtbl.create 1024

  let get_from_pool : type a. int -> a ty -> a t =
   fun idx ty ->
    let key = (idx, PackedTy ty) in
    match Hashtbl.find_opt var_pool key with
    | None ->
        let name = get_next_name () in
        let v = mk_var name ty in
        Hashtbl.replace var_pool key (Packed v);
        v
    | Some (Packed v) -> (
        match eq_ty v.node.ty ty with Some Equal -> v | None -> assert false)
end

module Bool = struct
  let v_true = Bool true <| TBool
  let v_false = Bool false <| TBool
  let bool b = if b then v_true else v_false
  let not_ v = UnBool (Not, v) <| TBool
  let and_ v1 v2 = BoolBin (And, v1, v2) <| TBool
  let or_ v1 v2 = BoolBin (Or, v1, v2) <| TBool
  let ite c t e = Ite (c, t, e) <| t.node.ty

  let eq v1 v2 =
    (* Eq always returns TBool *)
    Eq (v1, v2) <| TBool

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
  let add ?(checked = unchecked) v1 v2 =
    BvArith (Add checked, v1, v2) <| v1.node.ty

  let sub ?(checked = unchecked) v1 v2 =
    BvArith (Sub checked, v1, v2) <| v1.node.ty

  let mul ?(checked = unchecked) v1 v2 =
    BvArith (Mul checked, v1, v2) <| v1.node.ty

  let div ~signed v1 v2 = BvArith (Div signed, v1, v2) <| v1.node.ty
  let rem ~signed v1 v2 = BvArith (Rem signed, v1, v2) <| v1.node.ty
  let mod_ v1 v2 = BvArith (Mod, v1, v2) <| v1.node.ty
  let neg ?(checked = false) v = UnBv (Neg checked, v) <| v.node.ty

  (* Bitwise *)
  let not_ v = UnBv (BvNot, v) <| v.node.ty
  let and_ v1 v2 = BvArith (BitAnd, v1, v2) <| v1.node.ty
  let or_ v1 v2 = BvArith (BitOr, v1, v2) <| v1.node.ty
  let xor v1 v2 = BvArith (BitXor, v1, v2) <| v1.node.ty
  let shl v1 v2 = BvArith (Shl, v1, v2) <| v1.node.ty
  let lshr v1 v2 = BvArith (LShr, v1, v2) <| v1.node.ty
  let ashr v1 v2 = BvArith (AShr, v1, v2) <| v1.node.ty

  (* Bitvector manipulation *)
  let concat v1 v2 =
    let n1 = size_of v1.node.ty in
    let n2 = size_of v2.node.ty in
    BvArith (BvConcat, v1, v2) <| t_bv (n1 + n2)

  let extract from_ to_ v =
    UnBv (BvExtract (from_, to_), v) <| t_bv (to_ - from_ + 1)

  let extend ~signed by v =
    let n = size_of v.node.ty in
    UnBv (BvExtend (signed, by), v) <| t_bv (n + by)

  (* Comparisons *)
  let lt ~signed v1 v2 = BvCmp (Lt signed, v1, v2) <| TBool
  let leq ~signed v1 v2 = BvCmp (Leq signed, v1, v2) <| TBool
  let gt ~signed v1 v2 = lt ~signed v2 v1
  let geq ~signed v1 v2 = leq ~signed v2 v1

  (* Overflow checks *)
  let add_overflows ~signed v1 v2 = BvCmp (AddOvf signed, v1, v2) <| TBool
  let sub_overflows ~signed v1 v2 = BvCmp (SubOvf signed, v1, v2) <| TBool
  let mul_overflows ~signed v1 v2 = BvCmp (MulOvf signed, v1, v2) <| TBool

  let neg_overflows (v : sbv t) =
    let n = size_of v.node.ty in
    let min = Z.(neg (one lsl Stdlib.( - ) n 1)) in
    Eq (v, BitVec.mk_masked n min) <| TBool

  (* Bool-bv conversions *)
  let of_bool n v = UnBool (BvOfBool n, v) <| t_bv n

  let to_bool v =
    (* not(v == 0) *)
    let n = size_of v.node.ty in
    Bool.not_ (Bool.eq v (zero n))

  let not_bool v =
    (* v == 0 *)
    let n = size_of v.node.ty in
    Bool.eq v (zero n)

  (* Float-bv conversions *)
  let of_float ~rounding ~signed ~size v =
    UnFloat (BvOfFloat (rounding, signed, size), v) <| t_bv size

  let to_float ~rounding ~signed ~fp v =
    UnBv (FloatOfBv (rounding, signed, fp), v) <| TFloat fp

  let to_float_raw v =
    let n = size_of v.node.ty in
    let fp = FloatPrecision.of_size n in
    UnBv (FloatOfBvRaw fp, v) <| TFloat fp
end

(* A checked operation only promises no overflow in the signedness(es) recorded
   by its [checked] flag, so we only assume those. *)
let checked_assumptions assumptions checked v1 v2 overflows =
  Iter.bools @@ fun signed ->
  if checked_has ~signed checked then
    Dynarray.add_last assumptions (Bool.not_ (overflows ~signed v1 v2))

let collect_checked_assumptions (v : 'a t) : sbool t list =
  let assumptions = Dynarray.create () in

  let rec go : type a. a t -> unit =
   fun v ->
    match v.node.kind with
    | UnBv (Neg true, v') ->
        Dynarray.add_last assumptions (Bool.not_ (BitVec.neg_overflows v'));
        go v'
    | UnBv (_, v) -> go v
    | UnBool (_, v) -> go v
    | UnFloat (_, v) -> go v
    | UnPtr (_, v) -> go v
    | Ptr (a, b) ->
        go a;
        go b
    | BvArith (Add checked, v1, v2) ->
        checked_assumptions assumptions checked v1 v2 BitVec.add_overflows;
        go v1;
        go v2
    | BvArith (Sub checked, v1, v2) ->
        checked_assumptions assumptions checked v1 v2 BitVec.sub_overflows;
        go v1;
        go v2
    | BvArith (Mul checked, v1, v2) ->
        checked_assumptions assumptions checked v1 v2 BitVec.mul_overflows;
        go v1;
        go v2
    | BvArith (_, v1, v2) ->
        go v1;
        go v2
    | BvCmp (_, v1, v2) ->
        go v1;
        go v2
    | FArith (_, v1, v2) ->
        go v1;
        go v2
    | FCmp (_, v1, v2) ->
        go v1;
        go v2
    | BoolBin (_, v1, v2) ->
        go v1;
        go v2
    | Eq (v1, v2) ->
        go v1;
        go v2
    | Ite (c, t, e) ->
        go c;
        go t;
        go e
    | Seq vs -> List.iter go vs
    | Nop (_, vs) -> List.iter go vs
    | Var _ | Bool _ | BitVec _ | Float _ | LocLit _ -> ()
    | Exists _ ->
        failwith
          "collect_checked_assumptions: not implemented for quantifiers (we \
           don't generate quantifiers for fuzzing yet)"
  in
  go v;
  Dynarray.to_list assumptions |> List.sort_uniq compare

module Shrink = struct
  let ( @ ) = Seq.append
  let ( ++ ) = Seq.cons

  let small_of_ty : type a. a ty -> a t Seq.t =
   fun ty ->
    match ty with
    | TBool -> Seq.cons Bool.v_false (Seq.singleton Bool.v_true)
    | TBitVector n -> Seq.cons (BitVec.zero n) (Seq.singleton (BitVec.one n))
    | _ -> Seq.empty

  let just_var ty = Var_pool.get_from_pool 0 ty

  let rec children_of_ty : type a b. ty:a ty -> b t -> a t Seq.t =
   fun ~ty v ->
    let if_ty : type c. c t -> a t Seq.t =
     fun v ->
      match eq_ty ty v.node.ty with
      | Some Equal -> v ++ shrink v
      | None -> children_of_ty ~ty v
    in
    match v.node.kind with
    | UnBv (_, v) -> if_ty v
    | UnBool (_, v) -> if_ty v
    | UnFloat (_, v) -> if_ty v
    | UnPtr (_, v) -> if_ty v
    | BvArith (_, v1, v2) -> if_ty v1 @ if_ty v2
    | BvCmp (_, v1, v2) -> if_ty v1 @ if_ty v2
    | FArith (_, v1, v2) -> if_ty v1 @ if_ty v2
    | FCmp (_, v1, v2) -> if_ty v1 @ if_ty v2
    | BoolBin (_, v1, v2) -> if_ty v1 @ if_ty v2
    | Eq (v1, v2) -> if_ty v1 @ if_ty v2
    | Ite (c, t, e) ->
        if equal c Bool.v_true then if_ty t
        else if equal c Bool.v_false then if_ty e
        else if_ty t @ if_ty e
    | _ -> Seq.empty

  and shrink : type a. a t -> a t Seq.t =
   fun v ->
    match v.node.kind with
    | Var _ -> small_of_ty v.node.ty
    | Bool _ -> Seq.empty
    | BitVec n ->
        let size = size_of v.node.ty in
        if Z.gt n Z.one then List.to_seq [ BitVec.zero size; BitVec.one size ]
        else if Z.gt n Z.zero then Seq.singleton (BitVec.zero size)
        else Seq.empty
    | _ ->
        let ty = v.node.ty in
        (just_var ty ++ small_of_ty ty) @ children_of_ty ~ty v
end
