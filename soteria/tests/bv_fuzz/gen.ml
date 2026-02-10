(** Type-aware random expression pair generator.

    Generates pairs [(smart, direct)] of {!Soteria.Bv_values.Svalue.t}
    expressions simultaneously — one built via smart constructors and one via
    direct (non-simplifying) constructors. Both should be semantically
    equivalent. *)

open Soteria.Bv_values
module Sv = Svalue
module Smart = Svalue
module D = Direct

(** A pair of expressions: smart (simplified) and direct (raw). *)
type pair = Sv.t * Sv.t

(* ------------------------------------------------------------------ *)
(* Variable pool                                                       *)
(* ------------------------------------------------------------------ *)

(** Pre-allocated symbolic variables for each bitvector width. *)
let bv_var_pool : (int * int, pair) Hashtbl.t = Hashtbl.create 32

(** Pre-allocated boolean variables. *)
let bool_var_pool : (int, pair) Hashtbl.t = Hashtbl.create 8

(** Pre-allocated float variables for each precision. *)
let float_var_pool : (Sv.FloatPrecision.t * int, pair) Hashtbl.t =
  Hashtbl.create 8

let var_counter = ref 1000

let fresh_var () =
  let v = !var_counter in
  incr var_counter;
  Soteria.Symex.Var.of_int v

let get_bv_var bv_size idx =
  let key = (bv_size, idx) in
  match Hashtbl.find_opt bv_var_pool key with
  | Some p -> p
  | None ->
      let v = fresh_var () in
      let sv = Sv.mk_var v (Sv.t_bv bv_size) in
      (* Both smart and direct use the same variable *)
      let p = (sv, sv) in
      Hashtbl.replace bv_var_pool key p;
      p

let get_bool_var idx =
  match Hashtbl.find_opt bool_var_pool idx with
  | Some p -> p
  | None ->
      let v = fresh_var () in
      let sv = Sv.mk_var v Sv.t_bool in
      let p = (sv, sv) in
      Hashtbl.replace bool_var_pool idx p;
      p

let get_float_var fp idx =
  match Hashtbl.find_opt float_var_pool (fp, idx) with
  | Some p -> p
  | None ->
      let v = fresh_var () in
      let sv = Sv.mk_var v (Sv.t_float fp) in
      let p = (sv, sv) in
      Hashtbl.replace float_var_pool (fp, idx) p;
      p

(* ------------------------------------------------------------------ *)
(* Random helpers                                                      *)
(* ------------------------------------------------------------------ *)

let pick_random arr rs = arr.(Random.State.int rs (Array.length arr))

let random_z ~bv_size rs =
  (* Generate a random Z.t that fits in bv_size bits *)
  let bytes = Bytes.create ((bv_size + 7) / 8) in
  for i = 0 to Bytes.length bytes - 1 do
    Bytes.set bytes i (Char.chr (Random.State.int rs 256))
  done;
  let z = Z.of_bits (Bytes.to_string bytes) in
  Z.extract z 0 bv_size

let random_bool rs = Random.State.bool rs
let random_signed rs = Random.State.bool rs

(* ------------------------------------------------------------------ *)
(* Bitvector expression generation                                     *)
(* ------------------------------------------------------------------ *)

(** Operations that produce a bitvector of the same size as their inputs *)
type bv_same_binop =
  | BAdd
  | BSub
  | BMul
  | BAnd
  | BOr
  | BXor
  | BShl
  | BLshr
  | BAshr

type bv_same_unop = BNot | BNeg

let all_bv_same_binops =
  [| BAdd; BSub; BMul; BAnd; BOr; BXor; BShl; BLshr; BAshr |]

let all_bv_same_unops = [| BNot; BNeg |]

let apply_bv_same_binop op ~checked (s1, d1) (s2, d2) : pair =
  match op with
  | BAdd -> (Smart.BitVec.add ~checked s1 s2, D.BitVec.add ~checked d1 d2)
  | BSub -> (Smart.BitVec.sub ~checked s1 s2, D.BitVec.sub ~checked d1 d2)
  | BMul -> (Smart.BitVec.mul ~checked s1 s2, D.BitVec.mul ~checked d1 d2)
  | BAnd -> (Smart.BitVec.and_ s1 s2, D.BitVec.and_ d1 d2)
  | BOr -> (Smart.BitVec.or_ s1 s2, D.BitVec.or_ d1 d2)
  | BXor -> (Smart.BitVec.xor s1 s2, D.BitVec.xor d1 d2)
  | BShl -> (Smart.BitVec.shl s1 s2, D.BitVec.shl d1 d2)
  | BLshr -> (Smart.BitVec.lshr s1 s2, D.BitVec.lshr d1 d2)
  | BAshr -> (Smart.BitVec.ashr s1 s2, D.BitVec.ashr d1 d2)

let apply_bv_same_unop op (s, d) : pair =
  match op with
  | BNot -> (Smart.BitVec.not s, D.BitVec.not_ d)
  | BNeg -> (Smart.BitVec.neg s, D.BitVec.neg d)

(** Operations that produce a bool from two same-size bitvectors *)
type bv_cmp_op = BLt | BLeq | BEq | BAddOvf | BMulOvf

let all_bv_cmp_ops = [| BLt; BLeq; BEq; BAddOvf; BMulOvf |]

let apply_bv_cmp_op op ~signed (s1, d1) (s2, d2) : pair =
  match op with
  | BLt -> (Smart.BitVec.lt ~signed s1 s2, D.BitVec.lt ~signed d1 d2)
  | BLeq -> (Smart.BitVec.leq ~signed s1 s2, D.BitVec.leq ~signed d1 d2)
  | BEq -> (Smart.Bool.sem_eq s1 s2, D.Bool.eq d1 d2)
  | BAddOvf ->
      ( Smart.BitVec.add_overflows ~signed s1 s2,
        D.BitVec.add_overflows ~signed d1 d2 )
  | BMulOvf ->
      ( Smart.BitVec.mul_overflows ~signed s1 s2,
        D.BitVec.mul_overflows ~signed d1 d2 )

(** Operations that produce a bool from two booleans *)
type bool_binop = BoolAnd | BoolOr

let all_bool_binops = [| BoolAnd; BoolOr |]

let apply_bool_binop op (s1, d1) (s2, d2) : pair =
  match op with
  | BoolAnd -> (Smart.Bool.and_ s1 s2, D.Bool.and_ d1 d2)
  | BoolOr -> (Smart.Bool.or_ s1 s2, D.Bool.or_ d1 d2)

(* ------------------------------------------------------------------ *)
(* Float operation types                                               *)
(* ------------------------------------------------------------------ *)

type float_binop = FAdd | FSub | FMul | FDiv | FRem
type float_unop = FAbs | FRound_op

let all_float_binops = [| FAdd; FSub; FMul; FDiv; FRem |]
let all_float_unops = [| FAbs; FRound_op |]

type float_cmp_op = FEq | FLt | FLeq

let all_float_cmp_ops = [| FEq; FLt; FLeq |]

let apply_float_binop op (s1, d1) (s2, d2) : pair =
  match op with
  | FAdd -> (Smart.Float.add s1 s2, D.Float.add d1 d2)
  | FSub -> (Smart.Float.sub s1 s2, D.Float.sub d1 d2)
  | FMul -> (Smart.Float.mul s1 s2, D.Float.mul d1 d2)
  | FDiv -> (Smart.Float.div s1 s2, D.Float.div d1 d2)
  | FRem -> (Smart.Float.rem s1 s2, D.Float.rem d1 d2)

let apply_float_unop op (s, d) : pair =
  match op with
  | FAbs -> (Smart.Float.abs s, D.Float.abs d)
  | FRound_op ->
      let rm = Sv.RoundingMode.NearestTiesToEven in
      (Smart.Float.round rm s, D.Float.round rm d)

let apply_float_cmp_op op (s1, d1) (s2, d2) : pair =
  match op with
  | FEq -> (Smart.Float.eq s1 s2, D.Float.eq d1 d2)
  | FLt -> (Smart.Float.lt s1 s2, D.Float.lt d1 d2)
  | FLeq -> (Smart.Float.leq s1 s2, D.Float.leq d1 d2)

(* ------------------------------------------------------------------ *)
(* Core generators                                                     *)
(* ------------------------------------------------------------------ *)

let num_vars = 3

(** Generate a bitvector expression pair of the given size. *)
let rec gen_bv ~depth ~bv_size rs : pair =
  if depth <= 0 then gen_bv_leaf ~bv_size rs
  else
    let choice = Random.State.int rs 10 in
    match choice with
    (* 0-3: same-size binary op *)
    | 0 | 1 | 2 | 3 ->
        let op = pick_random all_bv_same_binops rs in
        (* checked=true is a precondition (caller guarantees no overflow), not
           something we can set randomly — always use false *)
        let checked = false in
        let p1 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let p2 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_same_binop op ~checked p1 p2
    (* 4-5: same-size unary op *)
    | 4 | 5 ->
        let op = pick_random all_bv_same_unops rs in
        let p = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_same_unop op p
    (* 6: ite *)
    | 6 ->
        let cond = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let p1 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let p2 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let s = Smart.Bool.ite (fst cond) (fst p1) (fst p2) in
        let d = D.Bool.ite (snd cond) (snd p1) (snd p2) in
        (s, d)
    (* 7: extract — generate wider, then extract bv_size bits *)
    | 7 when bv_size >= 2 ->
        let extra = Random.State.int rs (min 8 bv_size) in
        let wider = bv_size + extra in
        let from_ = Random.State.int rs (extra + 1) in
        let to_ = from_ + bv_size - 1 in
        let p = gen_bv ~depth:(depth - 1) ~bv_size:wider rs in
        ( Smart.BitVec.extract from_ to_ (fst p),
          D.BitVec.extract from_ to_ (snd p) )
    (* 8: extend — generate narrower, then extend *)
    | 8 when bv_size >= 2 ->
        let narrow = 1 + Random.State.int rs (bv_size - 1) in
        let by = bv_size - narrow in
        let signed = random_signed rs in
        let p = gen_bv ~depth:(depth - 1) ~bv_size:narrow rs in
        ( Smart.BitVec.extend ~signed by (fst p),
          D.BitVec.extend ~signed by (snd p) )
    (* 9: of_bool *)
    | 9 ->
        let p = gen_bool ~depth:(depth - 1) ~bv_size rs in
        (Smart.BitVec.of_bool bv_size (fst p), D.BitVec.of_bool bv_size (snd p))
    (* fallback for when bv_size < 2: unary op *)
    | _ ->
        let op = pick_random all_bv_same_unops rs in
        let p = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_same_unop op p

(** Generate a bitvector leaf (concrete value or symbolic variable). *)
and gen_bv_leaf ~bv_size rs : pair =
  if random_bool rs then
    (* Concrete bitvector *)
    let z = random_z ~bv_size rs in
    let s = Smart.BitVec.mk bv_size z in
    let d = D.BitVec.mk bv_size z in
    (s, d)
  else
    (* Symbolic variable *)
    let idx = Random.State.int rs num_vars in
    get_bv_var bv_size idx

(** Generate a boolean expression pair. [bv_size] is the bitvector width used
    when we need BV sub-expressions. *)
and gen_bool ~depth ~bv_size rs : pair =
  if depth <= 0 then gen_bool_leaf rs
  else
    let choice = Random.State.int rs 10 in
    match choice with
    (* 0-1: BV comparison *)
    | 0 | 1 ->
        let op = pick_random all_bv_cmp_ops rs in
        let signed = random_signed rs in
        let p1 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let p2 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_cmp_op op ~signed p1 p2
    (* 2-3: bool binary op *)
    | 2 | 3 ->
        let op = pick_random all_bool_binops rs in
        let p1 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let p2 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        apply_bool_binop op p1 p2
    (* 4: not *)
    | 4 ->
        let p = gen_bool ~depth:(depth - 1) ~bv_size rs in
        (Smart.Bool.not (fst p), D.Bool.not_ (snd p))
    (* 5: ite *)
    | 5 ->
        let cond = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let p1 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let p2 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        ( Smart.Bool.ite (fst cond) (fst p1) (fst p2),
          D.Bool.ite (snd cond) (snd p1) (snd p2) )
    (* 6: bool eq *)
    | 6 ->
        let p1 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let p2 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        (Smart.Bool.sem_eq (fst p1) (fst p2), D.Bool.eq (snd p1) (snd p2))
    (* 7: to_bool (bv -> bool) *)
    | 7 ->
        let p = gen_bv ~depth:(depth - 1) ~bv_size rs in
        (Smart.BitVec.to_bool (fst p), D.BitVec.to_bool (snd p))
    (* 8: float comparison *)
    | 8 ->
        let fp = pick_random [| Sv.FloatPrecision.F32; F64 |] rs in
        let op = pick_random all_float_cmp_ops rs in
        let p1 = gen_float ~depth:(depth - 1) ~fp rs in
        let p2 = gen_float ~depth:(depth - 1) ~fp rs in
        apply_float_cmp_op op p1 p2
    (* 9: float classification *)
    | 9 ->
        let fp = pick_random [| Sv.FloatPrecision.F32; F64 |] rs in
        let fc =
          pick_random
            [| Sv.FloatClass.Normal; Subnormal; Zero; Infinite; NaN |]
            rs
        in
        let p = gen_float ~depth:(depth - 1) ~fp rs in
        (Smart.Float.is_floatclass fc (fst p), D.Float.is fc (snd p))
    | _ -> gen_bool_leaf rs

(** Generate a boolean leaf (concrete or symbolic). *)
and gen_bool_leaf rs : pair =
  if random_bool rs then
    let b = random_bool rs in
    (Smart.Bool.bool b, D.Bool.bool b)
  else
    let idx = Random.State.int rs num_vars in
    get_bool_var idx

(** Generate a float expression pair. *)
and gen_float ~depth ~fp rs : pair =
  if depth <= 0 then gen_float_leaf ~fp rs
  else
    let choice = Random.State.int rs 6 in
    match choice with
    (* 0-2: binary float op *)
    | 0 | 1 | 2 ->
        let op = pick_random all_float_binops rs in
        let p1 = gen_float ~depth:(depth - 1) ~fp rs in
        let p2 = gen_float ~depth:(depth - 1) ~fp rs in
        apply_float_binop op p1 p2
    (* 3-4: unary float op *)
    | 3 | 4 ->
        let op = pick_random all_float_unops rs in
        let p = gen_float ~depth:(depth - 1) ~fp rs in
        apply_float_unop op p
    (* 5: ite *)
    | 5 ->
        let bv_size = 8 in
        let cond = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let p1 = gen_float ~depth:(depth - 1) ~fp rs in
        let p2 = gen_float ~depth:(depth - 1) ~fp rs in
        ( Smart.Bool.ite (fst cond) (fst p1) (fst p2),
          D.Bool.ite (snd cond) (snd p1) (snd p2) )
    | _ -> gen_float_leaf ~fp rs

(** Generate a float leaf (symbolic variable only — no concrete floats to avoid
    NaN comparison issues in generation). *)
and gen_float_leaf ~fp rs : pair =
  let idx = Random.State.int rs num_vars in
  get_float_var fp idx

(* ------------------------------------------------------------------ *)
(* Exported top-level generators                                       *)
(* ------------------------------------------------------------------ *)

(** Generate a bitvector equivalence check pair. *)
let gen_bv_pair ~depth ~bv_size rs : pair = gen_bv ~depth ~bv_size rs

(** Generate a boolean equivalence check pair. *)
let gen_bool_pair ~depth ~bv_size rs : pair = gen_bool ~depth ~bv_size rs

(** Generate a float equivalence check pair. *)
let gen_float_pair ~depth ~fp rs : pair = gen_float ~depth ~fp rs
