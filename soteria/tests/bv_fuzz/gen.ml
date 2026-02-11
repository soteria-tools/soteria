(** Random expression generator and smart-constructor replay.

    Generates a random {!Soteria.Bv_values.Svalue.t} expression built via direct
    (non-simplifying) constructors, then replays it through the smart
    constructors using {!Soteria.Bv_values.Eval.eval} to obtain a simplified
    version. Both should be semantically equivalent. *)

open Soteria.Bv_values
module Sv = Svalue
module D = Direct

(* ------------------------------------------------------------------ *)
(* Variable pool                                                       *)
(* ------------------------------------------------------------------ *)

(** Pre-allocated symbolic variables for each bitvector width. *)
let bv_var_pool : (int * int, Sv.t) Hashtbl.t = Hashtbl.create 32

(** Pre-allocated boolean variables. *)
let bool_var_pool : (int, Sv.t) Hashtbl.t = Hashtbl.create 8

let var_counter = ref 1000

let fresh_var () =
  let v = !var_counter in
  incr var_counter;
  Soteria.Symex.Var.of_int v

let get_bv_var bv_size idx =
  let key = (bv_size, idx) in
  match Hashtbl.find_opt bv_var_pool key with
  | Some v -> v
  | None ->
      let v = fresh_var () in
      let sv = Sv.mk_var v (Sv.t_bv bv_size) in
      Hashtbl.replace bv_var_pool key sv;
      sv

let get_bool_var idx =
  match Hashtbl.find_opt bool_var_pool idx with
  | Some v -> v
  | None ->
      let v = fresh_var () in
      let sv = Sv.mk_var v Sv.t_bool in
      Hashtbl.replace bool_var_pool idx sv;
      sv

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
(* Bitvector operations to pick from                                   *)
(* ------------------------------------------------------------------ *)

type bv_binop = BAdd | BSub | BMul | BAnd | BOr | BXor | BShl | BLshr | BAshr
type bv_unop = BNot | BNeg

let all_bv_binops = [| BAdd; BSub; BMul; BAnd; BOr; BXor; BShl; BLshr; BAshr |]
let all_bv_unops = [| BNot; BNeg |]

let apply_bv_binop op ~checked v1 v2 =
  match op with
  | BAdd -> D.BitVec.add ~checked v1 v2
  | BSub -> D.BitVec.sub ~checked v1 v2
  | BMul -> D.BitVec.mul ~checked v1 v2
  | BAnd -> D.BitVec.and_ v1 v2
  | BOr -> D.BitVec.or_ v1 v2
  | BXor -> D.BitVec.xor v1 v2
  | BShl -> D.BitVec.shl v1 v2
  | BLshr -> D.BitVec.lshr v1 v2
  | BAshr -> D.BitVec.ashr v1 v2

let apply_bv_unop op v =
  match op with BNot -> D.BitVec.not_ v | BNeg -> D.BitVec.neg v

(* ------------------------------------------------------------------ *)
(* Bool operations that compare bitvectors                             *)
(* ------------------------------------------------------------------ *)

type bv_cmp_op = BLt | BLeq | BEq | BAddOvf | BMulOvf

let all_bv_cmp_ops = [| BLt; BLeq; BEq; BAddOvf; BMulOvf |]

let apply_bv_cmp_op op ~signed v1 v2 =
  match op with
  | BLt -> D.BitVec.lt ~signed v1 v2
  | BLeq -> D.BitVec.leq ~signed v1 v2
  | BEq -> D.Bool.eq v1 v2
  | BAddOvf -> D.BitVec.add_overflows ~signed v1 v2
  | BMulOvf -> D.BitVec.mul_overflows ~signed v1 v2

(* ------------------------------------------------------------------ *)
(* Core generators — produce direct (non-simplifying) AST only        *)
(* ------------------------------------------------------------------ *)

let num_vars = 3

(** Generate a bitvector expression of the given size via direct constructors.
*)
let rec gen_bv ~depth ~bv_size rs : Sv.t =
  if depth <= 0 then gen_bv_leaf ~bv_size rs
  else
    let choice = Random.State.int rs 10 in
    match choice with
    (* 0-3: same-size binary op *)
    | 0 | 1 | 2 | 3 ->
        let op = pick_random all_bv_binops rs in
        let checked = false in
        let v1 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let v2 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_binop op ~checked v1 v2
    (* 4-5: unary op *)
    | 4 | 5 ->
        let op = pick_random all_bv_unops rs in
        let v = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_unop op v
    (* 6: ite *)
    | 6 ->
        let cond = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let v1 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let v2 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        D.Bool.ite cond v1 v2
    (* 7: extract — generate wider, then extract bv_size bits *)
    | 7 when bv_size >= 2 ->
        let extra = Random.State.int rs (min 8 bv_size) in
        let wider = bv_size + extra in
        let from_ = Random.State.int rs (extra + 1) in
        let to_ = from_ + bv_size - 1 in
        let v = gen_bv ~depth:(depth - 1) ~bv_size:wider rs in
        D.BitVec.extract from_ to_ v
    (* 8: extend — generate narrower, then extend *)
    | 8 when bv_size >= 2 ->
        let narrow = 1 + Random.State.int rs (bv_size - 1) in
        let by = bv_size - narrow in
        let signed = random_signed rs in
        let v = gen_bv ~depth:(depth - 1) ~bv_size:narrow rs in
        D.BitVec.extend ~signed by v
    (* 9: of_bool *)
    | 9 ->
        let v = gen_bool ~depth:(depth - 1) ~bv_size rs in
        D.BitVec.of_bool bv_size v
    (* fallback for when bv_size < 2: unary op *)
    | _ ->
        let op = pick_random all_bv_unops rs in
        let v = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_unop op v

(** Generate a bitvector leaf (concrete value or symbolic variable). *)
and gen_bv_leaf ~bv_size rs : Sv.t =
  if random_bool rs then
    let z = random_z ~bv_size rs in
    D.BitVec.mk bv_size z
  else
    let idx = Random.State.int rs num_vars in
    get_bv_var bv_size idx

(** Generate a boolean expression. [bv_size] is the bitvector width used when we
    need BV sub-expressions. *)
and gen_bool ~depth ~bv_size rs : Sv.t =
  if depth <= 0 then gen_bool_leaf rs
  else
    let choice = Random.State.int rs 10 in
    match choice with
    (* 0-2: BV comparison *)
    | 0 | 1 | 2 ->
        let op = pick_random all_bv_cmp_ops rs in
        let signed = random_signed rs in
        let v1 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let v2 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_cmp_op op ~signed v1 v2
    (* 3-4: bool binary op (and / or) *)
    | 3 | 4 ->
        let v1 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let v2 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        if random_bool rs then D.Bool.and_ v1 v2 else D.Bool.or_ v1 v2
    (* 5: not *)
    | 5 ->
        let v = gen_bool ~depth:(depth - 1) ~bv_size rs in
        D.Bool.not_ v
    (* 6: ite *)
    | 6 ->
        let cond = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let v1 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let v2 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        D.Bool.ite cond v1 v2
    (* 7: bool eq *)
    | 7 ->
        let v1 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        let v2 = gen_bool ~depth:(depth - 1) ~bv_size rs in
        D.Bool.eq v1 v2
    (* 8: to_bool (bv -> bool), i.e. not(v == 0) *)
    | 8 ->
        let v = gen_bv ~depth:(depth - 1) ~bv_size rs in
        D.BitVec.to_bool v
    (* 9: additional BV comparison *)
    | 9 ->
        let op = pick_random all_bv_cmp_ops rs in
        let signed = random_signed rs in
        let v1 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        let v2 = gen_bv ~depth:(depth - 1) ~bv_size rs in
        apply_bv_cmp_op op ~signed v1 v2
    | _ -> gen_bool_leaf rs

(** Generate a boolean leaf (concrete or symbolic). *)
and gen_bool_leaf rs : Sv.t =
  if random_bool rs then D.Bool.bool (random_bool rs)
  else
    let idx = Random.State.int rs num_vars in
    get_bool_var idx

(* ------------------------------------------------------------------ *)
(* Exported top-level generators                                       *)
(* ------------------------------------------------------------------ *)

(** Replay a direct AST through the smart constructors.

    Unlike {!Eval.eval} (which short-circuits when children are physically
    unchanged), this function unconditionally calls the smart constructor at
    every node. Variables and literals are returned as-is since they are already
    canonical. *)
let smartify (x : Sv.t) : Sv.t =
  let memo : (int, Sv.t) Hashtbl.t = Hashtbl.create 64 in
  let rec go (v : Sv.t) : Sv.t =
    match Hashtbl.find_opt memo v.Hc.tag with
    | Some r -> r
    | None ->
        let r = go_node v in
        Hashtbl.replace memo v.Hc.tag r;
        r
  and go_node (v : Sv.t) : Sv.t =
    match v.Hc.node.kind with
    | Sv.Var _ | Sv.Bool _ | Sv.BitVec _ -> v
    | Sv.Unop (op, a) ->
        let a' = go a in
        Eval.eval_unop op a'
    | Sv.Binop (op, a, b) ->
        let a' = go a in
        let b' = go b in
        Eval.eval_binop op a' b'
    | Sv.Ite (c, t, e) ->
        let c' = go c in
        let t' = go t in
        let e' = go e in
        Sv.Bool.ite c' t' e'
    | Sv.Nop (Sv.Nop.Distinct, l) -> Sv.Bool.distinct (List.map go l)
    | Sv.Ptr (l, o) -> Sv.Ptr.mk (go l) (go o)
    | Sv.Float _ | Sv.Seq _ -> v
  in
  go x

(** Generate a bitvector expression (direct) and its smartified version. *)
let gen_bv_pair ~depth ~bv_size rs : Sv.t * Sv.t =
  let direct = gen_bv ~depth ~bv_size rs in
  let smart = smartify direct in
  (smart, direct)

(** Generate a boolean expression (direct) and its smartified version. *)
let gen_bool_pair ~depth ~bv_size rs : Sv.t * Sv.t =
  let direct = gen_bool ~depth ~bv_size rs in
  let smart = smartify direct in
  (smart, direct)
