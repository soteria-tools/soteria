(** Random expression generator and smart-constructor replay.

    Generates a random {!Soteria.Bv_values.Svalue.t} expression built via direct
    (non-simplifying) constructors, then replays it through the smart
    constructors using {!Soteria.Bv_values.Eval.eval} to obtain a simplified
    version. Both should be semantically equivalent. *)
open QCheck2

open Soteria.Bv_values
module Sv = Svalue
module D = Direct
module Var = Soteria.Symex.Var

(* ------------------------------------------------------------------ *)
(* Variable pool                                                       *)
(* ------------------------------------------------------------------ *)

let gen_var ~ty =
  let open Direct.Var_pool in
  let open Gen in
  let+ idx = int_bound (max_var_per_ty - 1) in
  get_from_pool idx ty

let gen_bv_var ~bv_size = gen_var ~ty:(TBitVector bv_size)
let gen_bool_var = gen_var ~ty:TBool

let gen_z ~bv_size =
  let open QCheck2.Gen in
  (* We limit most tests to 16-bit integers anyway *)
  assert (bv_size < 63);
  let+ v = QCheck2.Gen.int_bound ((1 lsl bv_size) - 1) in
  Z.of_int v

(* ------------------------------------------------------------------ *)
(* Bitvector operations to pick from                                   *)
(* ------------------------------------------------------------------ *)

let bv_checked_binops = [| D.BitVec.add; D.BitVec.sub; D.BitVec.mul |]

let bv_unchecked_binops =
  [|
    D.BitVec.and_;
    D.BitVec.or_;
    D.BitVec.xor;
    D.BitVec.shl;
    D.BitVec.lshr;
    D.BitVec.ashr;
  |]

let bv_binop : 'a Gen.t =
  let bv_checked_binop =
    let open Gen in
    let* op = oneof_array bv_checked_binops in
    let+ checked = bool in
    op ~checked
  in
  let bv_unchecked_binop = Gen.oneof_array bv_unchecked_binops in
  Gen.oneof_weighted
    [
      (Array.length bv_checked_binops, bv_checked_binop);
      (Array.length bv_unchecked_binops, bv_unchecked_binop);
    ]

let all_bv_unops = [| D.BitVec.not_; D.BitVec.neg |]

(* ------------------------------------------------------------------ *)
(* Bool operations that compare bitvectors                             *)
(* ------------------------------------------------------------------ *)

let all_bv_cmp_ops =
  [|
    D.BitVec.lt;
    D.BitVec.leq;
    (fun ~signed:_ -> D.Bool.eq);
    D.BitVec.add_overflows;
    D.BitVec.mul_overflows;
  |]

(* ------------------------------------------------------------------ *)
(* Core generators — produce direct (non-simplifying) AST only        *)
(* ------------------------------------------------------------------ *)

(** Generate a bitvector expression of the given size via direct constructors.
*)
let rec gen_bv ~bv_size : Sv.t Gen.sized =
  let open Gen in
  fun depth ->
    if depth <= 0 then gen_bv_leaf ~bv_size
    else
      let gen_same_size_binop =
        let* bv_binop = bv_binop in
        let* v1 = gen_bv ~bv_size (depth - 1) in
        let+ v2 = gen_bv ~bv_size (depth - 1) in
        bv_binop v1 v2
      in
      let gen_unop =
        let* bv_unop = oneof_array all_bv_unops in
        let+ v = gen_bv ~bv_size (depth - 1) in
        bv_unop v
      in
      let gen_ite =
        let generator =
          let* cond = gen_bool ~bv_size (depth - 1) in
          let* t = gen_bv ~bv_size (depth - 1) in
          let+ e = gen_bv ~bv_size (depth - 1) in
          D.Bool.ite cond t e
        in
        let shrink_to_branches (ite : Sv.t) =
          match ite.node.kind with
          | Sv.Ite (_, t, e) -> List.to_seq [ t; e ]
          | _ -> Seq.empty
        in
        Gen.set_shrink shrink_to_branches generator
      in
      let gen_extract =
        if bv_size >= 2 then
          let* extra = Gen.int_bound (min 8 bv_size - 1) in
          let wider = bv_size + extra in
          let* from_ = Gen.int_bound extra in
          let to_ = from_ + bv_size - 1 in
          let+ v = gen_bv ~bv_size:wider (depth - 1) in
          D.BitVec.extract from_ to_ v
        else
          (* Fallback *)
          gen_unop
      in
      let gen_extend =
        if bv_size >= 2 then
          let* narrow_m_1 = Gen.int_bound (bv_size - 2) in
          let narrow = narrow_m_1 + 1 in
          let by = bv_size - narrow in
          let* signed = Gen.bool in
          let+ v = gen_bv ~bv_size:narrow (depth - 1) in
          D.BitVec.extend ~signed by v
        else
          (* Fallback *)
          gen_unop
      in
      let gen_of_bool =
        let+ v = gen_bool ~bv_size (depth - 1) in
        D.BitVec.of_bool bv_size v
      in
      oneof_weighted
        [
          (1, gen_bv_leaf ~bv_size);
          (1, gen_of_bool);
          (2, gen_unop);
          (1, gen_extract);
          (1, gen_extend);
          (1, gen_ite);
          (4, gen_same_size_binop);
        ]

(** Generate a bitvector leaf (concrete value or symbolic variable). *)
and gen_bv_leaf ~bv_size : Sv.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let gen_concrete =
    let+ z = gen_z ~bv_size in
    D.BitVec.mk bv_size z
  in
  oneof [ gen_concrete; gen_bv_var ~bv_size ]

(** Generate a boolean expression. [bv_size] is the bitvector width used when we
    need BV sub-expressions. *)
and gen_bool ~bv_size : Sv.t Gen.sized =
  let open Gen in
  fun depth ->
    if depth <= 0 then gen_bool_leaf
    else
      let gen_bool_binop =
        let* v1 = gen_bool ~bv_size (depth - 1) in
        let* v2 = gen_bool ~bv_size (depth - 1) in
        let+ op = oneof_array [| D.Bool.and_; D.Bool.or_ |] in
        op v1 v2
      in
      let gen_not =
        let+ v = gen_bool ~bv_size (depth - 1) in
        D.Bool.not_ v
      in
      let gen_ite =
        let generator =
          let* cond = gen_bool ~bv_size (depth - 1) in
          let* t = gen_bool ~bv_size (depth - 1) in
          let+ e = gen_bool ~bv_size (depth - 1) in
          D.Bool.ite cond t e
        in
        let shrink_to_branches (ite : Sv.t) =
          match ite.node.kind with
          | Sv.Ite (_, t, e) -> List.to_seq [ t; e ]
          | _ -> Seq.empty
        in
        Gen.set_shrink shrink_to_branches generator
      in
      let gen_eq =
        let* v1 = gen_bv ~bv_size (depth - 1) in
        let+ v2 = gen_bv ~bv_size (depth - 1) in
        D.Bool.eq v1 v2
      in
      let gen_to_bool =
        let+ v = gen_bv ~bv_size (depth - 1) in
        D.BitVec.to_bool v
      in
      let gen_bv_comp =
        let* op = oneof_array all_bv_cmp_ops in
        let* signed = Gen.bool in
        let* v1 = gen_bv ~bv_size (depth - 1) in
        let+ v2 = gen_bv ~bv_size (depth - 1) in
        op ~signed v1 v2
      in
      oneof_weighted
        [
          (1, gen_bool_leaf);
          (1, gen_eq);
          (1, gen_not);
          (1, gen_to_bool);
          (1, gen_ite);
          (2, gen_bool_binop);
          (3, gen_bv_comp);
        ]

(** Generate a boolean leaf (concrete or symbolic). *)
and gen_bool_leaf : Sv.t Gen.t =
  let open Gen in
  let gen_concrete =
    let+ b = bool in
    D.Bool.bool b
  in
  oneof [ gen_concrete; gen_bool_var ]

let depth = Gen.int_bound 6

(* We could restrict to 8 and 16, but this means the generator can then shrink
   the inputs to bvs of size 2 if it can reproduce it there and that's really
   nice. *)
let bv_size = Gen.int_range 1 16

let gen_bv : Sv.t Gen.t =
  let open Gen in
  set_shrink Direct.Shrink.shrink
    (let* bv_size = bv_size in
     Gen.sized_size depth @@ gen_bv ~bv_size)

let gen_bool : Sv.t Gen.t =
  let open Gen in
  set_shrink Direct.Shrink.shrink
    (let* bv_size = bv_size in
     Gen.sized_size depth @@ gen_bool ~bv_size)
