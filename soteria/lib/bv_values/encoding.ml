open Soteria_std
open Simple_smt
open Solvers.Smt_utils

let pointers_not_supported () =
  failwith "Encoding of pointers is not supported in Bv_values"

type t = Svalue.t
type ty = Svalue.ty

let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let t_seq = atom "Seq"
let seq_singl t = atom "seq.unit" $$ [ t ]
let seq_concat ts = atom "seq.++" $$ ts

let rec sort_of_ty : Svalue.ty -> sexp = function
  | TBool -> t_bool
  | TLoc n -> t_bits n
  | TFloat F16 -> t_f16
  | TFloat F32 -> t_f32
  | TFloat F64 -> t_f64
  | TFloat F128 -> t_f128
  | TSeq ty -> t_seq $ sort_of_ty ty
  | TPointer _ -> pointers_not_supported ()
  | TBitVector n -> t_bits n

let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023

let rm_to_smt : Svalue.RoundingMode.t -> Solvers.Smt_utils.RoundingMode.t =
  function
  | NearestTiesToEven -> NearestTiesToEven
  | NearestTiesToAway -> NearestTiesToAway
  | Ceil -> Ceil
  | Floor -> Floor
  | Truncate -> Truncate

let smt_of_unop : Svalue.Unop.t -> sexp -> sexp = function
  | Not -> bool_not
  | FAbs -> fp_abs
  | GetPtrLoc -> pointers_not_supported ()
  | GetPtrOfs -> pointers_not_supported ()
  | BvOfBool n -> fun b -> ite b (bv_k n Z.one) (bv_k n Z.zero)
  | BvOfFloat (rm, true, n) -> sbv_of_float (rm_to_smt rm) n
  | BvOfFloat (rm, false, n) -> ubv_of_float (rm_to_smt rm) n
  | FloatOfBv (rm, true, fp) ->
      float_of_sbv (rm_to_smt rm) (Svalue.FloatPrecision.size fp)
  | FloatOfBv (rm, false, fp) ->
      float_of_ubv (rm_to_smt rm) (Svalue.FloatPrecision.size fp)
  | BvExtract (from_, to_) -> bv_extract to_ from_
  | BvExtend (true, by) -> bv_sign_extend by
  | BvExtend (false, by) -> bv_zero_extend by
  | BvNot -> bv_not
  | Neg -> bv_neg
  | NegOvf -> bv_nego
  | FIs fc -> fp_is (Svalue.FloatClass.as_fpclass fc)
  | FRound rm -> fp_round (rm_to_smt rm)

let smt_of_nop : Svalue.Nop.t -> sexp list -> sexp = function
  | Distinct -> distinct
  | And -> bool_ands
  | Or -> bool_ors
  | Add -> (
      (* we implement add but we could do a custom encoding to convert negs to bvsubs *)
      function
      | [] -> failwith "Add nop with no arguments"
      | h :: t -> List.fold_left bv_add h t)

let smt_of_binop : Svalue.Binop.t -> sexp -> sexp -> sexp = function
  | Eq -> eq
  | FEq -> fp_eq
  | FLeq -> fp_leq
  | FLt -> fp_lt
  | FAdd -> fp_add
  | FSub -> fp_sub
  | FMul -> fp_mul
  | FDiv -> fp_div
  | FRem -> fp_rem
  | BitAnd -> bv_and
  | BitOr -> bv_or
  | BitXor -> bv_xor
  | Shl -> bv_shl
  | LShr -> bv_lshr
  | AShr -> bv_ashr
  | Mul _ -> bv_mul
  | Div true -> bv_sdiv
  | Div false -> bv_udiv
  | Rem true -> bv_srem
  | Rem false -> bv_urem
  | Mod -> bv_smod
  | AddOvf true -> bv_saddo
  | AddOvf false -> bv_uaddo
  | MulOvf true -> bv_smulo
  | MulOvf false -> bv_umulo
  | Lt true -> bv_slt
  | Lt false -> bv_ult
  | Leq true -> bv_sleq
  | Leq false -> bv_uleq
  | BvConcat -> bv_concat

let rec encode_value (v : Svalue.t) =
  match v.node.kind with
  | Var v -> atom (Svalue.Var.to_string v)
  | Float f -> (
      match Svalue.precision_of_f v.node.ty with
      | F16 -> f16_k @@ Float.of_string f
      | F32 -> f32_k @@ Float.of_string f
      | F64 -> f64_k @@ Float.of_string f
      | F128 -> f128_k @@ Float.of_string f)
  | Bool b -> bool_k b
  | BitVec z ->
      let n = Svalue.size_of v.node.ty in
      bv_k n z
  | Ptr _ -> pointers_not_supported ()
  | Seq vs -> (
      match vs with
      | [] -> failwith "need type to encode empty lists"
      | _ :: _ ->
          List.map (fun v -> seq_singl (encode_value_memo v)) vs |> seq_concat)
  | Ite (c, t, e) ->
      ite (encode_value_memo c) (encode_value_memo t) (encode_value_memo e)
  | Unop (unop, v1) ->
      let v1 = encode_value_memo v1 in
      smt_of_unop unop v1
  | Binop (binop, v1, v2) ->
      let v1 = encode_value_memo v1 in
      let v2 = encode_value_memo v2 in
      smt_of_binop binop v1 v2
  (* | Nop (Add, vs) -> (
      let rest, neg =
        List.partition_map
          (fun (v : Svalue.t) ->
            match v.node.kind with
            | Unop (Neg, a) -> Right (encode_value_memo a)
            | _ -> Left (encode_value_memo v))
          vs
      in
      match (rest, neg) with
      | [], [] -> failwith "Add nop with no arguments"
      | [], [ v ] -> bv_neg v
      | h :: t, neg ->
          let sum = List.fold_left bv_add h t in
          List.fold_left bv_sub sum neg
      | [], h :: t ->
          let sum = List.fold_left bv_add h t in
          bv_neg sum) *)
  | Nop (nop, vs) ->
      let vs = List.map encode_value_memo vs in
      smt_of_nop nop vs

and encode_value_memo v =
  match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Hc.tag with
  | Some k -> k
  | None ->
      let k = encode_value v in
      Hashtbl.Hint.add memo_encode_value_tbl v.Hc.tag k;
      k

let encode_value (v : Svalue.t) =
  Svalue.Bool.split_ands v
  |> Iter.map encode_value_memo
  |> Iter.to_list
  |> bool_ands

let init_commands = []
(* [ Simple_smt.set_option ":sat.smt" "true" ] *)
