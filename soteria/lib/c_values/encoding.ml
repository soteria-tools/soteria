open Soteria_std
open Simple_smt
open Solvers.Smt_utils

type t = Svalue.t
type ty = Svalue.ty

let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let t_seq = atom "Seq"
let seq_singl t = atom "seq.unit" $$ [ t ]
let seq_concat ts = atom "seq.++" $$ ts

let t_ptr, mk_ptr, get_loc, get_ofs, init_commands =
  let ptr = "Ptr" in
  let mk_ptr = "mk-ptr" in
  let loc = "loc" in
  let ofs = "ofs" in
  let cmd =
    declare_datatype ptr [] [ (mk_ptr, [ (loc, t_int); (ofs, t_int) ]) ]
  in

  ( atom ptr,
    (fun l o -> atom mk_ptr $$ [ l; o ]),
    (fun p -> atom loc $$ [ p ]),
    (fun p -> atom ofs $$ [ p ]),
    [ cmd ] )

let rec sort_of_ty : ty -> sexp = function
  | TBool -> t_bool
  | TInt -> t_int
  | TLoc -> t_int
  | TFloat F16 -> t_f16
  | TFloat F32 -> t_f32
  | TFloat F64 -> t_f64
  | TFloat F128 -> t_f128
  | TSeq ty -> t_seq $ sort_of_ty ty
  | TPointer -> t_ptr
  | TBitVector n -> t_bits n

let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023

let smt_of_unop : Svalue.Unop.t -> sexp -> sexp = function
  | Not -> bool_not
  | FAbs -> fp_abs
  | GetPtrLoc -> get_loc
  | GetPtrOfs -> get_ofs
  | IntOfBool -> fun b -> ite b (int_k 1) (int_k 0)
  | BvOfInt (_, size) -> bv_of_int size
  | IntOfBv signed -> int_of_bv signed
  | BvOfFloat (true, n) -> sbv_of_float n
  | BvOfFloat (false, n) -> ubv_of_float n
  | FloatOfBv (true, fp) -> float_of_sbv (Svalue.FloatPrecision.size fp)
  | FloatOfBv (false, fp) -> float_of_ubv (Svalue.FloatPrecision.size fp)
  | BvExtract (from_, to_) -> bv_extract to_ from_
  | BvExtend (true, by) -> bv_sign_extend by
  | BvExtend (false, by) -> bv_zero_extend by
  | BvNot -> bv_not
  | BvNegOvf -> bv_nego
  | FIs fc -> fp_is (Svalue.FloatClass.as_fpclass fc)
  | FRound Ceil -> fp_round Ceil
  | FRound Floor -> fp_round Floor
  | FRound NearestTiesToAway -> fp_round NearestTiesToAway
  | FRound NearestTiesToEven -> fp_round NearestTiesToEven
  | FRound Truncate -> fp_round Truncate

let smt_of_binop : Svalue.Binop.t -> sexp -> sexp -> sexp = function
  | Eq -> eq
  | Leq -> num_leq
  | Lt -> num_lt
  | And -> bool_and
  | Or -> bool_or
  | Plus -> num_add
  | Minus -> num_sub
  | Times -> num_mul
  | Div -> num_div
  | Rem -> num_rem
  | Mod -> num_mod
  | FEq -> fp_eq
  | FLeq -> fp_leq
  | FLt -> fp_lt
  | FPlus -> fp_add
  | FMinus -> fp_sub
  | FTimes -> fp_mul
  | FDiv -> fp_div
  | FRem -> fp_rem
  | BitAnd -> bv_and
  | BitOr -> bv_or
  | BitXor -> bv_xor
  | BitShl -> bv_shl
  | BitLShr -> bv_lshr
  | BitAShr -> bv_ashr
  | BvPlus -> bv_add
  | BvMinus -> bv_sub
  | BvTimes -> bv_mul
  | BvDiv true -> bv_sdiv
  | BvDiv false -> bv_udiv
  | BvRem true -> bv_srem
  | BvRem false -> bv_urem
  | BvMod -> bv_smod
  | BvPlusOvf true -> bv_saddo
  | BvPlusOvf false -> bv_uaddo
  | BvTimesOvf true -> bv_smulo
  | BvTimesOvf false -> bv_umulo
  | BvLt true -> bv_slt
  | BvLt false -> bv_ult
  | BvLeq true -> bv_sleq
  | BvLeq false -> bv_uleq
  | BvConcat -> bv_concat

let rec encode_value (v : Svalue.t) =
  match v.node.kind with
  | Var v -> atom (Svalue.Var.to_string v)
  | Int z -> int_zk z
  | Float f -> (
      match Svalue.precision_of_f v.node.ty with
      | F16 -> f16_k @@ Float.of_string f
      | F32 -> f32_k @@ Float.of_string f
      | F64 -> f64_k @@ Float.of_string f
      | F128 -> f128_k @@ Float.of_string f)
  | Bool b -> bool_k b
  | BitVec z ->
      let n = Svalue.size_of_bv v.node.ty in
      bv_k n z
  | Ptr (l, o) -> mk_ptr (encode_value_memo l) (encode_value_memo o)
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
  | Nop (Distinct, vs) ->
      let vs = List.map encode_value_memo vs in
      distinct vs

and encode_value_memo v =
  match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Hc.tag with
  | Some k -> k
  | None ->
      let k = encode_value v in
      Hashtbl.Hint.add memo_encode_value_tbl v.Hc.tag k;
      k

let encode_value (v : Svalue.t) =
  Svalue.split_ands v |> Iter.map encode_value_memo |> Iter.to_list |> bool_ands
