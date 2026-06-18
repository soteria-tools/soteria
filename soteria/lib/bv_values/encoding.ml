open Logs.Import
open Soteria_std
open Soteria_smt

let pointers_not_supported () =
  L.failwith "Encoding of pointers is not supported in Bv_values"

(* The solver treats terms and types uniformly, so we encode the existentially
   wrapped forms. *)
type t = Svalue.packed
type ty = Svalue.packed_ty

let rec sort_of_ty : type a. a Svalue.ty -> sexp = function
  | TBool -> t_bool
  | TLoc n -> t_bits n
  | TFloat F16 -> t_f16
  | TFloat F32 -> t_f32
  | TFloat F64 -> t_f64
  | TFloat F128 -> t_f128
  | TSeq ty -> t_seq $ sort_of_ty ty
  | TPointer _ -> pointers_not_supported ()
  | TBitVector n -> t_bits n

let sort_of_ty (Svalue.PackedTy ty) = sort_of_ty ty
let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023

let rm_to_smt : Svalue.RoundingMode.t -> Soteria_smt.RoundingMode.t = function
  | NearestTiesToEven -> NearestTiesToEven
  | NearestTiesToAway -> NearestTiesToAway
  | Ceil -> Ceil
  | Floor -> Floor
  | Truncate -> Truncate

let smt_of_on_bv : type o. o Svalue.Unop.on_bv -> sexp -> sexp = function
  | FloatOfBv (rm, true, fp) ->
      float_of_sbv (rm_to_smt rm) (Svalue.FloatPrecision.size fp)
  | FloatOfBv (rm, false, fp) ->
      float_of_ubv (rm_to_smt rm) (Svalue.FloatPrecision.size fp)
  | FloatOfBvRaw fp -> float_of_bv (Svalue.FloatPrecision.size fp)
  | BvExtract (from_, to_) -> bv_extract to_ from_
  | BvExtend (true, by) -> bv_sign_extend by
  | BvExtend (false, by) -> bv_zero_extend by
  | BvNot -> bv_not
  | Neg _ -> bv_neg

let smt_of_on_bool : type o. o Svalue.Unop.on_bool -> sexp -> sexp = function
  | Not -> bool_not
  | BvOfBool n -> fun b -> ite b (bv_k n Z.one) (bv_k n Z.zero)

let smt_of_on_float : type o. o Svalue.Unop.on_float -> sexp -> sexp = function
  | BvOfFloat (rm, true, n) -> sbv_of_float (rm_to_smt rm) n
  | BvOfFloat (rm, false, n) -> ubv_of_float (rm_to_smt rm) n
  | FAbs -> fp_abs
  | FIs fc -> fp_is (Svalue.FloatClass.as_fpclass fc)
  | FRound rm -> fp_round (rm_to_smt rm)

let smt_of_on_ptr : type o. o Svalue.Unop.on_ptr -> sexp -> sexp = function
  | GetPtrLoc -> pointers_not_supported ()
  | GetPtrOfs -> pointers_not_supported ()

let smt_of_arith : Svalue.Binop.arith -> sexp -> sexp -> sexp = function
  | BitAnd -> bv_and
  | BitOr -> bv_or
  | BitXor -> bv_xor
  | Shl -> bv_shl
  | LShr -> bv_lshr
  | AShr -> bv_ashr
  | Add _ -> bv_add
  | Sub _ -> bv_sub
  | Mul _ -> bv_mul
  | Div true -> bv_sdiv
  | Div false -> bv_udiv
  | Rem true -> bv_srem
  | Rem false -> bv_urem
  | Mod -> bv_smod
  | BvConcat -> bv_concat

let smt_of_cmp : Svalue.Binop.cmp -> sexp -> sexp -> sexp = function
  | AddOvf true -> bv_saddo
  | AddOvf false -> bv_uaddo
  | SubOvf true -> bv_ssubo
  | SubOvf false -> bv_usubo
  | MulOvf true -> bv_smulo
  | MulOvf false -> bv_umulo
  | Lt true -> bv_slt
  | Lt false -> bv_ult
  | Leq true -> bv_sleq
  | Leq false -> bv_uleq

let smt_of_farith : Svalue.Binop.farith -> sexp -> sexp -> sexp = function
  | FAdd -> fp_add
  | FSub -> fp_sub
  | FMul -> fp_mul
  | FDiv -> fp_div
  | FRem -> fp_rem

let smt_of_fcmp : Svalue.Binop.fcmp -> sexp -> sexp -> sexp = function
  | FEq -> fp_eq
  | FLeq -> fp_leq
  | FLt -> fp_lt

let smt_of_boolean : Svalue.Binop.boolean -> sexp -> sexp -> sexp = function
  | And -> bool_and
  | Or -> bool_or

let encode_var v = atom (Svalue.Var.to_string v)

let rec encode_value : type a. a Svalue.t -> sexp =
 fun v ->
  match v.node.kind with
  | Var v -> encode_var v
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
  | LocLit z ->
      let n = Svalue.size_of v.node.ty in
      bv_k n z
  | Ptr _ -> pointers_not_supported ()
  | Seq vs -> (
      match vs with
      | [] -> L.failwith "need type to encode empty lists"
      | _ :: _ ->
          List.map (fun v -> seq_singl (encode_value_memo v)) vs |> seq_concat)
  | Ite (c, t, e) ->
      ite (encode_value_memo c) (encode_value_memo t) (encode_value_memo e)
  | Exists (vs, sv) ->
      let encode_binder (v, Svalue.PackedTy ty) =
        list [ encode_var v; sort_of_ty (Svalue.PackedTy ty) ]
      in
      exists (List.map encode_binder vs) (encode_value_memo sv)
  | UnBv (op, v1) -> smt_of_on_bv op (encode_value_memo v1)
  | UnBool (op, v1) -> smt_of_on_bool op (encode_value_memo v1)
  | UnFloat (op, v1) -> smt_of_on_float op (encode_value_memo v1)
  | UnPtr (op, v1) -> smt_of_on_ptr op (encode_value_memo v1)
  | BvArith (op, v1, v2) ->
      smt_of_arith op (encode_value_memo v1) (encode_value_memo v2)
  | BvCmp (op, v1, v2) ->
      smt_of_cmp op (encode_value_memo v1) (encode_value_memo v2)
  | FArith (op, v1, v2) ->
      smt_of_farith op (encode_value_memo v1) (encode_value_memo v2)
  | FCmp (op, v1, v2) ->
      smt_of_fcmp op (encode_value_memo v1) (encode_value_memo v2)
  | BoolBin (op, v1, v2) ->
      smt_of_boolean op (encode_value_memo v1) (encode_value_memo v2)
  | Eq (v1, v2) -> eq (encode_value_memo v1) (encode_value_memo v2)
  | Nop (Distinct, vs) ->
      let vs = List.map encode_value_memo vs in
      distinct vs

and encode_value_memo : type a. a Svalue.t -> sexp =
 fun v ->
  match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Svalue.tag with
  | Some k -> k
  | None ->
      let k = encode_value v in
      Hashtbl.Hint.add memo_encode_value_tbl v.Svalue.tag k;
      k

let encode_value (Svalue.Packed v) = encode_value_memo v
let init_commands = []
