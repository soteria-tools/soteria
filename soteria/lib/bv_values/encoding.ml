open Logs.Import
open Soteria_std
open Soteria_smt
open Svalue

(** Lowers the svalues of a built typed layer [Typed] (from {!Typed.Make}) into
    SMT terms and sorts for the Z3 backend. *)
module Make (Typed : Typed_intf.Solver_value) = struct
  let pointers_not_supported () =
    L.failwith "Encoding of pointers is not supported in Bv_values"

  module Svalue = Typed.Svalue

  type t = Svalue.t
  type ty = Svalue.ty

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
    | TExtension x -> Typed.Ext.encode_ty sort_of_ty x

  let memo_encode_value_tbl : sexp Svalue.Hashtbl.t = Svalue.Hashtbl.create 1023

  let rm_to_smt : RoundingMode.t -> Soteria_smt.RoundingMode.t = function
    | NearestTiesToEven -> NearestTiesToEven
    | NearestTiesToAway -> NearestTiesToAway
    | Ceil -> Ceil
    | Floor -> Floor
    | Truncate -> Truncate

  let smt_of_unop : Unop.t -> sexp -> sexp = function
    | Not -> bool_not
    | FAbs -> fp_abs
    | GetPtrLoc -> pointers_not_supported ()
    | GetPtrOfs -> pointers_not_supported ()
    | BvOfBool n -> fun b -> ite b (bv_k n Z.one) (bv_k n Z.zero)
    | BvOfFloat (rm, true, n) -> sbv_of_float (rm_to_smt rm) n
    | BvOfFloat (rm, false, n) -> ubv_of_float (rm_to_smt rm) n
    | FloatOfBv (rm, true, fp) ->
        float_of_sbv (rm_to_smt rm) (FloatPrecision.size fp)
    | FloatOfBv (rm, false, fp) ->
        float_of_ubv (rm_to_smt rm) (FloatPrecision.size fp)
    | FloatOfBvRaw fp -> float_of_bv (FloatPrecision.size fp)
    | BvExtract (from_, to_) -> bv_extract to_ from_
    | BvExtend (true, by) -> bv_sign_extend by
    | BvExtend (false, by) -> bv_zero_extend by
    | BvNot -> bv_not
    | Neg _ -> bv_neg
    | FIs fc -> fp_is (FloatClass.as_fpclass fc)
    | FRound rm -> fp_round (rm_to_smt rm)

  let smt_of_binop : Binop.t -> sexp -> sexp -> sexp = function
    | Eq -> eq
    | And -> bool_and
    | Or -> bool_or
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
    | Add _ -> bv_add
    | Sub _ -> bv_sub
    | Mul _ -> bv_mul
    | Div true -> bv_sdiv
    | Div false -> bv_udiv
    | Rem true -> bv_srem
    | Rem false -> bv_urem
    | Mod -> bv_smod
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
    | BvConcat -> bv_concat

  let encode_var v = atom (Var.to_string v)

  let rec encode_value (v : Svalue.t) =
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
    | Ptr _ -> pointers_not_supported ()
    | Seq vs -> (
        match vs with
        | [] -> L.failwith "need type to encode empty lists"
        | _ :: _ ->
            List.map (fun v -> seq_singl (encode_value_memo v)) vs |> seq_concat
        )
    | Ite (c, t, e) ->
        ite (encode_value_memo c) (encode_value_memo t) (encode_value_memo e)
    | Exists (vs, sv) ->
        let encode_binder (v, ty) = list [ encode_var v; sort_of_ty ty ] in
        exists (List.map encode_binder vs) (encode_value_memo sv)
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
    | Extension x -> Typed.Ext.encode_value encode_value_memo x

  and encode_value_memo v =
    match Svalue.Hashtbl.find_opt memo_encode_value_tbl v with
    | Some k -> k
    | None ->
        let k = encode_value v in
        Svalue.Hashtbl.add memo_encode_value_tbl v k;
        k

  let encode_value (v : Svalue.t) =
    Svalue.Bool.split_ands v
    |> Iter.map encode_value_memo
    |> Iter.to_list
    |> bool_ands

  let init_commands = []
end
