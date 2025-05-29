open Simple_smt
open Smt_utils

let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let is_constr constr = list [ atom "_"; atom "is"; atom constr ]
let t_seq = atom "Seq"
let seq_singl t = atom "seq.unit" $$ [ t ]
let seq_concat ts = atom "seq.++" $$ ts

let t_ptr, mk_ptr, get_loc, get_ofs =
  let ptr = "Ptr" in
  let mk_ptr = "mk-ptr" in
  let loc = "loc" in
  let ofs = "ofs" in
  let cmd =
    Simple_smt.(
      declare_datatype ptr [] [ (mk_ptr, [ (loc, t_int); (ofs, t_int) ]) ])
  in
  Solver_exe.register_solver_init (fun solver -> ack_command solver cmd);
  ( atom ptr,
    (fun l o -> atom mk_ptr $$ [ l; o ]),
    (fun p -> atom loc $$ [ p ]),
    fun p -> atom ofs $$ [ p ] )

let t_opt, mk_some, opt_unwrap, none, is_some, is_none =
  let opt = "Opt" in
  let mk_some = "mk-some" in
  let opt_unwrap = "opt-unwrap" in
  let none = "none" in
  let cmd =
    Simple_smt.(
      declare_datatype opt [ "P" ]
        [ (mk_some, [ (opt_unwrap, atom "P") ]); (none, []) ])
  in
  Solver_exe.register_solver_init (fun solver -> ack_command solver cmd);
  ( atom opt,
    (fun v -> atom mk_some $ v),
    (fun v -> atom opt_unwrap $ v),
    atom none,
    (fun v -> is_constr mk_some $ v),
    fun v -> is_constr none $ v )

let rec sort_of_ty = function
  | Svalue.TBool -> Simple_smt.t_bool
  | TInt -> Simple_smt.t_int
  | TLoc -> Simple_smt.t_int
  | TFloat F16 -> t_f16
  | TFloat F32 -> t_f32
  | TFloat F64 -> t_f64
  | TFloat F128 -> t_f128
  | TSeq ty -> t_seq $ sort_of_ty ty
  | TPointer -> t_ptr
  | TBitVector n -> t_bits n

let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023

let memoz table f v =
  match Hashtbl.Hint.find_opt table v.Hashcons.tag with
  | Some k -> k
  | None ->
      let k = f v in
      Hashtbl.Hint.add table v.Hashcons.tag k;
      k

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
  | Unop (unop, v1_) -> (
      let v1 = encode_value_memo v1_ in
      match unop with
      | Not -> bool_not v1
      | FAbs -> fp_abs v1
      | GetPtrLoc -> get_loc v1
      | GetPtrOfs -> get_ofs v1
      | IntOfBool -> ite v1 (int_k 1) (int_k 0)
      | BvOfInt ->
          let size = Svalue.size_of_bv v.node.ty in
          bv_of_int size v1
      | IntOfBv signed -> int_of_bv signed v1
      | BvOfFloat -> (
          match Svalue.precision_of_f v1_.node.ty with
          | F16 -> bv_of_f16 v1
          | F32 -> bv_of_f32 v1
          | F64 -> bv_of_f64 v1
          | F128 -> bv_of_f128 v1)
      | FloatOfBv -> (
          match Svalue.precision_of_f v.node.ty with
          | F16 -> f16_of_bv v1
          | F32 -> f32_of_bv v1
          | F64 -> f64_of_bv v1
          | F128 -> f128_of_bv v1)
      | BvExtract (from_, to_) -> bv_extract to_ from_ v1
      | FIs fc -> fp_is fc v1
      | FRound rm -> fp_round rm v1)
  | Binop (binop, v1, v2) -> (
      let v1 = encode_value_memo v1 in
      let v2 = encode_value_memo v2 in
      match binop with
      | Eq -> eq v1 v2
      | Leq -> num_leq v1 v2
      | Lt -> num_lt v1 v2
      | And -> bool_and v1 v2
      | Or -> bool_or v1 v2
      | Plus -> num_add v1 v2
      | Minus -> num_sub v1 v2
      | Times -> num_mul v1 v2
      | Div -> num_div v1 v2
      | Rem -> num_rem v1 v2
      | Mod -> num_mod v1 v2
      | FEq -> fp_eq v1 v2
      | FLeq -> fp_leq v1 v2
      | FLt -> fp_lt v1 v2
      | FPlus -> fp_add v1 v2
      | FMinus -> fp_sub v1 v2
      | FTimes -> fp_mul v1 v2
      | FDiv -> fp_div v1 v2
      | FRem -> fp_rem v1 v2
      | BitAnd -> bv_and v1 v2
      | BitOr -> bv_or v1 v2
      | BitXor -> bv_xor v1 v2
      | BitShl -> bv_shl v1 v2
      | BitShr -> bv_lshr v1 v2
      | BvPlus -> bv_add v1 v2
      | BvMinus -> bv_sub v1 v2)
  | Nop (Distinct, vs) ->
      let vs = List.map encode_value_memo vs in
      distinct vs

and encode_value_memo v =
  match Hashtbl.Hint.find_opt memo_encode_value_tbl v.Hashcons.tag with
  | Some k -> k
  | None ->
      let k = encode_value v in
      Hashtbl.Hint.add memo_encode_value_tbl v.Hashcons.tag k;
      k

let encode_value v = encode_value_memo v
