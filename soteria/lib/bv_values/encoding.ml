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

let smt_of_unop : Svalue.Unop.t -> sexp -> sexp = function
  | Not -> bool_not
  | FAbs -> fp_abs
  | GetPtrLoc -> pointers_not_supported ()
  | GetPtrOfs -> pointers_not_supported ()
  | BvOfBool n -> fun b -> ite b (bv_k n Z.one) (bv_k n Z.zero)
  | BvOfFloat (true, n) -> sbv_of_float n
  | BvOfFloat (false, n) -> ubv_of_float n
  | FloatOfBv (true, fp) -> float_of_sbv (Svalue.FloatPrecision.size fp)
  | FloatOfBv (false, fp) -> float_of_ubv (Svalue.FloatPrecision.size fp)
  | BvExtract (from_, to_) -> bv_extract to_ from_
  | BvExtend (true, by) -> bv_sign_extend by
  | BvExtend (false, by) -> bv_zero_extend by
  | BvNot -> bv_not
  | Neg -> bv_neg
  | NegOvf -> bv_nego
  | FIs fc -> fp_is (Svalue.FloatClass.as_fpclass fc)
  | FRound Ceil -> fp_round Ceil
  | FRound Floor -> fp_round Floor
  | FRound NearestTiesToAway -> fp_round NearestTiesToAway
  | FRound NearestTiesToEven -> fp_round NearestTiesToEven
  | FRound Truncate -> fp_round Truncate

let smt_of_binop : Svalue.Binop.t -> sexp -> sexp -> sexp = function
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
  | Add -> bv_add
  | Sub -> bv_sub
  | Mul -> bv_mul
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
  Svalue.Bool.split_ands v
  |> Iter.map encode_value_memo
  |> Iter.to_list
  |> bool_ands

module LetBinder = struct
  module VarTbl = Hashtbl.Make (struct
    type t = Svalue.t

    let equal = Svalue.equal
    let hash (v : t) = v.tag
  end)

  module VarSet = Hashset.Make (struct
    type t = Svalue.Var.t

    let equal = Svalue.Var.equal
    let hash = Svalue.Var.to_int
    let pp = Svalue.Var.pp
  end)

  (** Generates an appropriate [Var] that uniquely identifies this value. *)
  let var_of_value (v : Svalue.t) = Svalue.Var.of_int v.tag

  (** Like [var_of_value] but returns a properly typed [Svalue]. *)
  let val_of_value (v : Svalue.t) = Svalue.mk_var (var_of_value v) v.node.ty

  (** Returns the set of [Var]s mentionned in this value. *)
  let dependencies v =
    let deps = VarSet.with_capacity 31 in
    Svalue.iter v (fun v ->
        match v.node.kind with Var dep -> VarSet.add deps dep | _ -> ());
    deps

  (** [replace ?cond v] replaces [v] with a [Var] node with id [v.tag] if
      [cond v] is true ([cond] defaults to [Fun.const true]); otherwise
      recursively replaces the children. *)
  let rec replace ?(cond = Fun.const true) (v : Svalue.t) =
    let replace = replace ~cond in
    if cond v then val_of_value v
    else
      match v.node.kind with
      | Var _ | Bool _ | Float _ | BitVec _ -> v
      | Ite (c, t, e) ->
          let c' = replace c in
          let t' = replace t in
          let e' = replace e in
          if c == c' && t == t' && e == e' then v else Svalue.Bool.ite c' t' e'
      | Unop (unop, v1) ->
          let v1' = replace v1 in
          if v1 == v1' then v else Eval.eval_unop unop v1'
      | Binop (binop, v1, v2) ->
          let v1' = replace v1 in
          let v2' = replace v2 in
          if v1 == v1' && v2 == v2' then v else Eval.eval_binop binop v1' v2'
      | Nop (Distinct, vs) ->
          let vs', changed = List.map_changed replace vs in
          if Stdlib.not changed then v else Svalue.Bool.distinct vs'
      | Seq vs ->
          let vs', changed = List.map_changed replace vs in
          if Stdlib.not changed then v else Svalue.SSeq.mk ~seq_ty:v.node.ty vs'
      | Ptr (l, o) ->
          let l' = replace l in
          let o' = replace o in
          if l == l' && o == o' then v else Svalue.Ptr.mk l' o'

  (** Makes the bindings for a map of bindings (ignores the pointed-to value).
      Will return, for each binding, an appropriate variable for the binding
      (using [var_of_value]), the encoded value (with possible sub-bindings
      replaced), and a set containing the binding dependencies of this value. *)
  let mk_bindings bind_map =
    VarTbl.fold
      (fun v _ acc ->
        let var = var_of_value v in
        let v =
          replace ~cond:(fun w -> VarTbl.mem bind_map w && w.tag <> v.tag) v
        in
        let sexp = encode_value v in
        let deps = dependencies v in
        (var, sexp, deps) :: acc)
      bind_map []

  (** [order_bindings known_vars bindings] will order the given [bindings] into
      groups of bindings; each group only needs the variables in the previous
      groups or in [known_vars] to be defined. *)
  let order_bindings known_vars bindings =
    let for_all f s = Seq.for_all f (VarSet.to_seq s) in
    let rec aux acc acc_full cur_visited
        (bindings : (Svalue.Var.t * sexp * VarSet.t) list)
        (next_bindings : (Svalue.Var.t * sexp * VarSet.t) list) :
        (string * sexp) list list =
      match bindings with
      | [] -> (
          match next_bindings with
          | [] -> acc :: acc_full
          | _ ->
              List.iter (VarSet.add known_vars) cur_visited;
              aux [] (acc :: acc_full) [] next_bindings [])
      | ((v, sexp, deps) as binding) :: bindings ->
          if VarSet.mem known_vars v then
            aux acc acc_full cur_visited bindings next_bindings
          else if for_all (VarSet.mem known_vars) deps then
            aux
              ((Svalue.Var.to_string v, sexp) :: acc)
              acc_full (v :: cur_visited) bindings next_bindings
          else aux acc acc_full cur_visited bindings (binding :: next_bindings)
    in
    aux [] [] [] bindings []

  (** Will calculate the let-bindings for the given value. Will only consider
      bindings for values that occur at least [min_occurrences] times, and will
      only return any bindings at all if at least [min_binds] are found matching
      the above criterium. Will return the value with any relevant binding
      substituted (but {b not} defined in the AST!) along with the bindings to
      be applied after encoding. *)
  let let_binds_for ?(min_binds = 5) ?(min_occurrences = 20) v =
    let tag_counts = VarTbl.create 255 in

    Svalue.iter v (fun v ->
        match v.node.kind with
        | Var _ | Bool _ | Float _ | BitVec _ -> ()
        | _ ->
            let curr =
              VarTbl.find_opt tag_counts v |> Option.value ~default:0
            in
            VarTbl.replace tag_counts v (curr + 1));
    VarTbl.filter_map_inplace
      (fun _ count -> if count >= min_occurrences then Some count else None)
      tag_counts;

    let length = VarTbl.length tag_counts in
    if length == 0 || length < min_binds then (v, [])
    else
      let bindings = mk_bindings tag_counts in

      let known_vars = dependencies v in
      let bindings = order_bindings known_vars bindings in

      let v_subst = replace ~cond:(VarTbl.mem tag_counts) v in
      (v_subst, bindings)

  (** [apply_bindings bindings sexp] Applies the given groups of [bindings] to
      [sexp]. *)
  let apply_bindings bindings sexp =
    List.fold_left (Fun.flip Simple_smt.let_) sexp bindings
end

let encode_value v =
  let v, bindings = LetBinder.let_binds_for ~min_occurrences:10 v in
  encode_value v |> LetBinder.apply_bindings bindings

let init_commands = []
