open Soteria_std
open Simple_smt
module L = Logging.Logs.L

let initialize_solver : (Simple_smt.solver -> unit) ref = ref (fun _ -> ())

let register_solver_init f =
  let old = !initialize_solver in
  let f' solver =
    old solver;
    f solver
  in
  initialize_solver := f'

let () =
  register_solver_init (fun solver ->
      match !Solver_config.current.solver_timeout with
      | None -> ()
      | Some timeout ->
          ack_command solver (set_option ":timeout" (string_of_int timeout)))

module Encoding = struct
  open Smt_utils

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
    | TPointer _ ->
        failwith "The pointer type is currently unsupported for SMT-lib"
    | TBitVector n -> t_bits n

  let memo_encode_value_tbl : sexp Hashtbl.Hint.t = Hashtbl.Hint.create 1023

  let smt_of_unop : Svalue.Unop.t -> sexp -> sexp = function
    | Not -> bool_not
    | FAbs -> fp_abs
    | GetPtrLoc -> failwith "Pointers are unsupported for SMT-lib"
    | GetPtrOfs -> failwith "Pointers are unsupported for SMT-lib"
    | BvOfBool n -> fun b -> ite b (bv_k n Z.one) (bv_k n Z.zero)
    | BvOfFloat (signed, n) -> bv_of_float signed n
    | FloatOfBv (signed, fp) -> float_of_bv signed fp
    | BvExtract (from_, to_) -> bv_extract to_ from_
    | BvExtend (true, by) -> bv_sign_extend by
    | BvExtend (false, by) -> bv_zero_extend by
    | BvNot -> bv_not
    | BvNeg -> bv_neg
    | BvNegOvf -> bv_nego
    | FIs fc -> fp_is fc
    | FRound rm -> fp_round rm

  let smt_of_binop : Svalue.Binop.t -> sexp -> sexp -> sexp = function
    | Eq -> eq
    | And -> bool_and
    | Or -> bool_or
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
    | Ptr _ -> failwith "Pointers are unsupported for SMT-lib"
    | Seq vs -> (
        match vs with
        | [] -> failwith "need type to encode empty lists"
        | _ :: _ ->
            List.map (fun v -> seq_singl (encode_value_memo v)) vs |> seq_concat
        )
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

  let encode_value v = encode_value_memo v
end

let solver_log =
  let debug_str ~prefix s = L.smt (fun m -> m "%s: %s" prefix s) in
  {
    send = debug_str ~prefix:"-> ";
    receive = debug_str ~prefix:"<- ";
    stop = Fun.id;
  }

module Dump = struct
  let current_channel = ref None

  let close_channel () =
    match !current_channel with
    | None -> ()
    | Some (oc, _) ->
        close_out oc;
        current_channel := None

  let () = at_exit close_channel

  let open_channel f =
    let oc = open_out f in
    current_channel := Some (oc, f);
    Some oc

  let channel () =
    (* We only open if current file is not None and its different from current config *)
    match (!Solver_config.current.dump_smt_file, !current_channel) with
    | None, None -> None
    | Some f, None -> open_channel f
    | Some f, Some (oc, f') ->
        if f == f' then Some oc
        else (
          close_channel ();
          open_channel f)
    | None, Some _ ->
        close_channel ();
        None

  let log_sexp sexp =
    match channel () with
    | None -> ()
    | Some oc ->
        Sexplib.Sexp.output_hum oc sexp;
        flush oc

  let log_response response elapsed =
    match channel () with
    | None -> ()
    | Some oc ->
        output_string oc " ; -> ";
        Sexplib.Sexp.output_hum oc response;
        if elapsed > 0 && not !Solver_config.current.hide_response_times then (
          output_string oc " (";
          output_string oc (string_of_int elapsed);
          output_string oc "ms)");
        output_char oc '\n';
        flush oc
end

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
        let sexp = Encoding.encode_value v in
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

  (** [apply_bindings sexp bindings] Applies the given groups of [bindings] to
      [sexp]. *)
  let apply_bindings sexp bindings =
    List.fold_left (Fun.flip Simple_smt.let_) sexp bindings
end

type t = Simple_smt.solver

let solver_config () =
  { z3 with log = solver_log; exe = !Solver_config.current.z3_path }

let init () =
  let solver = new_solver (solver_config ()) in
  let command sexp =
    Dump.log_sexp sexp;
    let now = Unix.gettimeofday () in
    let res = solver.command sexp in
    let elapsed = Int.of_float ((Unix.gettimeofday () -. now) *. 1000.) in
    Dump.log_response res elapsed;
    res
  in
  let solver = { solver with command } in
  !initialize_solver solver;
  solver

let declare_var solver name ty =
  let name = Soteria_symex.Var.to_string name in
  let ty = Encoding.sort_of_ty ty in
  let sexp = declare name ty in
  ack_command solver sexp

let add_constraint solver (v : Svalue.t) =
  let v, bindings = LetBinder.let_binds_for ~min_binds:1 ~min_occurrences:5 v in
  let sexp =
    match v.node.kind with
    | Binop (And, _, _) ->
        Svalue.Bool.split_ands v
        |> Iter.uniq ~eq:Svalue.equal
        |> Iter.map Encoding.encode_value
        |> Iter.to_list
        |> Simple_smt.bool_ands
    | _ -> Encoding.encode_value v
  in
  let sexp = LetBinder.apply_bindings sexp bindings in
  let sexp = Simple_smt.assume sexp in
  ack_command solver sexp

let check_sat solver : Soteria_symex.Solver_result.t =
  let smt_res =
    try check solver
    with Simple_smt.UnexpectedSolverResponse s ->
      L.error (fun m ->
          m "Unexpected solver response: %s" (Sexplib.Sexp.to_string_hum s));
      Unknown
  in
  match smt_res with
  | Sat -> Sat
  | Unsat -> Unsat
  | Unknown ->
      L.info (fun m -> m "Solver returned unknown");
      Unknown

let push solver n = ack_command solver (Simple_smt.push n)
let pop solver n = ack_command solver (Simple_smt.pop n)

let reset solver =
  ack_command solver (Simple_smt.simple_command [ "reset" ]);
  !initialize_solver solver
