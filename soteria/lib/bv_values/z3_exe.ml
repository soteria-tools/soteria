open Simple_smt
module L = Soteria_logs.Logs.L

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
    | BvOfFloat n -> bv_of_float n
    | FloatOfBv F16 -> f16_of_bv
    | FloatOfBv F32 -> f32_of_bv
    | FloatOfBv F64 -> f64_of_bv
    | FloatOfBv F128 -> f128_of_bv
    | BvExtract (from_, to_) -> bv_extract to_ from_
    | BvExtend (true, by) -> bv_sign_extend by
    | BvExtend (false, by) -> bv_zero_extend by
    | BvNot -> bv_not
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

let add_constraint solver v =
  let vs =
    Iter.to_list @@ Iter.map Encoding.encode_value @@ Svalue.Bool.split_ands v
  in
  let sexp = Simple_smt.assume (Simple_smt.bool_ands vs) in
  ack_command solver sexp

let check_sat solver : Soteria_symex.Solver.result =
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
