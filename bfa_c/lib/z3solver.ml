module Value = Svalue

let z3_env_var = "BFA_Z3_PATH"
let log_src = Logs.Src.create "bfa_c.SOLVER"
let debug_str ~prefix s = Logs.debug ~src:log_src (fun m -> m "%s: %s" prefix s)

open Simple_smt

let smallest_power_of_two_greater_than n =
  let f n =
    if n < 1 then 1
    else
      let n = n - 1 in
      let n = n lor (n lsr 1) in
      let n = n lor (n lsr 2) in
      let n = n lor (n lsr 4) in
      let n = n lor (n lsr 8) in
      let n = n lor (n lsr 16) in
      n + 1
  in
  f (n + 1)

let solver_log =
  {
    send = debug_str ~prefix:"SMT:SEND";
    receive = debug_str ~prefix:"SMT:RECV";
    stop = Fun.id;
  }

let smt_log_file = ref None

let close_smt_log_file () =
  match !smt_log_file with None -> () | Some oc -> close_out oc

let () = at_exit close_smt_log_file

let set_smt_file f =
  close_smt_log_file ();
  match f with
  | None -> smt_log_file := None
  | Some f -> smt_log_file := Some (open_out f)

let log_sexp sexp =
  match !smt_log_file with
  | None -> ()
  | Some oc ->
      Sexplib.Sexp.output_hum oc sexp;
      output_char oc '\n';
      flush oc

let solver_config =
  let base_config = { z3 with log = solver_log } in
  match Sys.getenv_opt z3_env_var with
  | None -> base_config
  | Some exe -> { z3 with exe }

let solver =
  let solver = new_solver solver_config in
  let command sexp =
    log_sexp sexp;
    solver.command sexp
  in
  { solver with command }

let simplify v = solver.command (list [ atom "simplify"; v ])
let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let is_constr constr = list [ atom "_"; atom "is"; atom constr ]
let ack_command sexp = ack_command solver sexp
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
  ack_command cmd;
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
  ack_command cmd;
  ( atom opt,
    (fun v -> atom mk_some $ v),
    (fun v -> atom opt_unwrap $ v),
    atom none,
    (fun v -> is_constr mk_some $ v),
    fun v -> is_constr none $ v )

let rec sort_of_ty = function
  | Value.TBool -> Simple_smt.t_bool
  | TInt -> Simple_smt.t_int
  | TLoc -> Simple_smt.t_int
  | TSeq ty -> t_seq $ sort_of_ty ty
  | TPointer -> t_ptr
  | TOption ty -> t_opt $ sort_of_ty ty

let var_history = ref (Array.make 1023 Svalue.zero)

let var_of_int i =
  let res = Array.get !var_history i in
  if Svalue.equal Svalue.zero res then invalid_arg "var_of_int" else res

let var_of_str s = var_of_int (Value.Var_name.of_string s)

let record_var (var : Value.t) =
  match var.node.kind with
  | Var v ->
      if v < Array.length !var_history then Array.set !var_history v var
      else
        let new_arr =
          Array.make (smallest_power_of_two_greater_than v) Svalue.zero
        in
        Array.blit !var_history 0 new_arr 0 (Array.length !var_history);
        Array.set new_arr v var;
        var_history := new_arr
  | _ -> failwith "UNREACHABLE: record_var not a var"

let declare_v (var : Value.t) =
  record_var var;
  let v, ty =
    (* SValue and Solver should really be merged... *)
    match var.node with
    | { kind = Var v; ty } -> (v, ty)
    | _ -> failwith "UNREACHABLE: alloc_v not a var"
  in
  let v = Value.Var_name.to_string v in
  declare v (sort_of_ty ty)

let memo_encode_value_tbl : sexp Utils.Hint.t = Utils.Hint.create 1023

let memoz table f v =
  match Utils.Hint.find_opt table v.Hashcons.tag with
  | Some k -> k
  | None ->
      let k = f v in
      Utils.Hint.add table v.Hashcons.tag k;
      k

let rec encode_value (v : Svalue.t) =
  match v.node.kind with
  | Var v -> atom (Value.Var_name.to_string v)
  | Int z -> int_zk z
  | Bool b -> bool_k b
  | Ptr (l, o) -> mk_ptr (encode_value_memo l) (encode_value_memo o)
  | Opt opt -> (
      match opt with
      | None -> none
      | Some v ->
          let v = encode_value_memo v in
          mk_some v)
  | Seq vs -> (
      match vs with
      | [] -> failwith "need type to encode empty lists"
      | _ :: _ ->
          List.map (fun v -> seq_singl (encode_value_memo v)) vs |> seq_concat)
  | Unop (unop, v) -> (
      let v = encode_value_memo v in
      match unop with
      | Not -> bool_not v
      | IsSome -> is_some v
      | IsNone -> is_none v
      | GetPtrLoc -> get_loc v
      | GetPtrOfs -> get_ofs v
      | UnwrapOpt -> opt_unwrap v
      | IntOfBool -> ite v (int_k 1) (int_k 0))
  | Binop (binop, v1, v2) -> (
      let v1 = encode_value_memo v1 in
      let v2 = encode_value_memo v2 in
      match binop with
      | Eq -> eq v1 v2
      | Geq -> num_geq v1 v2
      | Gt -> num_gt v1 v2
      | Leq -> num_leq v1 v2
      | Lt -> num_lt v1 v2
      | And -> bool_and v1 v2
      | Plus -> num_add v1 v2
      | Minus -> num_sub v1 v2
      | Times -> num_mul v1 v2
      | Div -> num_div v1 v2)
  | Nop (nop, vs) -> (
      let vs = List.map encode_value_memo vs in
      match nop with Distinct -> distinct vs)

and encode_value_memo v = (memoz memo_encode_value_tbl encode_value) v

let decode_value (v : sexp) =
  match v with
  | Atom "true" -> Some Value.v_true
  | Atom "false" -> Some Value.v_false
  | _ ->
      L.debug (fun m -> m "SMT: Cannot decode value %a" Sexplib.Sexp.pp v);
      None

let fresh ty =
  let v = Value.fresh ty in
  let c = declare_v v in
  ack_command c;
  v

let save_cmd = Simple_smt.push 1
let save () = ack_command save_cmd
let backtrack_cmd = Simple_smt.pop 1
let backtrack () = ack_command backtrack_cmd

let add_constraints vs =
  vs
  |> List.iter (fun v ->
         let constr = encode_value v in
         ack_command (assume constr))

let simplify (v : Svalue.t) =
  match v.node.kind with
  | Int _ | Bool _ -> v
  | _ -> (
      let enc = encode_value v in
      let res = simplify enc in
      match decode_value res with Some d -> d | None -> v)

let as_bool v =
  if Svalue.equal v Svalue.v_true then Some true
  else if Svalue.equal v Svalue.v_false then Some false
  else None

(* Incremental doesn't allow for caching queries... *)
let sat () =
  let answer =
    try check solver
    with Simple_smt.UnexpectedSolverResponse s ->
      Logs.err ~src:log_src (fun m ->
          m "Unexpected solver response: %s" (Sexplib.Sexp.to_string_hum s));
      Unknown
  in
  match answer with
  | Sat -> true
  | Unsat -> false
  | Unknown ->
      Logs.info ~src:log_src (fun m -> m "Solver returned unknown");
      (* We return UNSAT by default: under-approximating behaviour *)
      false

let delayed_sat = sat

let check_entailment vs =
  save ();
  vs
  |> List.iter (fun v ->
         let constr = encode_value (Value.not v) in
         ack_command (assume constr));
  let res = sat () in
  backtrack ();
  res
