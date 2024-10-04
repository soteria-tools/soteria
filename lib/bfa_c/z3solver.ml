module Value = Svalue

let log_src = Logs.Src.create "bfa_c.SOLVER"
let debug_str s = Logs.debug ~src:log_src (fun m -> m "%s" s)

let solver_log =
  Simple_smt.{ send = debug_str; receive = debug_str; stop = Fun.id }

let solver_config = { Simple_smt.z3 with log = solver_log }
let solver = Simple_smt.new_solver solver_config

open Simple_smt

let ( $$ ) = app
let ack_command = ack_command solver
let t_seq ty = list [ atom "Seq"; ty ]
let seq_singl t = atom "seq.unit" $$ [ t ]
let seq_concat ts = atom "seq.++" $$ ts

let t_void, void =
  let voidT = "Void" in
  let voidV = "CONST_VOID" in
  let cmd = Simple_smt.declare_datatype voidT [] [ (voidV, []) ] in
  ack_command cmd;
  (atom voidT, atom voidV)

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

let rec sort_of_ty = function
  | Value.TBool -> Simple_smt.t_bool
  | TInt -> Simple_smt.t_int
  | TSeq ty -> t_seq (sort_of_ty ty)
  | TPointer -> t_ptr
  | TVoid -> t_void

let declare_v (var : Value.t) =
  let v, ty =
    (* SValue and Solver should really be merged... *)
    match var.node with
    | Var (v, ty) -> (v, ty)
    | _ -> failwith "UNREACHABLE: alloc_v not a var"
  in
  let v = Value.Var_name.to_string v in
  declare v (sort_of_ty ty)

let rec encode_value (v : Value.t) =
  match v.node with
  | Var (v, _) -> atom (Value.Var_name.to_string v)
  | Int z -> int_zk z
  | Bool b -> bool_k b
  | Ptr (l, o) -> mk_ptr (encode_value l) (encode_value o)
  | Seq vs -> (
      match vs with
      | [] -> failwith "need type to encode empty lists"
      | _ :: _ ->
          List.map vs ~f:(fun v -> seq_singl (encode_value v)) |> seq_concat)
  | Unop (unop, v) -> (
      let v = encode_value v in
      match unop with Not -> bool_not v)
  | Binop (binop, v1, v2) -> (
      let v1 = encode_value v1 in
      let v2 = encode_value v2 in
      match binop with Eq -> eq v1 v2)
  | Void -> void

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
  List.iter vs ~f:(fun v ->
      let constr = encode_value v in
      ack_command (assume constr))

let simplified_bool v =
  match v.Hashcons.node with Value.Bool b -> Some b | _ -> None

let sat () =
  match check solver with
  | Sat -> true
  | Unsat -> false
  | Unknown ->
      Logs.info ~src:log_src (fun m -> m "Solver returned unknown");
      (* We return UNSAT by default: under-approximating behaviour *)
      false

let delayed_sat = sat

let check_entailment vs =
  save ();
  List.iter vs ~f:(fun v ->
      let constr = encode_value (Value.not v) in
      ack_command (assume constr));
  let res = sat () in
  backtrack ();
  res
