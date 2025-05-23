module Value = Typed
module Var = Svalue.Var
module L = Soteria_logs.Logs.L

let debug_str ~prefix s = L.smt (fun m -> m "%s: %s" prefix s)

open Simple_smt
open Z3utils

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

let solver_config () =
  { z3 with log = solver_log; exe = !Solver_config.current.z3_path }

let z3_solver () =
  let solver = new_solver (solver_config ()) in
  let command sexp =
    Dump.log_sexp sexp;
    let now = Unix.gettimeofday () in
    let res = solver.command sexp in
    let elapsed = Int.of_float ((Unix.gettimeofday () -. now) *. 1000.) in
    Dump.log_response res elapsed;
    res
  in
  { solver with command }

module Save_counter : Reversible.Mutable with type t = int ref = struct
  type t = int ref

  let init () = ref 0
  let reset t = t := 0
  let save t = incr t
  let backtrack_n t n = t := !t - n
end

module Solver_state = struct
  type t = Typed.sbool Typed.t Dynarray.t Dynarray.t

  let init () =
    let t = Dynarray.create () in
    Dynarray.add_last t (Dynarray.create ());
    t

  let reset t =
    Dynarray.clear t;
    Dynarray.add_last t (Dynarray.create ())

  let save t = Dynarray.add_last t (Dynarray.create ())
  let backtrack_n t n = Dynarray.truncate t (Dynarray.length t - n)

  let add_constraint (t : t) v =
    if Typed.equal v Typed.v_true then ()
    else
      match Dynarray.find_last t with
      | None -> failwith "add_constraint: empty array"
      | Some last ->
          if Typed.equal v Typed.v_false then (
            Dynarray.clear last;
            Dynarray.add_last last Typed.v_false)
          else Dynarray.add_last last v

  (** This function returns [Some b] if the solver state is trivially [b] (true
      or false). We maintain solver state such that trivial truths are never
      added to the state, and false is false erases everything else. Therefore,
      it is enough to check either for emptyness of the topmost layer or
      falseness of the latest element. *)
  let trivial_truthiness (t : t) =
    match Dynarray.find_last t with
    | None -> Some true
    | Some last -> (
        match Dynarray.find_last last with
        | None -> Some true
        | Some v when Typed.equal v Typed.v_false -> Some false
        | _ -> None)

  let iter (t : t) f = Dynarray.iter (fun t -> Dynarray.iter f t) t
  let to_value_list (t : t) = Iter.to_rev_list (iter t)

  let mem (t : t) v =
    Dynarray.exists (fun d -> Dynarray.exists (Typed.equal v) d) t
end

type t = {
  z3_solver : Simple_smt.solver;
  save_counter : Save_counter.t;
  var_counter : Var.Incr_counter_mut.t;
  state : Solver_state.t;
}

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

let init () =
  let z3_solver = z3_solver () in
  !initialize_solver z3_solver;
  ack_command z3_solver (Simple_smt.push 1);
  {
    z3_solver;
    save_counter = Save_counter.init ();
    var_counter = Var.Incr_counter_mut.init ();
    state = Solver_state.init ();
  }

let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let is_constr constr = list [ atom "_"; atom "is"; atom constr ]
let ack_command solver sexp = ack_command solver sexp
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
  register_solver_init (fun solver -> ack_command solver cmd);
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
  register_solver_init (fun solver -> ack_command solver cmd);
  ( atom opt,
    (fun v -> atom mk_some $ v),
    (fun v -> atom opt_unwrap $ v),
    atom none,
    (fun v -> is_constr mk_some $ v),
    fun v -> is_constr none $ v )

(********* End of solver declarations *********)

let save solver =
  Var.Incr_counter_mut.save solver.var_counter;
  Save_counter.save solver.save_counter;
  Solver_state.save solver.state;
  ack_command solver.z3_solver (Simple_smt.push 1)

let backtrack_n solver n =
  Var.Incr_counter_mut.backtrack_n solver.var_counter n;
  Solver_state.backtrack_n solver.state n;
  Save_counter.backtrack_n solver.save_counter n;
  ack_command solver.z3_solver (Simple_smt.pop n)

(* Initialise and reset *)

let reset solver =
  (* We want to go back to 1, meaning after the first push which saved the declarations *)
  let save_counter = !(solver.save_counter) in
  if save_counter < 0 then failwith "Solver reset: save_counter < 0???";
  Save_counter.reset solver.save_counter;
  Var.Incr_counter_mut.reset solver.var_counter;
  Solver_state.reset solver.state;
  ack_command solver.z3_solver (Simple_smt.pop (save_counter + 1));
  (* Make sure the basic definitions are saved again *)
  ack_command solver.z3_solver (Simple_smt.push 1)

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

let declare_v v_id ty =
  let v = Svalue.Var.to_string v_id in
  declare v (sort_of_ty (Typed.untype_type ty))

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
      match v.node.ty with
      | TFloat F16 -> f16_k @@ Float.of_string f
      | TFloat F32 -> f32_k @@ Float.of_string f
      | TFloat F64 -> f64_k @@ Float.of_string f
      | TFloat F128 -> f128_k @@ Float.of_string f
      | _ -> failwith "Non-float type given")
  | Bool b -> bool_k b
  | BitVec z ->
      let n =
        match v.node.ty with
        | TBitVector n -> n
        | _ -> failwith "Non-bitvector type given"
      in
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
      | GetPtrLoc -> get_loc v1
      | GetPtrOfs -> get_ofs v1
      | IntOfBool -> ite v1 (int_k 1) (int_k 0)
      | BvOfInt ->
          let size =
            match v.node.ty with
            | TBitVector n -> n
            | _ -> failwith "Non-bitvector type given"
          in
          bv_of_int size v1
      | IntOfBv signed -> int_of_bv signed v1
      | BvOfFloat -> (
          match v1_.node.ty with
          | TFloat F16 -> bv_of_f16 v1
          | TFloat F32 -> bv_of_f32 v1
          | TFloat F64 -> bv_of_f64 v1
          | TFloat F128 -> bv_of_f128 v1
          | _ -> failwith "Non-float type given")
      | FloatOfBv -> (
          match v.node.ty with
          | TFloat F16 -> f16_of_bv v1
          | TFloat F32 -> f32_of_bv v1
          | TFloat F64 -> f64_of_bv v1
          | TFloat F128 -> f128_of_bv v1
          | _ -> failwith "Non-float type given")
      | BvExtract (from_, to_) -> bv_extract to_ from_ v1)
  | Binop (binop, v1, v2) -> (
      let ty = v1.node.ty in
      let v1 = encode_value_memo v1 in
      let v2 = encode_value_memo v2 in
      match binop with
      | Eq -> eq v1 v2
      | Leq -> (if Svalue.is_float ty then fp_leq else num_leq) v1 v2
      | Lt -> (if Svalue.is_float ty then fp_lt else num_lt) v1 v2
      | And -> bool_and v1 v2
      | Or -> bool_or v1 v2
      | Plus -> (if Svalue.is_float ty then fp_add else num_add) v1 v2
      | Minus -> (if Svalue.is_float ty then fp_sub else num_sub) v1 v2
      | Times -> (if Svalue.is_float ty then fp_mul else num_mul) v1 v2
      | Div -> (if Svalue.is_float ty then fp_div else num_div) v1 v2
      | Rem -> (if Svalue.is_float ty then fp_rem else num_rem) v1 v2
      | Mod ->
          if Svalue.is_float ty then
            failwith "mod not implemented for floating points"
          else num_mod v1 v2
      | BitAnd -> bv_and v1 v2
      | BitOr -> bv_or v1 v2
      | BitXor -> bv_xor v1 v2
      | BitShl -> bv_shl v1 v2
      | BitShr -> bv_ashr v1 v2)
  | Nop (Distinct, vs) ->
      let vs = List.map encode_value_memo vs in
      distinct vs

and encode_value_memo v = (memoz memo_encode_value_tbl encode_value) v

let fresh_var solver ty =
  let v_id = Var.Incr_counter_mut.get_next solver.var_counter in
  let c = declare_v v_id ty in
  ack_command solver.z3_solver c;
  v_id

let fresh solver ty =
  let v_id = fresh_var solver ty in
  Typed.mk_var v_id ty

let rec simplify' solver (v : Svalue.t) : Svalue.t =
  match v.node.kind with
  | Int _ | Bool _ -> v
  | _ when Solver_state.mem solver.state (Typed.type_ v) -> Svalue.v_true
  | Unop (Not, e) ->
      let e' = simplify' solver e in
      if Svalue.equal e e' then v else Svalue.not e'
  | Binop (Eq, e1, e2) ->
      if Svalue.equal e1 e2 && (not @@ Svalue.is_float e1.node.ty) then
        Svalue.v_true
      else if Svalue.sure_neq e1 e2 then Svalue.v_false
      else v
  | Binop (Or, e1, e2) ->
      let se1 = simplify' solver e1 in
      let se2 = simplify' solver e2 in
      if Svalue.equal se1 e1 && Svalue.equal se2 e2 then v
      else Svalue.or_ se1 se2
  | _ -> v

and simplify solver (v : 'a Typed.t) : 'a Typed.t =
  v |> Typed.untyped |> simplify' solver |> Typed.type_

let is_diff_op (v : Svalue.t) =
  match v.node.kind with
  | Unop (Not, { node = { kind = Binop (Eq, v1, v2); _ }; _ }) -> Some (v1, v2)
  | _ -> None

let add_constraints solver ?(simplified = false) vs =
  let iter = vs |> Iter.of_list |> Iter.flat_map Typed.split_ands in
  iter @@ fun v ->
  let v = if simplified then v else simplify solver v in
  Solver_state.add_constraint solver.state v;
  ack_command solver.z3_solver @@ assume @@ encode_value @@ Typed.untyped v

let as_bool = Typed.as_bool

(* Incremental doesn't allow for caching queries... *)
let sat solver =
  match Solver_state.trivial_truthiness solver.state with
  | Some true -> Soteria_symex.Solver.Sat
  | Some false -> Unsat
  | None -> (
      let answer =
        try check solver.z3_solver
        with Simple_smt.UnexpectedSolverResponse s ->
          L.error (fun m ->
              m "Unexpected solver response: %s" (Sexplib.Sexp.to_string_hum s));
          Unknown
      in
      match answer with
      | Sat -> Sat
      | Unsat -> Unsat
      | Unknown ->
          L.warn (fun m -> m "Solver returned unknown");
          Unknown)

let as_values solver = Solver_state.to_value_list solver.state
