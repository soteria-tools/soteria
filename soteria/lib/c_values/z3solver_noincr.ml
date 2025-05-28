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
  (** Each slot holds a symbolic boolean, as well a boolean indicating if it was
      checked to be satisfiable. The boolean is mutable and can be mutated even
      by future branches! If a branch downstream is satisfiable, then so is any
      element on the path condition. *)
  type slot = {
    value : Typed.sbool Typed.t; [@printer Typed.ppa]
    mutable checked : bool;
  }
  [@@deriving show]

  (* Invariants: the PC only has checked things, and then only unchecked things. *)

  type t = slot Dynarray.t Dynarray.t [@@deriving show]

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
            (* We mark false as unchecked to make sure trivial_truthiness doesn't infer the wrong thing. *)
            Dynarray.add_last last { value = Typed.v_false; checked = false })
          else Dynarray.add_last last { value = v; checked = false }

  let to_seq_rev (t : t) =
    Seq.concat_map Dynarray.to_seq_rev (Dynarray.to_seq_rev t)

  (** This function returns [Some b] if the solver state is trivially [b] (true
      or false). We maintain solver state such that trivial truths are never
      added to the state, and false is false erases everything else. Therefore,
      it is enough to check either for emptyness of the topmost layer or
      falseness of the latest element. *)
  let trivial_truthiness (t : t) =
    match to_seq_rev t () with
    | Nil -> Some true (* The empty constraint is satisfiable *)
    | Cons ({ value; checked = true }, _) -> Some true
    | Cons ({ value; _ }, _) when Typed.(equal value v_false) -> Some false
    | _ -> None

  (* We check if the thing contains the value itself, or its negation. *)
  let trivial_truthiness_of (t : t) (v : Typed.sbool Typed.t) =
    let neg_v = Typed.not v in
    Dynarray.find_map
      (fun d ->
        Dynarray.find_map
          (fun { value; checked } ->
            if Typed.equal value v then Some true
            else if Typed.equal value neg_v then Some false
            else None)
          d)
      t

  let iter (t : t) f =
    Dynarray.iter (fun t -> Dynarray.iter (fun { value; _ } -> f value) t) t

  let to_value_list (t : t) = Iter.to_rev_list (iter t)

  let mem (t : t) v =
    Dynarray.exists
      (fun d -> Dynarray.exists (fun { value; _ } -> Typed.equal v value) d)
      t

  (** If we have checked sat and obtaied SAT, we can mark all elements of the
      list as checked! *)
  let mark_checked (t : t) =
    let rec aux seq =
      match seq () with
      | Seq.Cons (({ checked = false; _ } as slot), rest) ->
          (* If we see something unchecked we mark as checked and continue *)
          slot.checked <- true;
          aux rest
      | _ ->
          (* Otherwise we stop *)
          ()
    in
    aux (to_seq_rev t)

  (** We aggregate the unchecked constraints. We start from the right and
      collect all constraints marked as unchecked. We also aggregate all
      variables contained by the unchecked assertions, and fetch, All assertions
      (even checked) that also contain these variables. In the end, we need to
      fetch the closure from these variables.

      The function returns the list to encode, as well the set of all variables
      required. *)
  let unchecked_constraints t =
    L.smt (fun m -> m "Solver_state.unchecked_constraint:@\n%a" pp t);
    let changed = ref false in
    let var_set = Var.Hashset.with_capacity 8 in
    let vars value = Value.iter_vars value |> Iter.map fst in
    let to_encode = Dynarray.create () in
    let add_vars_raw vars = Var.Hashset.add_iter var_set vars in
    let add_vars vars =
      vars @@ fun v ->
      if not (Var.Hashset.mem var_set v) then (
        changed := true;
        Var.Hashset.add var_set v)
    in
    let relevant vars =
      L.smt (fun m -> m "Current relevant vars: %a" Var.Hashset.pp var_set);
      Iter.exists (Var.Hashset.mem var_set) vars
    in
    (* We need to reach some kind of fixpoint *)
    let rec aux_checked others seq =
      match seq () with
      | Seq.Nil ->
          L.smt (fun m -> m "Reached the end of the sequence");
          if !changed then (
            L.smt (fun m -> m "Set has changed, re-iterating");
            changed := false;
            aux_checked Seq.empty others)
          else ()
      | Seq.Cons (({ value; _ } as slot), rest) ->
          L.smt (fun m -> m "Checking checked slot: %a" Typed.ppa value);
          let vars = vars value in
          if relevant vars then (
            L.smt (fun m -> m "It's relevant!");
            add_vars vars;
            Dynarray.add_last to_encode value;
            aux_checked others rest)
          else
            let () = L.smt (fun m -> m "It's not relevant, skipping") in
            let others = fun () -> Seq.Cons (slot, others) in
            aux_checked others rest
    in
    let rec aux seq =
      match seq () with
      | Seq.Nil -> ()
      | Cons ({ value; checked = false }, rest) ->
          L.smt (fun m -> m "unchecked: %a" Typed.ppa value);
          Dynarray.add_last to_encode value;
          add_vars_raw (vars value);
          aux rest
      | Cons ({ checked = true; _ }, rest) as c ->
          let seq () = c in
          aux_checked Seq.empty seq
    in
    let () = aux (to_seq_rev t) in
    (to_encode, var_set)
end

module Declared_vars = struct
  type t = {
    counter : Var.Incr_counter_mut.t;
    types : Svalue.ty Dynarray.t;
        (** The var_counter is keeping track of how many variables we actually
            have in the context. We don't need to separate each bit of that
            array by saved branch, we just fetch at the index for each variable,
            and override when we change branch. *)
  }

  let init () =
    { counter = Var.Incr_counter_mut.init (); types = Dynarray.create () }

  let save t = Var.Incr_counter_mut.save t.counter
  let backtrack_n t n = Var.Incr_counter_mut.backtrack_n t.counter n
  let reset t = Var.Incr_counter_mut.reset t.counter

  let fresh t ty =
    let next = Var.Incr_counter_mut.get_next t.counter in
    let next_i = Var.to_int next in
    let () =
      if Dynarray.length t.types == next_i then Dynarray.add_last t.types ty
      else if Dynarray.length t.types > next_i then
        Dynarray.set t.types next_i ty
      else failwith "Broke var-counter/declared-types invariant"
    in
    next

  let get_ty t var = Dynarray.get t.types (Var.to_int var)
end

type t = {
  z3_solver : Simple_smt.solver;
  vars : Declared_vars.t;
  save_counter : Save_counter.t;
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
  (* Before every check-sat we pop then push again. *)
  ack_command z3_solver (Simple_smt.push 2);
  {
    z3_solver;
    save_counter = Save_counter.init ();
    vars = Declared_vars.init ();
    state = Solver_state.init ();
  }

let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let is_constr constr = list [ atom "_"; atom "is"; atom constr ]
let ack_command solver sexp = ack_command solver sexp
let check solver = check solver
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
  Declared_vars.save solver.vars;
  Save_counter.save solver.save_counter;
  Solver_state.save solver.state

let backtrack_n solver n =
  Declared_vars.backtrack_n solver.vars n;
  Solver_state.backtrack_n solver.state n;
  Save_counter.backtrack_n solver.save_counter n

(* Initialise and reset *)

let reset solver =
  (* We want to go back to 1, meaning after the first push which saved the declarations *)
  let save_counter = !(solver.save_counter) in
  if save_counter < 0 then failwith "Solver reset: save_counter < 0???";
  Save_counter.reset solver.save_counter;
  Declared_vars.reset solver.vars;
  Solver_state.reset solver.state

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
  declare v (sort_of_ty ty)

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

let fresh_var solver ty = Declared_vars.fresh solver.vars (Typed.untype_type ty)

let rec simplify' solver (v : Svalue.t) : Svalue.t =
  match v.node.kind with
  | Int _ | Bool _ | BitVec _ | Float _ -> v
  | _ -> (
      match Solver_state.trivial_truthiness_of solver.state (Typed.type_ v) with
      | Some true -> Svalue.v_true
      | Some false -> Svalue.v_false
      | None -> (
          match v.node.kind with
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
          | _ -> v))

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
  Solver_state.add_constraint solver.state v

let as_bool = Typed.as_bool

let memo_sat_check_tbl : Simple_smt.result Hashtbl.Hint.t =
  Hashtbl.Hint.create 1023

let check_sat_raw solver relevant_vars to_check =
  (* TODO: we shouldn't wait for ack for each command individually... *)
  ack_command solver.z3_solver (Simple_smt.pop 1);
  ack_command solver.z3_solver (Simple_smt.push 1);
  (* Declare all relevant variables *)
  Var.Hashset.iter
    (fun v ->
      let ty = Declared_vars.get_ty solver.vars v in
      ack_command solver.z3_solver (declare_v v ty))
    relevant_vars;
  (* Declare the constraint *)
  let expr = encode_value_memo to_check in
  ack_command solver.z3_solver (Simple_smt.assume expr);
  (* Actually check sat *)
  try check solver.z3_solver
  with Simple_smt.UnexpectedSolverResponse s ->
    L.error (fun m ->
        m "Unexpected solver response: %s" (Sexplib.Sexp.to_string_hum s));
    Unknown

let check_sat_raw_memo solver relevant_vars to_check =
  let to_check = Typed.untyped to_check in
  match Hashtbl.Hint.find_opt memo_sat_check_tbl to_check.Hashcons.tag with
  | Some result -> result
  | None ->
      let result = check_sat_raw solver relevant_vars to_check in
      Hashtbl.Hint.add memo_sat_check_tbl to_check.Hashcons.tag result;
      result

(* TODO: Add query caching! *)
let sat solver =
  match Solver_state.trivial_truthiness solver.state with
  | Some true -> Soteria_symex.Solver.Sat
  | Some false -> Unsat
  | None -> (
      let to_check, relevant_vars =
        Solver_state.unchecked_constraints solver.state
      in
      (* This will put the check in a somewhat-normal form, to increase cache hits. *)
      let to_check = Dynarray.fold_left Typed.and_ Typed.v_true to_check in
      let answer = check_sat_raw_memo solver relevant_vars to_check in
      match answer with
      | Sat ->
          Solver_state.mark_checked solver.state;
          Sat
      | Unsat -> Unsat
      | Unknown ->
          L.warn (fun m -> m "Solver returned unknown");
          Unknown)

let as_values solver = Solver_state.to_value_list solver.state
