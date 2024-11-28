module Value = Svalue

let z3_env_var = "BFA_Z3_PATH"
let log_src = Logs.Src.create "bfa_c.SOLVER"
let debug_str ~prefix s = Logs.debug ~src:log_src (fun m -> m "%s: %s" prefix s)

module Dist_set = struct
  (* Pairs of distinct expressions *)

  module Hashtbl = Hashtbl.Make (struct
    type t = Svalue.t * Svalue.t

    let equal (a1, b1) (a2, b2) = Svalue.equal a1 a2 && Svalue.equal b1 b2

    let hash (a, b) =
      let a, b = (a.Hashcons.tag, b.Hashcons.tag) in
      ((a land ((1 lsl 31) - 1)) lsl 31) lor b
  end)

  type t = unit Hashtbl.t Dynarray.t

  let pp ft t =
    let first = ref true in
    Dynarray.iter
      (fun h ->
        Hashtbl.iter
          (fun (i, j) _ ->
            if !first then first := false else Fmt.pf ft "; ";
            Fmt.pf ft "(%d != %d)" i.Hashcons.tag j.Hashcons.tag)
          h)
      t

  let create () =
    let r = Dynarray.create () in
    Dynarray.ensure_capacity r 1023;
    Dynarray.add_last r (Hashtbl.create 0);
    r

  let reset (ds : t) =
    Dynarray.clear ds;
    Dynarray.add_last ds (Hashtbl.create 0)

  let sure_are_diff (ds : t) (i : Svalue.t) (j : Svalue.t) =
    let rec find idx pair =
      if idx < 0 then false
      else
        let h = Dynarray.get ds idx in
        if Hashtbl.mem h pair then true else find (idx - 1) pair
    in
    let tagi = i.Hashcons.tag in
    let tagj = j.Hashcons.tag in
    if tagi = tagj then false
    else
      let pair = if tagi < tagj then (i, j) else (j, i) in
      find (Dynarray.length ds - 1) pair

  let add (ds : t) i j =
    let pair = if i.Hashcons.tag < j.Hashcons.tag then (i, j) else (j, i) in
    let h = Dynarray.get_last ds in
    Hashtbl.add h pair ()

  let save (ds : t) = Dynarray.add_last ds (Hashtbl.create 0)
  let backtrack_n (ds : t) n = Dynarray.truncate ds (Dynarray.length ds - n)
end

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

let z3_solver () =
  let solver = new_solver solver_config in
  let command sexp =
    log_sexp sexp;
    solver.command sexp
  in
  { solver with command }

module Solver_state = struct
  type t = Svalue.t Dynarray.t list

  let empty = []
  let to_value_list (t : t) = List.concat_map Dynarray.to_list t
  let save t = Dynarray.create () :: t

  let rec backtrack_n l n =
    if n <= 0 then l
    else
      match l with
      | [] -> failwith "backtrack_n: empty list"
      | _ :: tl -> backtrack_n tl (n - 1)

  let add_constraint t v =
    if Svalue.equal v Svalue.v_true then ()
    else
      match t with
      | [] -> failwith "add_constraint: empty list"
      | hd :: _ ->
          if Svalue.equal v Svalue.v_false then (
            Dynarray.clear hd;
            Dynarray.add_last hd Svalue.v_false)
          else Dynarray.add_last hd v

  (** This function returns [Some b] if the solver state is trivially [b] (true or false).
      We maintain solver state such that trivial truths are never added to the state, and false is false erases everything else.
      Therefore, it is enough to check either for emptyness of the topmost layer or falseness of the latest element.
    *)
  let trivial_truthiness (t : t) =
    match t with
    | [] -> Some true
    | hd :: _ -> (
        match Dynarray.find_last hd with
        | None -> Some true
        | Some v when Svalue.equal v Svalue.v_false -> Some false
        | _ -> None)

  let iter (t : t) =
    Iter.flat_map (fun d f -> Dynarray.iter f d) (Iter.of_list t)
end

type t = {
  z3_solver : Simple_smt.solver;
  mutable save_counter : int;
  mutable var_stack : int Dynarray.t;
  dist_set : Dist_set.t;
  mutable state : Solver_state.t;
}

let init () =
  let var_stack = Dynarray.create () in
  Dynarray.add_last var_stack 0;
  {
    z3_solver = z3_solver ();
    save_counter = 0;
    dist_set = Dist_set.create ();
    var_stack;
    state = Solver_state.empty;
  }

let solver = lazy (init ())
let ( $$ ) = app
let ( $ ) f v = f $$ [ v ]
let is_constr constr = list [ atom "_"; atom "is"; atom constr ]

let ack_command sexp =
  let (lazy solver) = solver in
  ack_command solver.z3_solver sexp

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
  Initialize_analysis.register_once_initialiser (fun () -> ack_command cmd);
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
  Initialize_analysis.register_once_initialiser (fun () -> ack_command cmd);
  ( atom opt,
    (fun v -> atom mk_some $ v),
    (fun v -> atom opt_unwrap $ v),
    atom none,
    (fun v -> is_constr mk_some $ v),
    fun v -> is_constr none $ v )

(********* End of solver declarations *********)

let save () =
  let (lazy solver) = solver in
  Dynarray.add_last solver.var_stack (Dynarray.get_last solver.var_stack);
  solver.save_counter <- solver.save_counter + 1;
  solver.state <- Solver_state.save solver.state;
  Dist_set.save solver.dist_set;
  ack_command (Simple_smt.push 1)

let backtrack_n n =
  let (lazy solver) = solver in
  Dist_set.backtrack_n solver.dist_set n;
  solver.state <- Solver_state.backtrack_n solver.state n;
  solver.save_counter <- solver.save_counter - n;
  Dynarray.truncate solver.var_stack (solver.save_counter + 1);
  ack_command (Simple_smt.pop n)

let backtrack () = backtrack_n 1

(* Initialise and reset *)

let () = Initialize_analysis.register_once_initialiser (fun () -> save ())

let reset () =
  let (lazy solver) = solver in
  (* We want to go back to 1, meaning after the first push which saved the declarations *)
  L.debug (fun m -> m "Resetting solver: %d" solver.save_counter);
  if solver.save_counter > 0 then backtrack_n solver.save_counter;
  save ()

let () = Initialize_analysis.register_resetter reset

let rec sort_of_ty = function
  | Value.TBool -> Simple_smt.t_bool
  | TInt -> Simple_smt.t_int
  | TLoc -> Simple_smt.t_int
  | TSeq ty -> t_seq $ sort_of_ty ty
  | TPointer -> t_ptr
  | TOption ty -> t_opt $ sort_of_ty ty

let declare_v v_id ty =
  let v = Value.Var_name.to_string v_id in
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
      | Or -> bool_or v1 v2
      | Plus -> num_add v1 v2
      | Minus -> num_sub v1 v2
      | Times -> num_mul v1 v2
      | Div -> num_div v1 v2)

and encode_value_memo v = (memoz memo_encode_value_tbl encode_value) v

let fresh ty =
  let (lazy solver) = solver in
  let v_id = Dynarray.pop_last solver.var_stack in
  Dynarray.add_last solver.var_stack (v_id + 1);
  let c = declare_v v_id ty in
  ack_command c;
  Svalue.mk_var v_id ty

let rec simplify (v : Svalue.t) =
  let (lazy solver) = solver in
  match v.node.kind with
  | Int _ | Bool _ -> v
  | Unop (Not, e) ->
      let e' = simplify e in
      if Svalue.equal e e' then v else Svalue.not e'
  | Binop (Eq, e1, e2) ->
      if Svalue.equal e1 e2 then Svalue.v_true
      else if Dist_set.sure_are_diff solver.dist_set e1 e2 then Svalue.v_false
      else v
  | Binop (Or, e1, e2) ->
      let se1 = simplify e1 in
      let se2 = simplify e2 in
      if Svalue.equal se1 e1 && Svalue.equal se2 e2 then v
      else Svalue.or_ se1 se2
  | _ -> v

let is_diff_op (v : Svalue.t) =
  match v.node.kind with
  | Unop (Not, { node = { kind = Binop (Eq, v1, v2); _ }; _ }) -> Some (v1, v2)
  | _ -> None

let add_constraints ?(simplified = false) vs =
  let (lazy solver) = solver in
  let iter = vs |> Iter.of_list |> Iter.flat_map Svalue.split_ands in
  iter @@ fun v ->
  let v = if simplified then v else simplify v in
  let () =
    match is_diff_op v with
    | None -> ()
    | Some (i, j) -> Dist_set.add solver.dist_set i j
  in
  Solver_state.add_constraint solver.state v

let as_bool v =
  if Svalue.equal v Svalue.v_true then Some true
  else if Svalue.equal v Svalue.v_false then Some false
  else None

(* Incremental doesn't allow for caching queries... *)
let sat () =
  let (lazy solver) = solver in
  match Solver_state.trivial_truthiness solver.state with
  | Some true -> true
  | Some false -> false
  | None -> (
      let () = ack_command (Simple_smt.push 1) in
      Solver_state.iter solver.state (fun v ->
          ack_command (assume (encode_value v)));
      let answer =
        try check solver.z3_solver
        with Simple_smt.UnexpectedSolverResponse s ->
          Logs.err ~src:log_src (fun m ->
              m "Unexpected solver response: %s" (Sexplib.Sexp.to_string_hum s));
          Unknown
      in
      ack_command (Simple_smt.pop 1);
      match answer with
      | Sat -> true
      | Unsat -> false
      | Unknown ->
          Logs.info ~src:log_src (fun m -> m "Solver returned unknown");
          (* We return UNSAT by default: under-approximating behaviour *)
          false)

let get_pc () =
  let (lazy solver) = solver in
  Solver_state.to_value_list solver.state
