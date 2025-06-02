module Value = Typed
module Var = Svalue.Var
module L = Soteria_logs.Logs.L
open Simple_smt

module Var_counter = Var.Incr_counter_mut (struct
  let start_at = 0
end)

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

  let trivial_truthiness_of (t : t) (v : Typed.sbool Typed.t) =
    let neg_v = Typed.not v in
    Dynarray.find_map
      (Dynarray.find_map (fun value ->
           if Typed.equal value v then Some true
           else if Typed.equal value neg_v then Some false
           else None))
      t
end

type t = {
  solver_exe : Solver_exe.t;
  save_counter : Save_counter.t;
  var_counter : Var_counter.t;
  state : Solver_state.t;
}

let init () =
  let solver_exe = Solver_exe.create () in
  Solver_exe.execute_init solver_exe;
  ack_command solver_exe (Simple_smt.push 1);
  {
    solver_exe;
    save_counter = Save_counter.init ();
    var_counter = Var_counter.init ();
    state = Solver_state.init ();
  }

let save solver =
  Var_counter.save solver.var_counter;
  Save_counter.save solver.save_counter;
  Solver_state.save solver.state;
  ack_command solver.solver_exe (Simple_smt.push 1)

let backtrack_n solver n =
  Var_counter.backtrack_n solver.var_counter n;
  Solver_state.backtrack_n solver.state n;
  Save_counter.backtrack_n solver.save_counter n;
  ack_command solver.solver_exe (Simple_smt.pop n)

(* Initialise and reset *)

let reset solver =
  (* We want to go back to 1, meaning after the first push which saved the declarations *)
  let save_counter = !(solver.save_counter) in
  if save_counter < 0 then failwith "Solver reset: save_counter < 0???";
  Save_counter.reset solver.save_counter;
  Var_counter.reset solver.var_counter;
  Solver_state.reset solver.state;
  ack_command solver.solver_exe (Simple_smt.pop (save_counter + 1));
  (* Make sure the basic definitions are saved again *)
  ack_command solver.solver_exe (Simple_smt.push 1)

let declare_v v_id ty =
  let v = Svalue.Var.to_string v_id in
  declare v (Smtlib_encoding.sort_of_ty (Typed.untype_type ty))

let fresh_var solver ty =
  let v_id = Var_counter.get_next solver.var_counter in
  let c = declare_v v_id ty in
  ack_command solver.solver_exe c;
  v_id

(* We should factor simplifications out also... *)
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

let add_constraints solver ?(simplified = false) vs =
  let iter = vs |> Iter.of_list |> Iter.flat_map Typed.split_ands in
  iter @@ fun v ->
  let v = if simplified then v else simplify solver v in
  Solver_state.add_constraint solver.state v;
  ack_command solver.solver_exe
  @@ assume
  @@ Smtlib_encoding.encode_value
  @@ Typed.untyped v

let as_bool = Typed.as_bool

(* Incremental doesn't allow for caching queries... *)
let sat solver =
  match Solver_state.trivial_truthiness solver.state with
  | Some true -> Soteria_symex.Solver.Sat
  | Some false -> Unsat
  | None -> (
      let answer =
        try check solver.solver_exe
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
