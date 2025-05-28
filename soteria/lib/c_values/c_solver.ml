module Value = Typed
module Var = Svalue.Var
module L = Soteria_logs.Logs.L

let debug_str ~prefix s = L.smt (fun m -> m "%s: %s" prefix s)

open Simple_smt

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
    | Cons ({ checked = true; _ }, _) ->
        Some true (* All constraints have been checked to be sat *)
    | Cons ({ value; _ }, _) when Typed.(equal value v_false) -> Some false
    | _ -> None

  (* We check if the thing contains the value itself, or its negation. *)
  let trivial_truthiness_of (t : t) (v : Typed.sbool Typed.t) =
    let neg_v = Typed.not v in
    Dynarray.find_map
      (fun d ->
        Dynarray.find_map
          (fun { value; _ } ->
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
    let relevant vars = Iter.exists (Var.Hashset.mem var_set) vars in
    (* We need to reach some kind of fixpoint *)
    let rec aux_checked others seq =
      match seq () with
      | Seq.Nil ->
          if !changed then (
            changed := false;
            aux_checked Seq.empty others)
          else ()
      | Seq.Cons (({ value; _ } as slot), rest) ->
          let vars = vars value in
          if relevant vars then (
            add_vars vars;
            Dynarray.add_last to_encode value;
            aux_checked others rest)
          else
            let others = fun () -> Seq.Cons (slot, others) in
            aux_checked others rest
    in
    let rec aux seq =
      match seq () with
      | Seq.Nil -> ()
      | Cons ({ value; checked = false }, rest) ->
          Dynarray.add_last to_encode value;
          add_vars_raw (vars value);
          aux rest
      | Cons ({ checked = true; _ }, _) -> aux_checked Seq.empty seq
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
  solver_exe : Solver_exe.t;
  vars : Declared_vars.t;
  save_counter : Save_counter.t;
  state : Solver_state.t;
}

let init () =
  let solver_exe = Solver_exe.create () in
  Solver_exe.execute_init solver_exe;
  (* Before every check-sat we pop then push again. *)
  ack_command solver_exe (Simple_smt.push 2);
  {
    solver_exe;
    save_counter = Save_counter.init ();
    vars = Declared_vars.init ();
    state = Solver_state.init ();
  }

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

let declare_v v_id ty =
  let v = Svalue.Var.to_string v_id in
  declare v (Smtlib_encoding.sort_of_ty ty)

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
  ack_command solver.solver_exe (Simple_smt.pop 1);
  ack_command solver.solver_exe (Simple_smt.push 1);
  (* Declare all relevant variables *)
  Var.Hashset.iter
    (fun v ->
      let ty = Declared_vars.get_ty solver.vars v in
      ack_command solver.solver_exe (declare_v v ty))
    relevant_vars;
  (* Declare the constraint *)
  let expr = Smtlib_encoding.encode_value to_check in
  ack_command solver.solver_exe (Simple_smt.assume expr);
  (* Actually check sat *)
  try check solver.solver_exe
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
