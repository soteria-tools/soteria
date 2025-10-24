open Soteria_std
open Logs.Import
module Var_id = Svalue.Var_id

module Make_incremental
    (Analysis : Analyses.S)
    (Intf :
      Solvers.Solver_interface.S
        with type value = Svalue.t
         and type ty = Svalue.ty) =
struct
  module Value = Typed

  module Var_counter = Var_id.Incr_counter_mut (struct
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

    (** This function returns [Some b] if the solver state is trivially [b]
        (true or false). We maintain solver state such that trivial truths are
        never added to the state, and false is false erases everything else.
        Therefore, it is enough to check either for emptyness of the topmost
        layer or falseness of the latest element. *)
    let trivial_truthiness (t : t) =
      match Dynarray.find_last t with
      | None -> Some true
      | Some last -> (
          match Dynarray.find_last last with
          | None -> Some true
          | Some v when Typed.equal v Typed.v_false -> Some false
          | _ -> None)

    let iter (t : t) f = Dynarray.iter (fun t -> Dynarray.iter f t) t

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
    z3_exe : Intf.t;
    save_counter : Save_counter.t;
    var_counter : Var_counter.t;
    state : Solver_state.t;
    analysis : Analysis.t;
  }

  let init () =
    let z3_exe = Intf.init () in
    Intf.push z3_exe 1;
    {
      z3_exe;
      save_counter = Save_counter.init ();
      var_counter = Var_counter.init ();
      state = Solver_state.init ();
      analysis = Analysis.init ();
    }

  let save solver =
    Var_counter.save solver.var_counter;
    Save_counter.save solver.save_counter;
    Solver_state.save solver.state;
    Analysis.save solver.analysis;
    Intf.push solver.z3_exe 1

  let backtrack_n solver n =
    Var_counter.backtrack_n solver.var_counter n;
    Solver_state.backtrack_n solver.state n;
    Save_counter.backtrack_n solver.save_counter n;
    Analysis.backtrack_n solver.analysis n;
    Intf.pop solver.z3_exe n

  (* Initialise and reset *)

  let reset solver =
    (* We want to go back to 1, meaning after the first push which saved the declarations *)
    let save_counter = !(solver.save_counter) in
    if save_counter < 0 then failwith "Solver reset: save_counter < 0???";
    Save_counter.reset solver.save_counter;
    Var_counter.reset solver.var_counter;
    Solver_state.reset solver.state;
    Analysis.reset solver.analysis;
    (* We need to pop the initial push, so we go back to the state before the first push *)
    Intf.pop solver.z3_exe (save_counter + 1);
    (* Make sure the basic definitions are saved again *)
    Intf.pop solver.z3_exe 1

  let fresh_var solver ty =
    let v_id = Var_counter.get_next solver.var_counter in
    Intf.declare_var solver.z3_exe v_id (Typed.untype_type ty);
    v_id

  (* We should factor simplifications out also... *)
  let rec simplify' solver (v : Svalue.t) : Svalue.t =
    match v.node.kind with
    | Int _ | Bool _ | BitVec _ | Float _ -> v
    | _ -> (
        match
          Solver_state.trivial_truthiness_of solver.state (Typed.type_ v)
        with
        | Some true -> Svalue.v_true
        | Some false -> Svalue.v_false
        | None -> (
            match v.node.kind with
            | Unop (Not, e) ->
                let e' = simplify' solver e in
                if Svalue.equal e e' then v else Svalue.not e'
            | Binop (Eq, e1, e2) ->
                if Svalue.equal e1 e2 && (not @@ Svalue.is_float e1.node.ty)
                then Svalue.v_true
                else if Svalue.sure_neq e1 e2 then Svalue.v_false
                else v
            | Binop (Or, e1, e2) ->
                let se1 = simplify' solver e1 in
                let se2 = simplify' solver e2 in
                if Svalue.equal se1 e1 && Svalue.equal se2 e2 then v
                else Svalue.or_ se1 se2
            | _ -> Analysis.simplify solver.analysis v))

  and simplify solver (v : 'a Typed.t) : 'a Typed.t =
    v |> Typed.untyped |> simplify' solver |> Typed.type_

  let add_constraints solver ?(simplified = false) vs =
    let iter = vs |> Iter.of_list |> Iter.flat_map Typed.split_ands in
    iter @@ fun v ->
    let v = if simplified then v else simplify solver v in
    (* the incremental solver doesn't need to dirty variables *)
    let v, _ = Analysis.add_constraint solver.analysis (Typed.untyped v) in
    Solver_state.add_constraint solver.state (Typed.type_ v);
    Intf.add_constraint solver.z3_exe v

  (* Incremental doesn't allow for caching queries... *)
  let check_sat solver =
    match Solver_state.trivial_truthiness solver.state with
    | Some true -> Symex.Solver_result.Sat
    | Some false -> Unsat
    | None -> (
        let answer = Intf.check_sat solver.z3_exe in
        match answer with
        | Sat -> Sat
        | Unsat -> Unsat
        | Unknown ->
            L.info (fun m -> m "Solver returned unknown");
            Unknown)

  let as_values solver =
    Iter.append
      (Solver_state.iter solver.state)
      (Analysis.encode solver.analysis)
    |> Iter.to_list
end

module Make
    (Analysis : Analyses.S)
    (Intf :
      Solvers.Solver_interface.S
        with type value = Svalue.t
         and type ty = Svalue.ty) =
struct
  module Value = Typed

  module Var_counter = Var_id.Incr_counter_mut (struct
    let start_at = 1
  end)

  module Solver_state = struct
    (** Inside a slot, we either have an assertion, or a marker indicating that
        all assertions relating to a variable may need to be rechecked -- for
        instance because an auxiliary analysis has new information about it that
        is not directly in the PC. *)
    type slot_content =
      | Asrt of Typed.sbool Typed.t [@printer Typed.ppa]
      | Dirty of Var_id.Set.t
          [@printer Fmt.(iter ~sep:comma) Var_id.Set.iter Var_id.pp]
    [@@deriving show]

    (** Each slot holds a symbolic boolean, as well a boolean indicating if it
        was checked to be satisfiable. The boolean is mutable and can be mutated
        even by future branches! If a branch downstream is satisfiable, then so
        is any element on the path condition. *)
    type slot = { value : slot_content; mutable checked : bool }
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
              Dynarray.add_last last
                { value = Asrt Typed.v_false; checked = false })
            else Dynarray.add_last last { value = Asrt v; checked = false }

    let dirty_variable (t : t) v =
      match Dynarray.find_last t with
      | None -> failwith "dirty_variable: empty array"
      | Some last -> Dynarray.add_last last { value = Dirty v; checked = false }

    let to_seq_rev (t : t) =
      Seq.concat_map Dynarray.to_seq_rev (Dynarray.to_seq_rev t)

    (** This function returns [Some b] if the solver state is trivially [b]
        (true or false). We maintain solver state such that trivial truths are
        never added to the state, and false is false erases everything else.
        Therefore, it is enough to check either for emptyness of the topmost
        layer or falseness of the latest element. *)
    let trivial_truthiness (t : t) =
      match to_seq_rev t () with
      | Nil -> Some true (* The empty constraint is satisfiable *)
      | Cons ({ checked = true; _ }, _) ->
          Some true (* All constraints have been checked to be sat *)
      | Cons ({ value = Asrt value; _ }, _) when Typed.(equal value v_false) ->
          Some false
      | _ -> None

    (* We check if the thing contains the value itself, or its negation. *)
    let trivial_truthiness_of (t : t) (v : Typed.sbool Typed.t) =
      let neg_v = Typed.not v in
      Dynarray.find_map
        (Dynarray.find_map (function
          | { value = Asrt value; _ } ->
              if Typed.equal value v then Some true
              else if Typed.equal value neg_v then Some false
              else None
          | _ -> None))
        t

    (** Iterate over the assertions in the PC. *)
    let iter (t : t) f =
      Dynarray.iter
        (Dynarray.iter (function
          | { value = Asrt value; _ } -> f value
          | { value = Dirty _; _ } -> ()))
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
        variables contained by the unchecked assertions, and fetch, All
        assertions (even checked) that also contain these variables. In the end,
        we need to fetch the closure from these variables.

        The function returns the list to encode, as well the set of all
        variables required. *)
    let unchecked_constraints t =
      let changed = ref false in
      let var_set = Var_id.Hashset.with_capacity 8 in
      let vars value = Value.iter_vars value |> Iter.map fst in
      let to_encode = Dynarray.create () in
      let add_vars_raw vars = Var_id.Hashset.add_iter var_set vars in
      let add_vars vars =
        vars @@ fun v ->
        let prev_size = Var_id.Hashset.cardinal var_set in
        Var_id.Hashset.add var_set v;
        if Var_id.Hashset.cardinal var_set <> prev_size then changed := true
      in
      let relevant = Iter.exists (Var_id.Hashset.mem var_set) in
      (* We need to reach some kind of fixpoint *)
      let rec aux_checked others seq =
        match seq () with
        | Seq.Nil ->
            if !changed then (
              changed := false;
              aux_checked Seq.empty others)
            else ()
        | Seq.Cons (({ value = Asrt value; _ } as slot), rest) ->
            let vars = vars value in
            if relevant vars then (
              add_vars vars;
              Dynarray.add_last to_encode value;
              aux_checked others rest)
            else
              let others = fun () -> Seq.Cons (slot, others) in
              aux_checked others rest
        | Seq.Cons ({ value = Dirty _; _ }, rest) ->
            (* A dirty checked variable can be ignored *)
            aux_checked others rest
      in
      let rec aux seq =
        match seq () with
        | Seq.Nil -> ()
        | Cons ({ value = Asrt value; checked = false }, rest) ->
            Dynarray.add_last to_encode value;
            add_vars_raw (vars value);
            aux rest
        | Cons ({ value = Dirty vars; checked = false }, rest) ->
            add_vars_raw (fun f -> Var_id.Set.iter f vars);
            aux rest
        | Cons ({ checked = true; _ }, _) -> aux_checked Seq.empty seq
      in
      let () = aux (to_seq_rev t) in
      (to_encode, var_set)
  end

  module Declared_vars = struct
    module Var_counter = Var_id.Incr_counter_mut (struct
      let start_at = 1
    end)

    (* Since we start addresses at one to improve trivial model hits, we need to offset to obtain an index. *)
    let var_to_index v = Var_id.to_int v - 1

    type t = {
      counter : Var_counter.t;
      types : Svalue.ty Dynarray.t;
          (** The var_counter is keeping track of how many variables we actually
              have in the context. We don't need to separate each bit of that
              array by saved branch, we just fetch at the index for each
              variable, and override when we change branch. *)
    }

    let init () = { counter = Var_counter.init (); types = Dynarray.create () }
    let save t = Var_counter.save t.counter
    let backtrack_n t n = Var_counter.backtrack_n t.counter n
    let reset t = Var_counter.reset t.counter

    let fresh t ty =
      let next = Var_counter.get_next t.counter in
      let next_i = var_to_index next in
      let () =
        if Dynarray.length t.types == next_i then Dynarray.add_last t.types ty
        else if Dynarray.length t.types > next_i then
          Dynarray.set t.types next_i ty
        else failwith "Broke var-counter/declared-types invariant"
      in
      next

    let get_ty t var = Dynarray.get t.types (var_to_index var)
  end

  type t = {
    z3_exe : Intf.t;
    vars : Declared_vars.t;
    save_counter : Save_counter.t;
    state : Solver_state.t;
    analysis : Analysis.t;
  }

  let init () =
    let z3_exe = Intf.init () in
    (* Before every check-sat we pop then push again. *)
    {
      z3_exe;
      save_counter = Save_counter.init ();
      vars = Declared_vars.init ();
      state = Solver_state.init ();
      analysis = Analysis.init ();
    }

  let save solver =
    Declared_vars.save solver.vars;
    Save_counter.save solver.save_counter;
    Solver_state.save solver.state;
    Analysis.save solver.analysis

  let backtrack_n solver n =
    Declared_vars.backtrack_n solver.vars n;
    Solver_state.backtrack_n solver.state n;
    Save_counter.backtrack_n solver.save_counter n;
    Analysis.backtrack_n solver.analysis n

  (* Initialise and reset *)

  let reset solver =
    (* We want to go back to 1, meaning after the first push which saved the declarations *)
    let save_counter = !(solver.save_counter) in
    if save_counter < 0 then failwith "Solver reset: save_counter < 0???";
    Save_counter.reset solver.save_counter;
    Declared_vars.reset solver.vars;
    Solver_state.reset solver.state;
    Analysis.reset solver.analysis

  let fresh_var solver ty =
    Declared_vars.fresh solver.vars (Typed.untype_type ty)

  let[@inline] as_untyped f v = v |> Typed.untyped |> f |> Typed.type_

  let rec simplify' solver (v : Svalue.t) : Svalue.t =
    match v.node.kind with
    | Int _ | Bool _ | BitVec _ | Float _ -> v
    | _ -> (
        match
          Solver_state.trivial_truthiness_of solver.state (Typed.type_ v)
        with
        | Some true -> Svalue.v_true
        | Some false -> Svalue.v_false
        | None -> (
            match v.node.kind with
            | Unop (Not, e) ->
                let e' = simplify' solver e in
                if Svalue.equal e e' then v else Svalue.not e'
            | Binop (Eq, e1, e2) ->
                if Svalue.equal e1 e2 && (not @@ Svalue.is_float e1.node.ty)
                then Svalue.v_true
                else if Svalue.sure_neq e1 e2 then Svalue.v_false
                else v
            | Binop (And, e1, e2) ->
                let se1 = simplify' solver e1 in
                let se2 = simplify' solver e2 in
                if Svalue.equal se1 e1 && Svalue.equal se2 e2 then v
                else Svalue.and_ se1 se2
            | Binop (Or, e1, e2) ->
                let se1 = simplify' solver e1 in
                let se2 = simplify' solver e2 in
                if Svalue.equal se1 e1 && Svalue.equal se2 e2 then v
                else Svalue.or_ se1 se2
            | Ite (g, e1, e2) ->
                let sg = simplify' solver g in
                let se1 = simplify' solver e1 in
                let se2 = simplify' solver e2 in
                if
                  Svalue.equal sg g
                  && Svalue.equal se1 e1
                  && Svalue.equal se2 e2
                then v
                else Svalue.ite sg se1 se2
            | _ -> Analysis.simplify solver.analysis v))

  and simplify solver : 'a Typed.t -> 'a Typed.t = as_untyped (simplify' solver)

  let add_constraints solver ?(simplified = false) vs =
    let iter = vs |> Iter.of_list |> Iter.flat_map Typed.split_ands in
    iter @@ fun v ->
    let v = if simplified then v else simplify solver v in
    let v, vars = Analysis.add_constraint solver.analysis (Typed.untyped v) in
    Solver_state.add_constraint solver.state (Typed.type_ v);
    if not (Var_id.Set.is_empty vars) then
      Solver_state.dirty_variable solver.state vars

  let memo_sat_check_tbl : Symex.Solver_result.t Hashtbl.Hint.t =
    Hashtbl.Hint.create 1023

  let trivial_model_works to_check =
    (* We try a trivial model where replacing each variable with name
    [|n|] with the corresponding integer [n]; except if an assertion
    [|n| == v] exists, in which case we replace it with the value [v].
    If the constraint evaluates to true, then it is satisfiable. *)
    let v_eqs = Var_id.Hashtbl.create 8 in
    Svalue.split_ands to_check (fun v ->
        match v.node.kind with
        | Binop
            ( Eq,
              { node = { kind = Var n; _ }; _ },
              ({ node = { kind = Int _; _ }; _ } as x) )
        | Binop
            ( Eq,
              ({ node = { kind = Int _; _ }; _ } as x),
              { node = { kind = Var n; _ }; _ } ) ->
            Var_id.Hashtbl.add v_eqs n x
        | _ -> ());
    let eval_var v ty =
      match ty with
      | Svalue.TInt | Svalue.TLoc -> (
          let i = Var_id.to_int v in
          try Some (Var_id.Hashtbl.find v_eqs v)
          with Not_found -> Some (Svalue.int i))
      | _ -> None
    in
    let res = Eval.eval ~eval_var to_check in
    match res with Some v -> Svalue.equal v Svalue.v_true | _ -> false

  let check_sat_raw solver relevant_vars to_check =
    (* TODO: we shouldn't wait for ack for each command individually... *)
    if trivial_model_works to_check then Symex.Solver_result.Sat
    else (
      (* We need to reset the state, so we can push the new constraints *)
      Intf.reset solver.z3_exe;

      (* Declare all relevant variables *)
      Var_id.Hashset.iter
        (fun v ->
          let ty = Declared_vars.get_ty solver.vars v in
          Intf.declare_var solver.z3_exe v ty)
        relevant_vars;
      (* Declare the constraint *)
      Intf.add_constraint solver.z3_exe to_check;
      (* Actually check sat *)
      Intf.check_sat solver.z3_exe)

  let check_sat_raw_memo solver relevant_vars to_check =
    let to_check = Typed.untyped to_check in
    match Hashtbl.Hint.find_opt memo_sat_check_tbl to_check.Hc.tag with
    | Some result -> result
    | None ->
        let result = check_sat_raw solver relevant_vars to_check in
        Hashtbl.Hint.add memo_sat_check_tbl to_check.Hc.tag result;
        result

  let check_sat solver : Symex.Solver_result.t =
    match Solver_state.trivial_truthiness solver.state with
    | Some true -> Sat
    | Some false -> Unsat
    | None ->
        let to_check, relevant_vars =
          Solver_state.unchecked_constraints solver.state
        in
        (* This will put the check in a somewhat-normal form, to increase cache hits. *)
        let to_check = Dynarray.fold_left Typed.and_ Typed.v_true to_check in
        let to_check =
          Iter.fold Typed.and_ to_check
            (Analysis.encode ~vars:relevant_vars solver.analysis)
        in
        let answer = check_sat_raw_memo solver relevant_vars to_check in
        if answer = Sat then Solver_state.mark_checked solver.state;
        answer

  let as_values solver =
    Iter.append
      (Solver_state.iter solver.state)
      (Analysis.encode solver.analysis)
    |> Iter.to_list
end

module Z3 = Solvers.Z3.Make (Encoding)
module Z3_incremental_solver = Make_incremental (Analyses.Interval) (Z3)
module Z3_solver = Make (Analyses.Interval) (Z3)
