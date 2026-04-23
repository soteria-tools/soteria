module Config_ = Config
open Soteria_rust_lib
module Config = Config_
module State = State.Tree_state
module Logic = Soteria.Logic.Make (Rustsymex)
module Execute = Logic.Asrt.Execute (State)

let state_to_syn state = State.to_syn @@ State.of_opt state

let state_iter_vars syn =
  let ins, outs = State.ins_outs syn in
  let iter_vars_l l = Iter.of_list l |> Iter.flat_map Svalue.iter_vars in
  let ins, outs = (iter_vars_l ins, iter_vars_l outs) in
  (* In our state representation, locations are single variables *)
  assert (Iter.length ins == 1);
  (Iter.head_exn ins, outs)

module Ret = struct
  type t = State.Sptr.t Rust_val.t
  type syn = State.Sptr.syn Rust_val.syn

  let pp fmt = Rust_val.pp State.Sptr.pp fmt
  let pp_syn fmt = Rust_val.pp_syn State.Sptr.pp_syn fmt
  let to_syn (ret : t) : syn = Rust_val.to_syn State.Sptr.to_syn ret

  let subst subst_var (ret : syn) : t =
    Rust_val.subst State.Sptr.subst subst_var ret

  let iter_vars (ret : syn) f =
    Rust_val.exprs_syn State.Sptr.exprs_syn ret
    |> List.iter (fun sv -> Svalue.iter_vars sv f)
end

type t = { ret : Ret.syn; asrt : State.syn Logic.Asrt.t }

let pp fmt = function
  | { ret; asrt } ->
      Fmt.pf fmt "{\n ret = %a;\n asrt = %a\n}" Ret.pp_syn ret
        (Logic.Asrt.pp State.pp_syn)
        asrt

let make (ret : Ret.t) (st : State.SM.st)
    (pcs : Rustsymex.Value.sbool Typed.t list) : (t, [> `MemoryLeak ]) result =
  let open Result.Syntax in
  let ret = Ret.to_syn ret in
  let+ asrt =
    let spatial = state_to_syn st in
    let module Var_graph = Soteria.Soteria_std.Graph.Make_in_place (Svalue.Var) in
    let module Var_hashset = Var_graph.Node_set in
    let reachable =
      let graph = Var_graph.with_node_capacity 0 in
      (* For each equality [e1 = e2] in the path condition, we add a double edge
         from all variables of [e1] to all variables of [e2] *)
      ListLabels.iter pcs ~f:(fun v ->
          match Typed.kind v with
          | Binop (Eq, el, er) ->
              let module Value = Soteria.Bv_values.Svalue in
              (* We make the second iterator peristent to avoid going over the
                 structure too many times if there are many *)
              let r_iter = Iter.persistent_lazy @@ Value.iter_vars er in
              let product = Iter.product (Value.iter_vars el) r_iter in
              product (fun ((x, _), (y, _)) ->
                  Var_graph.add_double_edge graph x y)
          | _ -> ());
      (* For each block $l -> B in the post state, we add a single-sided arrow
         from the variable in $l to all variables contained in B. *)
      ListLabels.iter spatial ~f:(fun syn ->
          let (var, _), outs = state_iter_vars syn in
          Iter.iter (fun (y, _) -> Var_graph.add_edge graph var y) outs);
      (* We mark all variables from the return value as reachable *)
      let init_reachable = Var_hashset.with_capacity 0 in
      Ret.iter_vars ret (fun (x, _) -> Var_hashset.add init_reachable x);
      (* [init_reachable] is the set of initially-reachable variables, and we
         have a reachability [graph]. We can compute all reachable values. *)
      Var_graph.reachable_from graph init_reachable
    in
    (* We can now filter the summary to keep only the reachable values *)
    let+ spatial =
      let reachable, unreachable =
        ListLabels.partition spatial ~f:(fun syn ->
            let (var, _), _ = state_iter_vars syn in
            Var_hashset.mem reachable var)
      in
      if (Config.get ()).ignore_leaks then Result.ok reachable
      else
        Result.fold_list unreachable ~init:reachable
          ~f:(fun reachable -> function
          | State.Ser_heap
              ( _,
                State.Freeable_block_with_meta.
                  {
                    (* A leak occurs when an unreachable pointer is alive *)
                    node = Soteria.Sym_states.Freeable.Alive _;
                    (* PEDRO: Is this good enough? Do I need info on globals? *)
                    info = Some { kind = Heap; _ };
                    _;
                  } ) ->
              Result.error `MemoryLeak
          | _ -> Result.ok reachable)
    in
    let pure =
      (* TODO: Filter pcs by removing useless inequalities *)
      List.map Typed.untyped pcs
    in
    Logic.Asrt.make ~spatial ~pure
  in
  { ret; asrt }

let produce (summ : t) (st : State.SM.st) :
    (Ret.t * State.SM.st) Rustsymex.Producer.t =
  let open Rustsymex.Producer.Syntax in
  let* ret = Rustsymex.Producer.apply_subst Ret.subst summ.ret in
  let* st = Execute.produce summ.asrt st in
  Rustsymex.Producer.return (ret, st)

let consume (summ : t) (ret : Ret.t) (st : State.SM.st) :
    (State.SM.st, State.syn list) Rustsymex.Consumer.t =
  (* FIXME: This should probably be fixed in the signature of Rustsymex *)
  let module Rustsymex = struct
    include Rustsymex
    module Value = Typed
  end in
  let module Learn_eq = Rust_val.Learn_eq (Rustsymex) in
  let open Rustsymex.Consumer.Syntax in
  let* () = Learn_eq.learn_eq State.Sptr.learn_eq summ.ret ret in
  Execute.consume summ.asrt st

let implies s_pre s_post =
  let process =
    let open Rustsymex.Syntax in
    let subst = Typed.Expr.Subst.empty in
    let* (ret, st), subst =
      Rustsymex.Producer.run ~subst @@ produce s_pre State.empty
    in
    Rustsymex.Consumer.run ~subst @@ consume s_post ret st
  in
  match Rustsymex.run_needs_stats ~mode:OX process with
  | [ (Soteria.Symex.Compo_res.Ok (st, _), _) ] -> st == State.empty
  | _ -> false

module Context = struct
  open Charon.Types
  module M = TypeDeclId.Map

  exception TypeNotSupported

  let type_not_supported (_ : ty) = raise TypeNotSupported

  (* Each custom type has separate lists for visited, unvisited and staged
     summaries *)
  type value = Base of t list | Custom of t list * t list * t list
  type nonrec t = (t list * t list * t list) M.t

  let empty : t = M.empty
  let is_base_ty ty = match ty with TLiteral _ -> true | _ -> false

  let ( let@ ) (ty, default) f =
    match ty with TAdt { id = TAdtId id; _ } -> f id | _ -> default

  let find ty (ctx : t) : value =
    match ty with
    | TAdt { id = TAdtId id; _ } ->
        let visited, unvisited, staged =
          Option.value ~default:([], [], []) (M.find_opt id ctx)
        in
        Custom (visited, unvisited, staged)
    | ty when is_base_ty ty ->
        let nondet ty =
          let process =
            let module Encoder = Value_codec.Encoder (State.Sptr) in
            State.SM.Result.run_with_state ~state:State.empty
              (State.SM.lift @@ Encoder.nondet_valid ty)
          in
          Rustsymex.run_needs_stats ~mode:UX process
          |> ListLabels.map ~f:(function
            | Soteria.Symex.Compo_res.Ok (ret, st), pcs ->
                let ret = Rust_val.to_syn State.Sptr.to_syn ret in
                let spatial = State.to_syn @@ State.of_opt st in
                let pure = List.map Typed.untyped pcs in
                let asrt = Logic.Asrt.make ~spatial ~pure in
                { ret; asrt }
            | _ -> failwith "Expected Ok in nondet")
        in
        Base (nondet ty)
    | ty -> type_not_supported ty

  let iter_summs tys (ctx : t) f =
    let rec aux ?(visited = false) ?(curr = []) ?(acc = []) = function
      | [] ->
          let acc = if visited then acc else curr :: acc in
          let rec aux' ?(acc = []) = function
            | [] -> f acc
            | summs :: rest ->
                List.iter (fun s -> aux' ~acc:(s :: acc) rest) summs
          in
          List.iter aux' acc
      | Base summs :: values -> aux values ~curr:(summs :: curr) ~acc
      | Custom (visited, unvisited, _) :: values ->
          let summs =
            ListLabels.fold_left values ~init:(unvisited :: curr)
              ~f:(fun acc -> function
              | Base summs -> summs :: acc
              | Custom (visited, unvisited, _) -> (visited @ unvisited) :: acc)
          in
          aux values ~visited:true ~curr:(visited :: curr) ~acc:(summs :: acc)
    in
    try aux (List.rev_map (fun ty -> find ty ctx) tys)
    with TypeNotSupported -> f []

  let stage ty summ (ctx : t) : t =
    let@ id = (ty, ctx) in
    let exception SummaryAlreadyExists in
    let[@tail_mod_cons] rec filter = function
      | [] -> []
      | x :: l ->
          if implies summ x then raise SummaryAlreadyExists
            (* The new summary implies an existing summary: discard the new
               summary *)
          else if implies x summ then filter l
            (* An existing summary implies the new summary: discard the existing
               summary *)
          else x :: filter l
    in
    let opt_cons = function
      | None -> Some ([], [], [ summ ])
      | Some (visited, unvisited, staged) -> (
          try
            filter unvisited |> fun unvisited ->
            filter visited |> fun visited ->
            Some (visited, unvisited, summ :: staged)
          with SummaryAlreadyExists -> Some (visited, unvisited, staged))
    in
    M.update id opt_cons ctx

  let commit (ctx : t) : t = M.map (fun (v, u, s) -> (u @ v, s, [])) ctx
end
