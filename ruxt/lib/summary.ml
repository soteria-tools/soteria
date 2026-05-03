module Config_ = Config
open Soteria_rust_lib
module Config = Config_

module State = struct
  include State.Tree_state.Make (Tree_borrows.Concrete.Make)

  let iter_vars syn =
    let ins, outs = ins_outs syn in
    let iter_vars_l l = Iter.of_list l |> Iter.flat_map Svalue.iter_vars in
    let ins, outs = (iter_vars_l ins, iter_vars_l outs) in
    (* In our state representation, locations are single variables *)
    assert (Iter.length ins = 1);
    (Iter.head_exn ins, outs)
end

module Logic = struct
  include Soteria.Logic.Make (Rustsymex)

  module Asrt = struct
    include Asrt

    module Execute =
      Execute_partial
        (State)
        (struct
          let mk_exists binders body =
            let binders =
              List.map (fun (var, ty) -> (var, Typed.untype_type ty)) binders
            in
            Svalue.Bool.mk_exists binders body

          let conj = Svalue.Bool.conj
        end)
  end
end

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

  let learn_eq (syn : syn) (ret : t) :
      (unit, State.syn list) Rustsymex.Consumer.t =
    let module Rustsymex = struct
      (* FIXME: This should probably be fixed in the signature of Rustsymex *)
      include Rustsymex
      module Value = Typed
    end in
    let module Learn_eq = Rust_val.Learn_eq (Rustsymex) in
    let open Rustsymex.Consumer.Syntax in
    let learn_eq_ptr syn sptr =
      let+ (), _ =
        Sptr.DecayMap.SM.Consumer.run_with_state ~state:State.empty
        @@ State.Sptr.learn_eq syn sptr
      in
      ()
    in
    Learn_eq.learn_eq learn_eq_ptr syn ret

  (* TODO: This should be part of rust_val.ml *)
  let rec sem_eq (ret1 : t) (ret2 : t) =
    let fold_sem_eq sem_eq rets1 rets2 =
      if List.length rets1 <> List.length rets2 then Typed.v_false
      else
        ListLabels.fold_left2 rets1 rets2 ~init:Typed.v_true
          ~f:(fun sv ret1 ret2 -> Typed.and_ sv @@ sem_eq ret1 ret2)
    in
    match (ret1, ret2) with
    | Int v1, Int v2 -> Typed.sem_eq v1 v2
    | Float v1, Float v2 -> Typed.sem_eq v1 v2
    (* TODO: Sptr needs sem_eq *)
    | Enum (disc1, rets1), Enum (disc2, rets2) ->
        let same_disc = Typed.sem_eq disc1 disc2 in
        Typed.and_ same_disc @@ fold_sem_eq sem_eq rets1 rets2
    | Tuple rets1, Tuple rets2 -> fold_sem_eq sem_eq rets1 rets2
    | Union rets1, Union rets2 ->
        let sem_eq (ret1, sv1) (ret2, sv2) =
          Typed.and_ (sem_eq ret1 ret2) (Typed.sem_eq sv1 sv2)
        in
        fold_sem_eq sem_eq rets1 rets2
    (* TODO: What to do with PolyVal? *)
    | _, _ -> Typed.v_false
end

type t = { ret : Ret.syn; asrt : State.syn Logic.Asrt.t }

let pp fmt = function
  | { ret; asrt } ->
      Fmt.pf fmt "{\n ret = %a;\n asrt = %a\n}" Ret.pp_syn ret
        (Logic.Asrt.pp State.pp_syn)
        asrt

let nondet (ty : Charon.Types.ty) : t list =
  let module Encoder = Value_codec.Encoder (State.Sptr) in
  Rustsymex.run_needs_stats ~mode:UX @@ Encoder.nondet_valid ty
  |> ListLabels.map ~f:(function
    | Soteria.Symex.Compo_res.Ok ret, pcs ->
        let ret = Ret.to_syn ret in
        let pure = List.map Typed.Expr.of_value pcs in
        let asrt = Logic.Asrt.make ~spatial:[] ~pure in
        { ret; asrt }
    | _ -> failwith "Expected Ok in nondet")

let make (ret : Ret.t) (st : State.SM.st) (pcs : Typed.sbool Typed.t list) :
    (t, [> `MemoryLeak ]) result =
  let open Result.Syntax in
  let ret = Ret.to_syn ret in
  let+ asrt =
    let spatial = State.to_syn @@ State.of_opt st in
    let module Var_graph = Soteria.Soteria_std.Graph.Make_in_place (Svalue.Var) in
    let module Var_hashset = Var_graph.Node_set in
    let reachable =
      let graph = Var_graph.with_node_capacity 0 in
      (* For each equality [e1 = e2] in the path condition, we add a double edge
         from all variables of [e1] to all variables of [e2] *)
      ListLabels.iter pcs ~f:(fun v ->
          match Typed.kind v with
          | Binop (Eq, el, er) ->
              (* We make the second iterator peristent to avoid going over the
                 structure too many times if there are many *)
              let r_iter = Iter.persistent_lazy @@ Svalue.iter_vars er in
              let product = Iter.product (Svalue.iter_vars el) r_iter in
              product (fun ((x, _), (y, _)) ->
                  Var_graph.add_double_edge graph x y)
          | _ -> ());
      (* For each block $l -> B in the post state, we add a single-sided arrow
         from the variable in $l to all variables contained in B. *)
      ListLabels.iter spatial ~f:(fun syn ->
          let (var, _), outs = State.iter_vars syn in
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
            let (var, _), _ = State.iter_vars syn in
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
      let locs =
        ListLabels.fold_left spatial ~init:[] ~f:(fun acc syn ->
            let ins, _ = State.ins_outs syn in
            assert (List.length ins = 1);
            List.hd ins :: acc)
      in
      let filtered =
        ListLabels.filter_map pcs ~f:(fun v ->
            let sv = Typed.Expr.of_value v in
            (* Ignore assertions with unreachable variables *)
            if
              not
              @@ Iter.exists (fun (var, _) -> Var_hashset.mem reachable var)
              @@ Svalue.iter_vars sv
            then None
            else
              match Typed.kind v with
              | Nop (Distinct, l) ->
                  (* Replace all distincts with a single distinct assertion *)
                  if List.for_all (fun sv -> List.mem sv locs) l then None
                  else Some sv
              | _ -> Some sv)
      in
      let distinct = Svalue.Bool.distinct locs in
      match Svalue.Bool.to_bool distinct with
      | Some true -> filtered
      | _ -> distinct :: filtered
    in
    Logic.Asrt.make ~spatial ~pure
  in
  { ret; asrt }

let produce (summ : t) (st : State.SM.st) :
    (Ret.t * State.SM.st) Rustsymex.Producer.t =
  let open Rustsymex.Producer.Syntax in
  let* ret = Rustsymex.Producer.apply_subst Ret.subst summ.ret in
  let* st = Logic.Asrt.Execute.produce summ.asrt st in
  Rustsymex.Producer.return (ret, st)

let consume (summ : t) (ret : Ret.t) (st : State.SM.st) :
    (State.SM.st, State.syn list) Rustsymex.Consumer.t =
  let open Rustsymex.Consumer.Syntax in
  let* () = Ret.learn_eq summ.ret ret in
  Logic.Asrt.Execute.consume summ.asrt st

let run_producer (subst : Typed.Expr.Subst.t) (summ : t) :
    (Ret.t * Typed.Expr.Subst.t) State.SM.t =
 fun st ->
  let symex = Rustsymex.Producer.run ~subst @@ produce summ st in
  Rustsymex.map symex (fun ((ret, st), subst) -> ((ret, subst), st))

let implies s_pre s_post =
  let process =
    let open Rustsymex.Syntax in
    (* Make sure we run the producer inside the symex *)
    let* () = Rustsymex.return () in
    let subst = Typed.Expr.Subst.empty in
    let* (ret, _), st = run_producer subst s_pre State.empty in
    Rustsymex.Consumer.run ~subst @@ consume s_post ret st
  in
  match Rustsymex.run_needs_stats ~mode:OX process with
  | [ (Soteria.Symex.Compo_res.Ok (st, _), _) ] -> st = State.empty
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
    | ty when is_base_ty ty -> Base (nondet ty)
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
      | Some (visited, unvisited, staged) as summs -> (
          try Some (filter visited, filter unvisited, summ :: filter staged)
          with SummaryAlreadyExists -> summs)
    in
    M.update id opt_cons ctx

  let commit (ctx : t) : t = M.map (fun (v, u, s) -> (u @ v, s, [])) ctx
end
