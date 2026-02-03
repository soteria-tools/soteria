module Config_ = Config
open Soteria_rust_lib
module Config = Config_
open Soteria.Symex
open Heap.SM.Syntax

type t = {
  ret : Heap.Sptr.t Rust_val.t;
  pcs : Typed.sbool Typed.t list;
  state : Heap.serialized_globals;
}

let pp fmt =
  let pp_list pp_elem fmt lst =
    Fmt.pf fmt "[\n    %a\n]" (Fmt.list ~sep:(Fmt.any ";\n    ") pp_elem) lst
  in
  function
  | { ret; pcs; state } ->
      Fmt.pf fmt "{\n ret = %a;\n pcs = %a;\n state = %a\n}"
        (Rust_val.pp Heap.Sptr.pp) ret
        (pp_list (Typed.pp Typed.T.pp_sbool))
        pcs Heap.pp_serialized_globals state

let iter_vars_ret ret = Rust_val.iter_vars Heap.Sptr.iter_vars ret
let subst_ret subst_var ret = Rust_val.subst Heap.Sptr.subst subst_var ret

(* TODO: export this to Rust_val *)
let rec sem_eq_ret v1 v2 =
  let fold vs1 vs2 =
    if List.length vs1 = List.length vs2 then
      ListLabels.fold_left2 vs1 vs2 ~init:Typed.v_true ~f:(fun pc v1 v2 ->
          Typed.and_ pc (sem_eq_ret v1 v2))
    else Typed.v_false
  in
  let open Rust_val in
  match (v1, v2) with
  | Int v1, Int v2 -> Typed.sem_eq v1 v2
  | Float v1, Float v2 -> Typed.sem_eq v1 v2
  | Ptr (p1, _), Ptr (p2, _) -> Heap.Sptr.sem_eq p1 p2
  | Enum (v1, vs1), Enum (v2, vs2) ->
      Typed.and_ (Typed.sem_eq v1 v2) (fold vs1 vs2)
  | Tuple vs1, Tuple vs2 -> fold vs1 vs2
  | Union vs1, Union vs2 -> fold (List.map fst vs1) (List.map fst vs2)
  | _ -> Typed.v_false

let iter_vars summ f =
  iter_vars_ret summ.ret f;
  List.iter (fun v -> Typed.iter_vars v f) summ.pcs;
  Heap.iter_vars_serialized summ.state f

let subst subst_var summ =
  let ret = subst_ret subst_var summ.ret in
  let pcs = List.map (Typed.subst subst_var) summ.pcs in
  let state = Heap.subst_serialized subst_var summ.state in
  { ret; pcs; state }

let produce (summ : t) : Heap.Sptr.t Rust_val.t Heap.SM.t =
  let* subst_var =
    Heap.SM.lift @@ Subst.add_vars Subst.empty (iter_vars summ)
  in
  let summ = subst (Subst.to_fn subst_var) summ in
  let* () = Heap.SM.assume summ.pcs in
  let* () =
    Heap.SM.fold_list (fst summ.state) ~init:() ~f:(fun () -> Heap.produce)
  in
  Heap.SM.return summ.ret

let consume (summ : t) (ret : Heap.Sptr.t Rust_val.t) :
    (unit, [> Rustsymex.lfail ], Heap.serialized) Heap.SM.Result.t =
  let ret_eqs = sem_eq_ret ret summ.ret |> Typed.split_ands |> Iter.to_list in
  Consume.run summ.state (ret_eqs @ summ.pcs)

let make ret pcs state =
  let module Var_graph = Soteria.Soteria_std.Graph.Make_in_place (Svalue.Var) in
  let module Var_hashset = Var_graph.Node_set in
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
          product (fun ((x, _), (y, _)) -> Var_graph.add_double_edge graph x y)
      | _ -> ());
  (* For each block $l -> B in the post state, we add a single-sided arrow from
     all variables in $l to all variables contained in B. *)
  let heap, globals = Heap.serialize state in
  ListLabels.iter heap ~f:(fun (l, s) ->
      let b_iter =
        Iter.persistent_lazy
        @@ Heap.Freeable_block_with_meta.iter_vars_serialized s
      in
      let product = Iter.product (Typed.iter_vars l) b_iter in
      product (fun ((x, _), (y, _)) -> Var_graph.add_edge graph x y));
  let init_reachable = Var_hashset.with_capacity 0 in
  (* We mark all variables from the return value as reachable *)
  iter_vars_ret ret (fun (x, _) -> Var_hashset.add init_reachable x);
  (* [init_reachable] is the set of initially-reachable variables, and we have a
     reachability [graph]. We can compute all reachable values. *)
  let reachable = Var_graph.reachable_from graph init_reachable in
  let is_relevant sv =
    Iter.exists (fun (var, _) -> Var_hashset.mem reachable var)
    @@ Typed.iter_vars sv
  in
  (* We can now filter the summary to keep only the reachable values *)
  let open Result.Syntax in
  let+ state =
    let heap, unreachable =
      ListLabels.partition heap ~f:(fun (loc, _) -> is_relevant loc)
    in
    if (Config.get ()).ignore_leaks then Result.ok (heap, globals)
    else
      let leaks =
        ListLabels.filter unreachable
          ~f:(fun (loc, Heap.Freeable_block_with_meta.{ node; _ }) ->
            (* A leak occurs when the pointer is alive and is not global *)
            node <> Soteria.Sym_states.Freeable.Freed
            && not (List.mem loc globals))
      in
      if leaks <> [] then Result.error `MemoryLeak else Result.ok (heap, globals)
  in
  let pcs =
    let pcs =
      ListLabels.filter pcs ~f:(fun sv ->
          match Typed.kind sv with
          | Nop (Distinct, _) -> false
          | _ -> is_relevant sv)
    in
    let locs = List.map fst (fst state) in
    if List.length locs <= 1 then pcs else Typed.distinct locs :: pcs
  in
  { ret; pcs; state }

let implies s_pre s_post =
  let process =
    Heap.SM.Result.run_with_state ~state:Heap.empty
      (let* ret = produce s_pre in
       consume s_post ret)
  in
  match Rustsymex.run_needs_stats ~mode:OX process with
  | [ (Compo_res.Ok ((), st), _) ] -> st == Heap.empty
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
            let module Encoder = Value_codec.Encoder (Heap.Sptr) in
            Heap.SM.Result.run_with_state ~state:Heap.empty
              (Heap.SM.lift @@ Encoder.nondet ty)
          in
          Rustsymex.run_needs_stats ~mode:UX process
          |> ListLabels.map ~f:(function
            | Compo_res.Ok (ret, st), pcs ->
                { ret; pcs; state = Heap.serialize st }
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
