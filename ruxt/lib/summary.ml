open Soteria_rust_lib
open Charon.Generated_Types
open Rustsymex.Syntax

type t = {
  ret : Heap.Sptr.t Rust_val.t;
  pcs : Typed.sbool Typed.t list;
  state : Heap.serialized;
}

let iter_vars_ret ret = Rust_val.iter_vars Heap.Sptr.iter_vars ret

let iter_vars summ f =
  iter_vars_ret summ.ret f;
  List.iter (fun v -> Typed.iter_vars v f) summ.pcs;
  Heap.iter_vars_serialized summ.state f

let subst subst_var summ =
  let ret = Rust_val.subst Heap.Sptr.subst subst_var summ.ret in
  let pcs = List.map (Typed.subst subst_var) summ.pcs in
  let state = Heap.subst_serialized subst_var summ.state in
  { ret; pcs; state }

let make ret pcs state =
  let module Var_graph = Soteria_std.Graph.Make_in_place (Svalue.Var) in
  let module Var_hashset = Var_graph.Node_set in
  let graph = Var_graph.with_node_capacity 0 in
  (* For each equality [e1 = e2] in the path condition,
     we add a double edge from all variables of [e1] to all variables of [e2] *)
  ListLabels.iter pcs ~f:(fun v ->
      match Typed.kind v with
      | Binop (Eq, el, er) ->
          (* We make the second iterator peristent to avoid going over the structure too many times if there are many *)
          let r_iter = Iter.persistent_lazy (Svalue.iter_vars er) in
          let product = Iter.product (Svalue.iter_vars el) r_iter in
          product (fun ((x, _), (y, _)) -> Var_graph.add_double_edge graph x y)
      | _ -> ());
  (* For each block $l -> B in the post state, we add a single-sided arrow
     from all variables in $l to all variables contained in B. *)
  let state = Heap.serialize state in
  ListLabels.iter state ~f:(fun (l, (b, _)) ->
      let b_iter =
        Iter.persistent_lazy
        @@ Rustsymex.Freeable.iter_vars_serialized
             Heap.Tree_block.iter_vars_serialized b
      in
      let product = Iter.product (Typed.iter_vars l) b_iter in
      product (fun ((x, _), (y, _)) -> Var_graph.add_edge graph x y));
  (* [init_reachable] is the set of initially-reachable variables, and we have a reachability [graph].
     We can compute all reachable values. *)
  let init_reachable = Var_hashset.with_capacity 0 in
  (* We mark all variables from the return value as reachable *)
  iter_vars_ret ret (fun (x, _) -> Var_hashset.add init_reachable x);
  let reachable = Var_graph.reachable_from graph init_reachable in
  (* We can now filter the summary to keep only the reachable values *)
  let is_relevant sv =
    Typed.iter_vars sv
    |> Iter.exists (fun (var, _) -> Var_hashset.mem reachable var)
  in
  let pcs = List.filter is_relevant pcs in
  let state, unreachable =
    List.partition (fun (loc, _) -> is_relevant loc) state
  in
  let leaks =
    if !Config.current.ignore_leaks then []
    else
      (* A leak occurs when the pointer is alive and is not global *)
      ListLabels.filter unreachable ~f:(fun (_, (f, g)) ->
          f <> Rustsymex.Freeable.Freed && not g)
  in
  ({ ret; pcs; state }, leaks)

module Symex = struct
  type t = (Heap.Sptr.t Rust_val.t * Heap.serialized) Rustsymex.t

  let make summ =
    let* subst_var = Subst.add_vars Subst.empty (iter_vars summ) in
    let { ret; pcs; state } = subst (Subst.to_fn subst_var) summ in
    let+ () = Rustsymex.assume pcs in
    (ret, state)

  let flatten (summs : t list) =
    let empty = Rustsymex.return ([], Heap.empty) in
    let extend flattened summ =
      let* args, state = flattened in
      let* arg, serialized = summ in
      let+ state = Heap.produce serialized state in
      (arg :: args, state)
    in
    List.fold_left extend empty summs
end

module Context = struct
  module M = Charon.Types.TypeDeclId.Map

  type nonrec t = t list M.t

  let empty : t = M.empty

  let update ty summ (ctx : t) : t =
    match ty with
    | TAdt { id = TAdtId id; _ } ->
        let opt_cons = function
          | None -> Some [ summ ]
          (* PEDRO: Check if summ is already in summs *)
          | Some summs -> Some (summ :: summs)
        in
        M.update id opt_cons ctx
    | _ -> ctx

  let get ty (ctx : t) : Symex.t list =
    match ty with
    | TAdt { id = TAdtId id; _ } -> (
        match M.find_opt id ctx with
        | None -> []
        | Some summs -> List.map Symex.make summs)
    | TLiteral lit ->
        [
          (let+ cval = Layout.nondet_literal_ty lit in
           (Rust_val.Base cval, Heap.serialize Heap.empty));
        ]
    | _ -> []

  let iter_summs tys (ctx : t) f =
    let rec aux acc = function
      | [] -> f acc
      | summs :: rest -> List.iter (fun s -> aux (s :: acc) rest) summs
    in
    aux [] (List.rev_map (fun ty -> get ty ctx) tys)
end
