module Typed = Soteria_rust_lib.Typed
module Rustsymex = Soteria_rust_lib.Rustsymex
module Compo_res = Soteria.Symex.Compo_res
module Rust_val = Soteria_rust_lib.Rust_val
open Rustsymex.Syntax

type t = {
  ret : Heap.Sptr.t Rust_val.t;
  pcs : Typed.sbool Typed.t list;
  state : Heap.serialized;
}

let iter_vars_ret ret = Rust_val.iter_vars Heap.Sptr.iter_vars ret
let subst_ret subst_var ret = Rust_val.subst Heap.Sptr.subst subst_var ret

let iter_vars summ f =
  iter_vars_ret summ.ret f;
  List.iter (fun v -> Typed.iter_vars v f) summ.pcs;
  Heap.iter_vars_serialized summ.state f

let subst subst_var summ =
  let ret = subst_ret subst_var summ.ret in
  let pcs = List.map (Typed.subst subst_var) summ.pcs in
  let state = Heap.subst_serialized subst_var summ.state in
  { ret; pcs; state }

let fresh_vars summ =
  let+ subst_var = Subst.add_vars Subst.empty (iter_vars summ) in
  subst (Subst.to_fn subst_var) summ

let produce (summ : t) st =
  let* summ = fresh_vars summ in
  let* () = Rustsymex.assume summ.pcs in
  let+ st = Heap.produce summ.state st in
  (summ.ret, st)

let make ret pcs state =
  let module Var_graph = Soteria.Soteria_std.Graph.Make_in_place (Svalue.Var) in
  let module Var_hashset = Var_graph.Node_set in
  let graph = Var_graph.with_node_capacity 0 in
  (* For each equality [e1 = e2] in the path condition,
     we add a double edge from all variables of [e1] to all variables of [e2] *)
  ListLabels.iter pcs ~f:(fun v ->
      match Typed.kind v with
      | Binop (Eq, el, er) ->
          let module Value = Soteria.Bv_values.Svalue in
          (* We make the second iterator peristent to avoid going over the structure too many times if there are many *)
          let r_iter = Iter.persistent_lazy @@ Value.iter_vars er in
          let product = Iter.product (Value.iter_vars el) r_iter in
          product (fun ((x, _), (y, _)) -> Var_graph.add_double_edge graph x y)
      | _ -> ());
  (* For each block $l -> B in the post state, we add a single-sided arrow
     from all variables in $l to all variables contained in B. *)
  let heap, globals = Heap.serialize state in
  let iter_vars_serialized_block s =
    Heap.With_meta.iter_vars_serialized
      (Heap.Freeable.iter_vars_serialized Heap.Tree_block.iter_vars_serialized)
      s
  in
  ListLabels.iter heap ~f:(fun (l, s) ->
      let b_iter = Iter.persistent_lazy @@ iter_vars_serialized_block s in
      let product = Iter.product (Typed.iter_vars l) b_iter in
      product (fun ((x, _), (y, _)) -> Var_graph.add_edge graph x y));
  let init_reachable = Var_hashset.with_capacity 0 in
  (* We mark all variables from the return value as reachable *)
  iter_vars_ret ret (fun (x, _) -> Var_hashset.add init_reachable x);
  (* [init_reachable] is the set of initially-reachable variables, and we have a reachability [graph].
     We can compute all reachable values. *)
  let reachable = Var_graph.reachable_from graph init_reachable in
  let is_relevant sv =
    Iter.exists (fun (var, _) -> Var_hashset.mem reachable var)
    @@ Typed.iter_vars sv
  in
  (* We can now filter the summary to keep only the reachable values *)
  let ( let** ) = Result.bind in
  let** state =
    let heap, unreachable =
      ListLabels.partition heap ~f:(fun (loc, _) -> is_relevant loc)
    in
    if !Config.current.ignore_leaks then Result.ok (heap, globals)
    else
      let leaks =
        ListLabels.filter unreachable
          ~f:(fun (loc, Heap.With_meta.{ node; _ }) ->
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
  Result.ok { ret; pcs; state }

let base ty =
  let process = Soteria_rust_lib.Layout.nondet (TLiteral ty) in
  match Rustsymex.run ~mode:UX process with
  | [ (Compo_res.Ok ret, pcs) ] ->
      { ret; pcs; state = Heap.serialize Heap.empty }
  | _ -> failwith "nondet for literals should have exactly 1 Ok outcome"

module Context = struct
  open Charon.Types
  module M = TypeDeclId.Map

  (* Each custom type has separate lists for visited and unvisited summaries *)
  type value = Base of t | Custom of t list * t list
  type nonrec t = (t list * t list) M.t

  let empty : t = M.empty

  let add ty summ (ctx : t) : t =
    match ty with
    | TAdt { id = TAdtId id; _ } ->
        let opt_cons = function
          | None -> Some ([], [ summ ])
          | Some (visited, unvisited) -> Some (visited, summ :: unvisited)
        in
        M.update id opt_cons ctx
    | _ -> ctx

  let find ty (ctx : t) : value =
    match ty with
    | TAdt { id = TAdtId id; _ } ->
        let visited, unvisited =
          Option.value ~default:([], []) (M.find_opt id ctx)
        in
        Custom (visited, unvisited)
    | TLiteral lit -> Base (base lit)
    | _ -> Custom ([], [])

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
      | Base summ :: values -> aux values ~curr:([ summ ] :: curr) ~acc
      | Custom (visited, unvisited) :: values ->
          let summs =
            ListLabels.fold_left values ~init:(unvisited :: curr)
              ~f:(fun acc -> function
              | Base summ -> [ summ ] :: acc
              | Custom (visited, unvisited) -> (visited @ unvisited) :: acc)
          in
          aux values ~visited:true ~curr:(visited :: curr) ~acc:(summs :: acc)
    in
    aux (List.rev_map (fun ty -> find ty ctx) tys)

  let visit (ctx : t) : t =
    M.map (fun (visited, unvisited) -> (visited @ unvisited, [])) ctx
end
