module T = Typed.T
module Var = Bfa_symex.Var

type 'err t = {
  args : T.cval Typed.t list;
      (** List of arguments values, corresponding to the formal arguments in
          order. Really a form of [(x == a0) * (y == a1)] *)
  pre : Heap.serialized list;  (** Pre-condition as a list of fixes *)
  pc : Svalue.t list;
      (** Path condition. Whether it is in the post or in the pre, it doesn't
          matter for UX. *)
  post : Heap.serialized;  (** Post condition as a serialized heap *)
  ret : (T.cval Typed.t, 'err) result;
      (** Return value. If `ok` then it is the C value that the function
          returned, if `err` then it is a description of the bug exhibitied by
          the code *)
}
[@@deriving show { with_path = false }]

(* TODO: for below: do we only need to compute reachability of variables of type Loc, since
   they are the only one that cannot be created out of the blue, and need provenance? e*)

(** This function prunes the summary by removing anything that isn't reachable
    from the arguments or the return value. It returns the updated summary, as
    well as a boolean capturing whether a memory leak was detected. A memory
    leak is detected if there was an unreachable block that was not freed. *)
let prune summary =
  let module Var_graph = Graph.Make_in_place (Var) in
  let graph = Var_graph.with_node_capacity 0 in
  let reachable = Var.Hashset.with_capacity 0 in
  let mark_reachable x = Var.Hashset.add reachable x in
  (* We mark all variables from the arguments and return value as reachable *)
  let () =
    List.iter
      (fun cval -> Typed.iter_vars cval (fun (x, _) -> mark_reachable x))
      summary.args
  in
  let () =
    Result.iter
      (fun cval -> Typed.iter_vars cval (fun (x, _) -> mark_reachable x))
      summary.ret
  in
  (* For each equality [e1 = e2] in the path condition,
     we add a double edge from all variables of [e1] to all variables of [e2] *)
  ListLabels.iter summary.pc ~f:(fun (v : Svalue.t) ->
      match v.node.kind with
      | Binop (Eq, el, er) ->
          (* We make the second iterator peristent to avoid going over the structure too many times if there are many *)
          let r_iter = Iter.persistent_lazy (Svalue.iter_vars er) in
          let product = Iter.product (Svalue.iter_vars el) r_iter in
          product (fun ((x, _), (y, _)) -> Var_graph.add_double_edge graph x y)
      | _ -> ());
  (* For each block $l -> B in the post heap, we add a single-sided arrow from $l to all variables contained in the values of B.
     Since offsets in a block are integers, they cannot be "learned". *)
  ListLabels.iter summary.post ~f:(fun (l, b) ->
      let b_iter =
       fun k ->
        match b with
        | Csymex.Freeable.Freed -> ()
        | Csymex.Freeable.Alive b ->
            Tree_block.iter_values_serialized b (fun v -> Typed.iter_vars v k)
      in
      Iter.product (Typed.iter_vars l) (Iter.persistent_lazy b_iter)
        (fun ((x, _), (y, _)) -> Var_graph.add_edge graph x y));

  (* [reachable] is the set of initially-reachable variables, and we have a reachability [graph].
     We can compute all reachable values. *)

  (* TODO:
    -> Create a graph by creating double-edges for
        - each equality in the PC: careful, this is OX?
        - loc to all variables in a block,
    -> Use a reachability algorithm on the graph to get all reachable vakues from `reachable`.
    -> Prune all heaps that have unreachable address.
    *)
  ()

let is_memory_leak _summary =
  (* This should be a reachability analysis checking for locations that are:
     - Not reachable (through the heap or PC) from the the arguments
     - Not reachable from the return value (through the heap or PC)
     - Point to non-freed blocks *)
  false

let is_manifest_bug _summary =
  (* Here are the steps to implement this reachability analysis.
     - Filter the post-condition and PC to keep only the pieces that are reachable from the arguments or the return value.
     - produce the post-condition into the empty heap, keep the resulting PC, calling it PC_post.
     - If PC_post => PC, then the bug is manifest.
  *)
  false

let analyse_summary _summary = []
