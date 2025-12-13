(** An in-place mutable graph implementation with reachability and topological
    ordering. *)

module Make_in_place (Node : Hashset.PrintableHashedType) = struct
  type node = Node.t

  module Node_set = Hashset.Make (Node)
  module Hashtbl = Hashtbl.Make (Node)

  type t = Node_set.t Hashtbl.t

  (** Create a new graph with the given initial capacity for nodes. *)
  let with_node_capacity n = Hashtbl.create n

  (** Return the number of nodes in the graph. *)
  let node_count graph = Hashtbl.length graph

  (** Pretty-printer for the graph. *)
  let pp =
    let pp_caller ft (caller, callees) =
      Fmt.pf ft "@[<h>%a ->@ %a@]" Node.pp caller Node_set.pp callees
    in
    Fmt.vbox (Fmt.iter_bindings ~sep:Fmt.sp Hashtbl.iter pp_caller)

  (** Add a directed edge from one node to another. *)
  let add_edge graph from to_ =
    match Hashtbl.find_opt graph from with
    | None -> Hashtbl.replace graph from (Node_set.singleton to_)
    | Some tos -> Node_set.add tos to_

  (** Add a bidirectional edge between two nodes. *)
  let add_double_edge graph from to_ =
    add_edge graph from to_;
    add_edge graph to_ from

  (** Set the outgoing edges for a specific node. *)
  let set_edges_from graph from nodes = Hashtbl.replace graph from nodes

  (** Compute the set of all nodes reachable from a given set of start nodes. *)
  let reachable_from graph reachable : Node_set.t =
    let visited = Node_set.with_capacity (Node_set.cardinal reachable) in
    let queue = Queue.of_seq (Node_set.to_seq reachable) in
    let rec bfs () =
      if Queue.is_empty queue then ()
      else
        let next = Queue.pop queue in
        let () =
          if not (Node_set.mem visited next) then
            let () = Node_set.add visited next in
            match Hashtbl.find_opt graph next with
            | None -> ()
            | Some set -> Queue.add_seq queue (Node_set.to_seq set)
        in
        bfs ()
    in
    bfs ();
    visited

  (** A topological order where SCCs are not necessarily well-ordered *)
  let weak_topological_order (cg : t) : node list =
    let cg_list =
      Hashtbl.to_seq cg
      |> Seq.map (fun (caller, callees) ->
             (caller, Node_set.to_seq callees |> List.of_seq))
      |> List.of_seq
    in
    let sorted_components = Tsort.sort_strongly_connected_components cg_list in
    (* We could order the components themselves a bit better, but let's ignore it for now. *)
    List.concat sorted_components
end
