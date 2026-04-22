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
    (* We could order the components themselves a bit better, but let's ignore
       it for now. *)
    List.concat sorted_components
end

(** Node type extended with display names for DOT serialization. *)
module type With_names = sig
  type t

  (** A short label used as the visible node label in the graph. *)
  val short_name : t -> string

  (** The full name used as the tooltip (visible on hover). *)
  val long_name : t -> string
end

module Make_with_dot (Node : [%mixins Hashset.PrintableHashedType + With_names]) =
struct
  include Make_in_place (Node)

  let dot_keywords = [| "graph"; "node"; "edge"; "digraph"; "subgraph" |]

  (** Serialize the graph to DOT format for visualization.

      Each node is rendered with [Node.short_name] as its visible label and
      [Node.long_name] as its tooltip, so that interactive renderers (e.g.
      d3-graphviz, xdot) show the full name on hover while keeping the graph
      layout readable. *)
  let to_dot ~graph_name fmt graph =
    (* Assign a stable integer ID to each node to avoid quoting issues with
       arbitrary node names. *)
    if Array.mem graph_name dot_keywords then
      failwith
        (Printf.sprintf
           "Graph name '%s' is a reserved DOT keyword, please choose a \
            different name"
           graph_name);
    let ids : int Hashtbl.t = Hashtbl.create (Hashtbl.length graph) in
    let next_id = ref 0 in
    let id_of node =
      match Hashtbl.find_opt ids node with
      | Some id -> id
      | None ->
          let id = !next_id in
          Hashtbl.replace ids node id;
          incr next_id;
          id
    in
    (* Collect all nodes (both sources and targets). *)
    Hashtbl.iter
      (fun from tos ->
        ignore (id_of from);
        Node_set.iter (fun to_ -> ignore (id_of to_)) tos)
      graph;
    Fmt.pf fmt "digraph %s {@\n" graph_name;
    Fmt.pf fmt "  node [shape=box fontname=\"monospace\"];@\n";
    (* Emit node declarations. *)
    Hashtbl.iter
      (fun node id ->
        Fmt.pf fmt "  n%d [label=\"%s\" tooltip=\"%s\"];@\n" id
          (String.escaped (Node.short_name node))
          (String.escaped (Node.long_name node)))
      ids;
    (* Emit edges. *)
    Hashtbl.iter
      (fun from tos ->
        let from_id = id_of from in
        Node_set.iter
          (fun to_ -> Fmt.pf fmt "  n%d -> n%d;@\n" from_id (id_of to_))
          tos)
      graph;
    Fmt.pf fmt "}@\n"
end
