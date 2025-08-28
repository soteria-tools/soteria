module Make_in_place (Node : Hashset.PrintableHashedType) = struct
  type node = Node.t

  module Node_set = Hashset.Make (Node)
  module Hashtbl = Hashtbl.Make (Node)

  type t = Node_set.t Hashtbl.t

  let with_node_capacity n = Hashtbl.create n
  let node_count graph = Hashtbl.length graph

  let pp =
    let pp_caller ft (caller, callees) =
      Fmt.pf ft "@[<h>%a ->@ %a@]" Node.pp caller Node_set.pp callees
    in
    Fmt.vbox (Fmt.iter_bindings ~sep:Fmt.sp Hashtbl.iter pp_caller)

  let add_edge graph from to_ =
    match Hashtbl.find_opt graph from with
    | None -> Hashtbl.replace graph from (Node_set.singleton to_)
    | Some tos -> Node_set.add tos to_

  let add_double_edge graph from to_ =
    add_edge graph from to_;
    add_edge graph to_ from

  let set_edges_from graph from nodes = Hashtbl.replace graph from nodes

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

let%test_module "Graph tests" =
  (module struct
    module G = Make_in_place (struct
      include Int

      let pp = Fmt.int
    end)

    let simple_graph =
      let graph = G.with_node_capacity 3 in
      G.add_edge graph 1 2;
      G.add_edge graph 1 3;
      graph

    let bigger_graph =
      let graph = G.with_node_capacity 12 in
      G.add_edge graph 1 2;
      G.add_edge graph 2 3;
      G.add_edge graph 4 5;
      G.add_edge graph 4 6;
      G.add_edge graph 6 7;
      G.add_edge graph 7 8;
      G.add_edge graph 6 4;
      G.add_edge graph 10 11;
      graph

    let straight_line =
      let graph = G.with_node_capacity 3 in
      G.add_edge graph 1 2;
      G.add_edge graph 2 3;
      G.add_edge graph 6 7;
      G.add_edge graph 5 6;
      G.add_edge graph 4 5;
      G.add_edge graph 3 4;
      graph

    let%test "empty reachable set" =
      let empty_set = G.Node_set.with_capacity 0 in
      let reachables = G.reachable_from simple_graph empty_set in
      G.Node_set.equal reachables empty_set

    let%test "one reachable set" =
      let singleton = G.Node_set.singleton 3 in
      let reachables = G.reachable_from simple_graph singleton in
      G.Node_set.equal reachables singleton

    let%test "several reachable set 1" =
      let init = G.Node_set.of_seq (List.to_seq [ 1; 7 ]) in
      let expected = G.Node_set.of_seq (List.to_seq [ 1; 2; 3; 7; 8 ]) in
      let reachables = G.reachable_from bigger_graph init in
      G.Node_set.equal reachables expected

    let%test "several reachable set 2" =
      let init = G.Node_set.of_seq (List.to_seq [ 1; 6 ]) in
      let expected =
        G.Node_set.of_seq (List.to_seq [ 1; 2; 3; 4; 5; 6; 7; 8 ])
      in
      let reachables = G.reachable_from bigger_graph init in
      G.Node_set.equal reachables expected

    let%test "disconnected nodes" =
      let init = G.Node_set.singleton 9 in
      let expected = G.Node_set.singleton 9 in
      let reachables = G.reachable_from bigger_graph init in
      G.Node_set.equal reachables expected

    let%test "all nodes reachable" =
      let init = G.Node_set.of_seq (List.to_seq [ 1; 4; 10; 5 ]) in
      let expected =
        G.Node_set.of_seq (List.to_seq [ 1; 2; 3; 4; 5; 6; 7; 8; 10; 11 ])
      in
      let reachables = G.reachable_from bigger_graph init in
      G.Node_set.equal reachables expected

    let%test "single node with self-loop" =
      let graph = G.with_node_capacity 1 in
      G.add_edge graph 1 1;
      let init = G.Node_set.singleton 1 in
      let expected = G.Node_set.singleton 1 in
      let reachables = G.reachable_from graph init in
      G.Node_set.equal reachables expected

    let%test "node with multiple edges" =
      let init = G.Node_set.singleton 1 in
      let expected = G.Node_set.of_seq (List.to_seq [ 1; 2; 3 ]) in
      let reachables = G.reachable_from simple_graph init in
      G.Node_set.equal reachables expected

    let%test "topological order" =
      let order = G.weak_topological_order straight_line in
      let expected = [ 7; 6; 5; 4; 3; 2; 1 ] in
      List.for_all2 ( = ) order expected
  end)
