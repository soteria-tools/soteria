open Test_register

let register f = register "Graph" f

open Graph

module G = Make_in_place (struct
  include Int

  let pp = Fmt.int
end)

let node_set : G.Node_set.t Alcotest.testable =
  (module struct
    type t = G.Node_set.t

    let pp = G.Node_set.pp
    let equal = G.Node_set.equal
  end)

let check_node_set = Alcotest.(check node_set) "nodes are equal"

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

let empty_reachable =
  let@ () = register "empty reachable set" in
  let empty_set = G.Node_set.with_capacity 0 in
  let reachables = G.reachable_from simple_graph empty_set in
  check_node_set reachables empty_set

let one_reacheable =
  let@ () = register "one reachable set" in
  let singleton = G.Node_set.singleton 3 in
  let reachables = G.reachable_from simple_graph singleton in
  check_node_set reachables singleton

let several_reachable_1 =
  let@ () = register "several reachable set 1" in
  let init = G.Node_set.of_seq (List.to_seq [ 1; 7 ]) in
  let expected = G.Node_set.of_seq (List.to_seq [ 1; 2; 3; 7; 8 ]) in
  let reachables = G.reachable_from bigger_graph init in
  check_node_set reachables expected

let several_reachable_2 =
  let@ () = register "several reachable set 2" in
  let init = G.Node_set.of_seq (List.to_seq [ 1; 6 ]) in
  let expected = G.Node_set.of_seq (List.to_seq [ 1; 2; 3; 4; 5; 6; 7; 8 ]) in
  let reachables = G.reachable_from bigger_graph init in
  check_node_set reachables expected

let disconnected_nodes =
  let@ () = register "disconnected nodes" in
  let init = G.Node_set.singleton 9 in
  let expected = G.Node_set.singleton 9 in
  let reachables = G.reachable_from bigger_graph init in
  check_node_set reachables expected

let all_reachable =
  let@ () = register "all nodes reachable" in
  let init = G.Node_set.of_seq (List.to_seq [ 1; 4; 10; 5 ]) in
  let expected =
    G.Node_set.of_seq (List.to_seq [ 1; 2; 3; 4; 5; 6; 7; 8; 10; 11 ])
  in
  let reachables = G.reachable_from bigger_graph init in
  check_node_set reachables expected

let single_self_loop =
  let@ () = register "single node with self-loop" in
  let graph = G.with_node_capacity 1 in
  G.add_edge graph 1 1;
  let init = G.Node_set.singleton 1 in
  let expected = G.Node_set.singleton 1 in
  let reachables = G.reachable_from graph init in
  check_node_set reachables expected

let multiple_edges =
  let@ () = register "node with multiple edges" in
  let init = G.Node_set.singleton 1 in
  let expected = G.Node_set.of_seq (List.to_seq [ 1; 2; 3 ]) in
  let reachables = G.reachable_from simple_graph init in
  check_node_set reachables expected

let topo_order =
  let@ () = register "topological order" in
  let order = G.weak_topological_order straight_line in
  let expected = [ 7; 6; 5; 4; 3; 2; 1 ] in
  Alcotest.(check (list int)) "same order" order expected
