open Test_register

let register = register "Graph"

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

(* ---- to_dot tests ---- *)

(** A node type for DOT tests: a pair of (short_name, long_name). *)
module StringNode = struct
  type t = { short : string; long : string }

  let equal a b = String.equal a.short b.short && String.equal a.long b.long
  let hash n = Hashtbl.hash n.short
  let pp ft n = Fmt.string ft n.long
  let short_name n = n.short
  let long_name n = n.long
end

module GDot = Make_with_dot (StringNode)

let node s l = StringNode.{ short = s; long = l }

(** Render a graph to a DOT string. *)
let to_dot_string ~graph_name graph =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  GDot.to_dot ~graph_name fmt graph;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let check_dot = Alcotest.(check string)

(** Assert that [substring] appears somewhere in [s]. *)
let check_contains msg substring s =
  if
    not
      (let re = Re.Str.regexp_string substring in
       try
         ignore (Re.Str.search_forward re s 0);
         true
       with Not_found -> false)
  then Alcotest.failf "%s: expected to find %S in:\n%s" msg substring s

let dot_empty_graph =
  let@ () = register "DOT empty graph" in
  let graph = GDot.with_node_capacity 0 in
  let dot = to_dot_string ~graph_name:"empty" graph in
  check_dot "empty graph"
    "digraph empty {\n  node [shape=box fontname=\"monospace\"];\n}\n" dot

let dot_graph_name =
  let@ () = register "DOT custom graph name" in
  let graph = GDot.with_node_capacity 1 in
  GDot.add_edge graph (node "a" "long_a") (node "b" "long_b");
  let dot = to_dot_string ~graph_name:"mygraph" graph in
  check_contains "graph name" "digraph mygraph {" dot

let dot_default_graph_name =
  let@ () = register "DOT default graph name" in
  let graph = GDot.with_node_capacity 1 in
  GDot.add_edge graph (node "a" "long_a") (node "b" "long_b");
  let dot = to_dot_string ~graph_name:"dot_default_graph_name" graph in
  check_contains "default graph name" "digraph dot_default_graph_name {" dot

let dot_short_name_as_label =
  let@ () = register "DOT short name used as label" in
  let graph = GDot.with_node_capacity 1 in
  GDot.add_edge graph
    (node "short_a" "very::long::a")
    (node "short_b" "very::long::b");
  let dot = to_dot_string ~graph_name:"dot_short_name_as_label" graph in
  check_contains "short label a" {|label="short_a"|} dot;
  check_contains "short label b" {|label="short_b"|} dot

let dot_long_name_as_tooltip =
  let@ () = register "DOT long name used as tooltip" in
  let graph = GDot.with_node_capacity 1 in
  GDot.add_edge graph
    (node "short_a" "very::long::a")
    (node "short_b" "very::long::b");
  let dot = to_dot_string ~graph_name:"dot_long_name_as_tooltip" graph in
  check_contains "tooltip a" {|tooltip="very::long::a"|} dot;
  check_contains "tooltip b" {|tooltip="very::long::b"|} dot

let dot_edge_present =
  let@ () = register "DOT edge between nodes" in
  let graph = GDot.with_node_capacity 2 in
  let a = node "a" "long_a" and b = node "b" "long_b" in
  GDot.add_edge graph a b;
  let dot = to_dot_string ~graph_name:"dot_edge_present" graph in
  (* Both nodes must be declared and there must be an edge n? -> n?. *)
  check_contains "edge arrow" " -> " dot;
  check_contains "node a label" {|label="a"|} dot;
  check_contains "node b label" {|label="b"|} dot

let dot_target_only_node_declared =
  let@ () = register "DOT target-only node is declared" in
  let graph = GDot.with_node_capacity 2 in
  GDot.add_edge graph (node "src" "long_src") (node "sink" "long_sink");
  let dot = to_dot_string ~graph_name:"dot_target_only_node_declared" graph in
  (* "sink" has no outgoing edges but must still appear as a node
     declaration. *)
  check_contains "sink label" {|label="sink"|} dot

let dot_escape_double_quote =
  let@ () = register "DOT escapes double quotes in names" in
  let graph = GDot.with_node_capacity 1 in
  GDot.add_edge graph (node {|say "hi"|} {|a "quoted" name|}) (node "b" "b");
  let dot = to_dot_string ~graph_name:"dot_escape_double_quote" graph in
  check_contains "escaped quote in label" {|label="say \"hi\""|} dot;
  check_contains "escaped quote in tooltip" {|tooltip="a \"quoted\" name"|} dot

let dot_escape_backslash =
  let@ () = register "DOT escapes backslashes in names" in
  let graph = GDot.with_node_capacity 1 in
  GDot.add_edge graph (node {|a\b|} {|path\to\node|}) (node "b" "b");
  let dot = to_dot_string ~graph_name:"dot_escape_backslash" graph in
  check_contains "escaped backslash in label" {|label="a\\b"|} dot;
  check_contains "escaped backslash in tooltip" {|tooltip="path\\to\\node"|} dot
