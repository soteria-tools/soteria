(** Call graph for Soteria-Rust.

    Records a directed edge from caller to callee on each function call in the
    interpreter. The graph is stored in a single global variable and is {b NOT}
    concurrent-safe. *)

open Charon

(** Node type: a fully-qualified function name rendered as a string. *)
module FunNode = struct
  type t = { short_name : string; long_name : string } [@@deriving eq, hash]

  let pp ft { long_name; _ } = Format.pp_print_string ft long_name
  let long_name s = s.long_name
  let short_name s = s.short_name

  (** The "short name" is the last identifier, if there is one (should always be
      the case), fallbacks to the full name *)
  let short_name_of_name n =
    let rec aux : Types.name -> Types.name option = function
      | [] -> None
      | (PeIdent _ as short) :: _ -> Some [ short ]
      | _ :: rest -> aux rest
    in
    match aux (List.rev n) with None -> n | Some short -> short

  let of_name n =
    let name_str n = (Fmt.to_to_string Crate.pp_name) n in
    let long_name = name_str n in
    let short_name = name_str (short_name_of_name n) in
    { short_name; long_name }
end

module G = Graph.Make_with_dot (FunNode)

(** The global call graph. Nodes are fully-qualified function names as strings.
    This is not concurrent-safe. *)
let graph : G.t = G.with_node_capacity 64

(** Adds an edge to the global callgraph. Always needs to be called within a
    crate context! *)
let add_edge (from : Types.name option) (to_ : Types.name option) =
  match (from, to_) with
  | Some from, Some to_ ->
      G.add_edge graph (FunNode.of_name from) (FunNode.of_name to_)
  | _ -> ()

(** Dump the call graph as a DOT file at [path]. *)
let dump path =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  G.to_dot fmt graph;
  Format.pp_print_flush fmt ();
  close_out oc

let () =
  at_exit (fun () ->
      match (Config.get ()).dump_callgraph with
      | None -> ()
      | Some path -> dump path)
