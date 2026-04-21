(** Call graph for Soteria-Rust.

    Records a directed edge from caller to callee on each function call in the
    interpreter. The graph is stored in a single global variable and is {b NOT}
    concurrent-safe. *)

open Charon

(** Node type: a fully-qualified function name rendered as a string. *)
module FunNode = struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
  let pp = Fmt.string
  let long_name s = s

  (** The short label is the last component of the dotted name. *)
  let short_name s =
    let end_ = String.length s - 1 in
    let end_ =
      if String.get s (String.length s - 1) = '>' then
        (* Handle case of poly name, e.g. `f::<T>` *)
        match String.rindex_opt s ':' with
        | Some i -> i - 2
        | None -> Fmt.failwith "Unexpected function name format: %s" s
      else end_
    in
    match String.rindex_from_opt s end_ ':' with
    | Some i -> String.sub s (i + 1) (String.length s - i - 1)
    | None -> s
end

module G = Graph.Make_with_dot (FunNode)

(* ------------------------------------------------------------------ *)
(* Global state – NOT concurrent-safe.                                *)
(* Will be reused across tests.                                       *)
(* ------------------------------------------------------------------ *)

let graph : G.t = G.with_node_capacity 64

(* ------------------------------------------------------------------ *)
(* Public API                                                           *)
(* ------------------------------------------------------------------ *)

(** Adds an edge to the global callgraph. Always needs to be called within a
    crate context! *)
let add_edge (from : Types.name option) (to_ : Types.name option) =
  let name_str n = (Fmt.to_to_string Crate.pp_name) n in
  match (from, to_) with
  | Some from, Some to_ -> G.add_edge graph (name_str from) (name_str to_)
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
