open Soteria_std
module IMap = PatriciaTree.MakeMap (Int)

type branch_side = Then | Else
type branch_span = { file : string; line : int; branch_id : string }

type code_item =
  | Line of int
  | Conditional of { line : int; branch_id : string; side : branch_side option }
  | Function of { name : string; line : int; end_line : int option }

type branch_coverage = { line : int; then_hits : int; else_hits : int }
type function_coverage = { line : int; end_line : int option; hits : int }

type file_hits = {
  lines : int Hashtbl.Hint.t;
  branches : branch_coverage Hashtbl.Hstring.t;
  functions : function_coverage Hashtbl.Hstring.t;
}

type t = file_hits Hashtbl.Hstring.t
