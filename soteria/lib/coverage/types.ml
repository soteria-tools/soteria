open Soteria_std

type function_id = { file : string; name : string } [@@deriving eq, ord]

module Hfun = Hashtbl.MakeYojsonable (struct
  type t = function_id

  let equal = equal_function_id
  let hash { file; name } = Hashtbl.hash (file, name)
  let to_string { file; name } = file ^ ":" ^ name

  let of_string s =
    match String.split_on_char ':' s with
    | [ file; name ] -> Ok { file; name }
    | _ -> Error ("Invalid function_id string: " ^ s)
end)

module IMap = PatriciaTree.MakeMap (Int)

type branch_coverage = { line : int; then_hits : int; else_hits : int }
[@@deriving yojson]

type function_meta = {
  hits : int; [@default 0]
  line : int option;
  end_line : int option;
}
[@@deriving yojson, make]

type 'meta file_hits = {
  lines : int Hashtbl.Hint.t;
  branches : branch_coverage Hashtbl.Hstring.t;
  mutable meta : 'meta;
}
[@@deriving yojson]

type t = {
  per_file : unit file_hits Hashtbl.Hstring.t;
  per_function : function_meta file_hits Hfun.t;
}
[@@deriving yojson]
