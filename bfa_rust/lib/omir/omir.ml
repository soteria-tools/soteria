type t = Ast.crate [@@deriving show, eq, ord]

let parse : Yojson.Safe.t -> (t, string) result = Ast.crate_of_yojson
