open Cerb_frontend.Symbol

type t = identifier

let to_string (Identifier (_loc, id)) = id
