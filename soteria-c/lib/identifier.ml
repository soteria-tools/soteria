open Cerb_frontend.Symbol

type t = identifier

let to_string (Identifier (_loc, id)) = id
let compare i1 i2 = String.compare (to_string i1) (to_string i2)
let equal i1 i2 = String.equal (to_string i1) (to_string i2)
