module Foo = struct
  type t = int

  let pp = Format.pp_print_int
end

type foo = int

let pp_foo = Format.pp_print_int

type t =
  | Msg of string [@diag.format "msg=%a"]
  | Maybe of string option [@diag.format "maybe=%a"]
  | Pair of Foo.t * foo [@diag.format "pair=%a/%a"]
[@@deriving diagnostic]
