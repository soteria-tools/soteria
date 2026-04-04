Deriving diagnostic guessed printers
  $ ../test.sh guess.ml
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
  
  include struct
    let _ = fun (_ : t) -> ()
  
    let pp ft (e : t) =
      match e with
      | Msg v0 -> (Fmt.pf ft "msg=%a") Format.pp_print_string v0
      | Maybe v0 ->
          (Fmt.pf ft "maybe=%a")
            (Format.pp_print_option Format.pp_print_string)
            v0
      | Pair (v0, v1) -> ((Fmt.pf ft "pair=%a/%a") Foo.pp v0) pp_foo v1
  
    let _ = pp
  
    let kind_string (e : t) =
      match e with
      | Msg v0 -> "MSG"
      | Maybe v0 -> "MAYBE"
      | Pair (v0, v1) -> "PAIR"
  
    let _ = kind_string
  
    let severity (e : t) =
      match e with
      | Msg v0 -> Soteria.Terminal.Diagnostic.Error
      | Maybe v0 -> Soteria.Terminal.Diagnostic.Error
      | Pair (v0, v1) -> Soteria.Terminal.Diagnostic.Error
  
    let _ = severity
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
  Success ✅
