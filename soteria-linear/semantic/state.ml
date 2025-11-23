open Aux

type err = string [@@deriving show { with_path = false }]

module PMap = Soteria.Sym_states.Pmap.Make (Symex) (S_int)
module Excl = Soteria.Sym_states.Excl.Make (Symex)

type t = S_val.T.any S_val.t Excl.t PMap.t option
[@@deriving show { with_path = false }]

type fixes = S_val.T.any S_val.t Excl.serialized PMap.serialized
[@@deriving show { with_path = false }]

let empty : t = None
let load addr st = PMap.wrap Excl.load addr st
let store addr value st = PMap.wrap (Excl.store value) addr st

let alloc st =
  let v_false = (S_val.v_false :> S_val.T.any S_val.t) in
  PMap.alloc ~new_codom:v_false st

let free _addr _st = Symex.give_up ~loc:() "unimplemented: free"
