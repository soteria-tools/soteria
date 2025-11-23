open Aux

type err = [ `UseAfterFree | `Interp of string ]
[@@deriving show { with_path = false }]

module PMap = Soteria.Sym_states.Pmap.Make (Symex) (S_int)
module Excl_val = Soteria.Sym_states.Excl.Make (Symex) (S_val)
module Freeable = Soteria.Sym_states.Freeable.Make (Symex)

type t = Excl_val.t PMap.t option [@@deriving show { with_path = false }]

type fixes = Excl_val.serialized PMap.serialized
[@@deriving show { with_path = false }]

let empty : t = None
let load addr st = PMap.wrap Excl_val.load addr st
let store addr value st = PMap.wrap (Excl_val.store value) addr st

let alloc st =
  let v_false = (S_val.v_false :> S_val.t) in
  PMap.alloc ~new_codom:v_false st

let free _addr _st = Symex.give_up ~loc:() "Free not implemented yet"
let error msg _state = `Interp msg
