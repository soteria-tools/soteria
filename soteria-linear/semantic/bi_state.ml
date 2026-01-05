open Aux
module Bi = Soteria.Sym_states.Bi_abd.Make (Symex)

type t = (State.t, State.syn) Bi.t [@@deriving show { with_path = false }]
type syn = State.syn [@@deriving show { with_path = false }]
type fixes = State.syn list [@@deriving show { with_path = false }]
type err = State.err * t [@@deriving show { with_path = false }]

let pp_spec ft ((st, pre), pc, res) =
  let post = State.to_syn st in
  Fmt.pf ft
    "@[<v>@[<2>Requires:@ %a@]@ @[<2>Ensures:@ %a@]@ @[<2>PC: %a@]@ %a@]"
    pp_fixes pre pp_fixes post
    (Fmt.list ~sep:(Fmt.any "@ && ") Symex.Value.ppa)
    pc
    (Fmt.Dump.result ~ok:S_val.pp ~error:State.pp_err)
    res

let empty = (State.empty, [])
let bi_wrap f = (Bi.wrap ~produce:State.produce) f

let load addr : t -> (S_val.t * t, err, syn list) Symex.Result.t =
  bi_wrap (State.load addr)

let store addr value = bi_wrap (State.store addr value)
let alloc st = bi_wrap State.alloc st
let free addr = bi_wrap (State.free addr)
let produce fix t = Bi.produce State.produce fix t
let consume fix t = Bi.consume ~produce:State.produce State.consume fix t
let error msg state = (`Interp msg, state)
