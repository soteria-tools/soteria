open Aux
include Soteria.Sym_states.Bi_abd.Make (Symex) (State)

let pp_spec ft (bi_state, pc, res) =
  let st, pre = of_opt bi_state in
  let post = Option.fold ~none:[] ~some:State.serialize st in
  Fmt.pf ft
    "@[<v>@[<2>Requires:@ %a@]@ @[<2>Ensures:@ %a@]@ @[<2>PC: %a@]@ %a@]"
    (Fmt.Dump.list State.pp_serialized)
    pre
    (Fmt.Dump.list State.pp_serialized)
    post
    (Fmt.list ~sep:(Fmt.any "@ && ") Symex.Value.ppa)
    pc
    (Fmt.Dump.result ~ok:S_val.pp ~error:State.pp_err)
    res

let empty = None
let load addr = wrap (State.load addr)
let store addr value = wrap (State.store addr value)
let alloc () st = wrap (State.alloc ()) st
let free addr = wrap (State.free addr)
