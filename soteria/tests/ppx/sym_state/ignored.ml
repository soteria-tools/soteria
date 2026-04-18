open Prelude

type t = { heap : Heap.t option; steps : int [@sym_state.ignore { empty = 0 }] }
[@@deriving sym_state { symex = Symex }]
