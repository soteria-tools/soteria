open Prelude

type state = { heap : Heap.t option } [@@deriving sym_state { symex = Symex }]
