open Prelude

type t = { heap : Heap.t option } [@@deriving sym_state { symex = Symex }]
