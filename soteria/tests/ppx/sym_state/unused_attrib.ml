open Prelude

type t = { heap : Heap.t option [@foo] }
[@@deriving sym_state { symex = Symex }]
