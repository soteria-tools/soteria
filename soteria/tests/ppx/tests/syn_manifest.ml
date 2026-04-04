open Prelude

module Syn = struct
  type t = Ser_heap of Heap.syn
end

type t = { heap : Heap.t option; steps : int [@sym_state.ignore { empty = 0 }] }
[@@deriving sym_state { symex = Symex; syn = Syn.t }]
