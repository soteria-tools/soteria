open Prelude

module Syn = struct
  type 'a t = Ser_heap of 'a
end

type t = { heap : Heap.t option; steps : int [@sym_state.ignore { empty = 0 }] }
[@@deriving sym_state { symex = Symex; syn = Heap.syn Syn.t }]
