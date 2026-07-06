open Prelude

type t = Cell of Excl_int.t | Map of Heap.t
[@@deriving sym_state { symex = Symex }]
