open Prelude

type t = { steps : int [@sym_state.ignore { empty = 0 }] }
[@@deriving sym_state { symex = Symex }]
