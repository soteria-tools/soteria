open Prelude

type t = { my_int : Excl_int.t option [@sym_state.ignore] }
[@@deriving sym_state { symex = Symex }]
