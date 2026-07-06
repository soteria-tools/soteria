open Prelude

type t = Only of Excl_int.t [@@deriving sym_state { symex = Symex }]
