open Prelude

type t = Bad of Excl_int.t option [@@deriving sym_state { symex = Symex }]
