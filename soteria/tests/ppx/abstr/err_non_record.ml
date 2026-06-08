open Prelude

type t = S_int.t [@@deriving abstr { symex = Symex }]
