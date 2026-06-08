open Prelude

type t = { off : S_int.t; size : int } [@@deriving abstr { symex = Symex }]
