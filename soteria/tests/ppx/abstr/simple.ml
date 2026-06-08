open Prelude

type t = { off : S_int.t; addr : Typed.T.sint Typed.t }
[@@deriving abstr { symex = Symex }]
