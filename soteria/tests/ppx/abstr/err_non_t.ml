open Prelude

type u = { off : S_int.t } [@@deriving abstr { symex = Symex }]
