open Prelude

type t = {
  a : S_int.t; [@fresh S_int.fresh] [@subst S_int.subst]
  b : Typed.T.sint Typed.t; [@exprs_syn fun _ -> []]
}
[@@deriving abstr { symex = Symex }]
