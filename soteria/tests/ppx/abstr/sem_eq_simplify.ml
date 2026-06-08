open Prelude

type t = {
  off : S_int.t;
  addr : Typed.T.sint Typed.t;
  exposed : bool; [@concrete]
}
[@@deriving abstr { symex = Symex; sem_eq = true; simplify = true }]
