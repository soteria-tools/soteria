open Prelude

type t = { foo : int; bar : Bar.t } [@@deriving reversible]
