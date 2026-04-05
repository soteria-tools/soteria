open Prelude

type t = { foo : Foo.t; [@reversible.ignore] bar : Bar.t [@reversible.ignore] }
[@@deriving reversible]
