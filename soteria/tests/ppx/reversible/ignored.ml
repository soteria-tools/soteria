open Prelude

module Struct = struct
  type t = { foo : Foo.t; [@reversible.ignore] bar : Bar.t }
  [@@deriving reversible]
end

module Tuple = struct
  type t = (Foo.t[@reversible.ignore]) * Bar.t [@@deriving reversible]
end
