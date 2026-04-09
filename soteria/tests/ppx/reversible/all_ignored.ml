open Prelude

module Struct = struct
  type t = { foo : Foo.t; [@ignore] bar : Bar.t [@ignore] }
  [@@deriving reversible]
end

module Tuple = struct
  type t = (Foo.t[@ignore]) * (Bar.t[@ignore]) [@@deriving reversible]
end
