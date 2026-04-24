open Prelude

module Struct = struct
  type t = { foo : Foo.t; bar : Bar.t } [@@deriving reversible]
end

module Tuple = struct
  type t = Foo.t * Bar.t [@@deriving reversible]
end
