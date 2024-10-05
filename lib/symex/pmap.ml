module type KeyS = sig
  type t

  include Stdlib.Map.OrderedType with type t := t
end

module Make (Key : KeyS) = struct
  module M = Stdlib.Map.Make (Key)
end
