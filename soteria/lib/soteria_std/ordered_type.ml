module type S = sig
  include Stdlib.Map.OrderedType

  val pp : Format.formatter -> t -> unit
end
