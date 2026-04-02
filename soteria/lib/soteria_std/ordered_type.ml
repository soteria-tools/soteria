(** Extensions to [Stdlib.Map.OrderedType] with pretty-printing support. *)

module type S = sig
  include Stdlib.Map.OrderedType

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
