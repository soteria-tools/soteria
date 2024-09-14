module type S = sig
  type t
  type ty

  val fresh : ty -> t
  val not : t -> t
  val eq : t -> t -> t
  val pp : Format.formatter -> t -> unit
end
