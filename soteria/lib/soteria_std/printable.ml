(** Describes a type that can be pretty-printed. *)
module type S = sig
  type t

  val pp : Format.formatter -> t -> unit
end
