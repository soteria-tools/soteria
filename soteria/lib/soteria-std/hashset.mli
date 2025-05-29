type 'a t

val with_capacity : int -> 'a t
val singleton : 'a -> 'a t
val add : 'a t -> 'a -> unit
val add_iter : 'a t -> 'a Iter.t -> unit
val mem : 'a t -> 'a -> bool
val remove : 'a t -> 'a -> unit
val iter : ('a -> unit) -> 'a t -> unit
val to_seq : 'a t -> 'a Seq.t
val of_seq : 'a Seq.t -> 'a t
val cardinal : 'a t -> int
val copy : 'a t -> 'a t
val subseteq : 'a t -> 'a t -> bool
val equal : 'a t -> 'a t -> bool
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

module type S = sig
  type elt
  type t

  val with_capacity : int -> t
  val singleton : elt -> t
  val add : t -> elt -> unit
  val add_iter : t -> elt Iter.t -> unit
  val mem : t -> elt -> bool
  val remove : t -> elt -> unit
  val iter : (elt -> unit) -> t -> unit
  val to_seq : t -> elt Seq.t
  val of_seq : elt Seq.t -> t
  val cardinal : t -> int
  val copy : t -> t
  val subseteq : t -> t -> bool
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module type PrintableHashedType = sig
  include Hashtbl.HashedType

  val pp : Format.formatter -> t -> unit
end

module Make (Elt : PrintableHashedType) : S with type elt = Elt.t
