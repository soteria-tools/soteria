type 'a t

val create : int -> 'a t
val add : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool
val remove : 'a t -> 'a -> unit
val iter : ('a -> unit) -> 'a t -> unit
val to_seq : 'a t -> 'a Seq.t
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

module type S = sig
  type elt
  type t

  val create : int -> t
  val add : t -> elt -> unit
  val mem : t -> elt -> bool
  val remove : t -> elt -> unit
  val iter : (elt -> unit) -> t -> unit
  val to_seq : t -> elt Seq.t
  val pp : Format.formatter -> t -> unit
end

module type PrintableHashedType = sig
  include Hashtbl.HashedType

  val pp : Format.formatter -> t -> unit
end

module Make (Elt : PrintableHashedType) : S with type elt = Elt.t
