(** A hash set implementation. *)

type 'a t

(** Create a new empty set with the given initial capacity. *)
val with_capacity : int -> 'a t

(** Create a set containing a single element. *)
val singleton : 'a -> 'a t

(** Add an element to the set. *)
val add : 'a t -> 'a -> unit

(** Add elements from an iterator to the set. *)
val add_iter : 'a t -> 'a Iter.t -> unit

(** Test whether an element is a member of the set. *)
val mem : 'a t -> 'a -> bool

(** Remove an element from the set. *)
val remove : 'a t -> 'a -> unit

(** Iterate over all elements in the set. *)
val iter : ('a -> unit) -> 'a t -> unit

(** Return a sequence of all elements in the set. *)
val to_seq : 'a t -> 'a Seq.t

(** Create a set from a sequence of elements. *)
val of_seq : 'a Seq.t -> 'a t

(** Return the number of elements in the set. *)
val cardinal : 'a t -> int

(** Return a copy of the set. *)
val copy : 'a t -> 'a t

(** Test whether the first set is a subset of the second set. *)
val subseteq : 'a t -> 'a t -> bool

(** Test whether two sets are equal. *)
val equal : 'a t -> 'a t -> bool

(** Pretty-printer for the set. *)
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

module type S = sig
  type elt
  type t

  (** Create a new empty set with the given initial capacity. *)
  val with_capacity : int -> t

  (** Create a set containing a single element. *)
  val singleton : elt -> t

  (** Add an element to the set. *)
  val add : t -> elt -> unit

  (** Add elements from an iterator to the set. *)
  val add_iter : t -> elt Iter.t -> unit

  (** Test whether an element is a member of the set. *)
  val mem : t -> elt -> bool

  (** Remove an element from the set. *)
  val remove : t -> elt -> unit

  (** Iterate over all elements in the set. *)
  val iter : (elt -> unit) -> t -> unit

  (** Return a sequence of all elements in the set. *)
  val to_seq : t -> elt Seq.t

  (** Create a set from a sequence of elements. *)
  val of_seq : elt Seq.t -> t

  (** Return the number of elements in the set. *)
  val cardinal : t -> int

  (** Return a copy of the set. *)
  val copy : t -> t

  (** Test whether the first set is a subset of the second set. *)
  val subseteq : t -> t -> bool

  (** Test whether two sets are equal. *)
  val equal : t -> t -> bool

  (** Pretty-printer for the set. *)
  val pp : Format.formatter -> t -> unit
end

module type PrintableHashedType = sig
  include Hashtbl.HashedType

  (** Pretty-printer for elements. *)
  val pp : Format.formatter -> t -> unit
end

(** Functor to create a hash set module for a given element type. *)
module Make (Elt : PrintableHashedType) : S with type elt = Elt.t

(** Hash set implementation for strings. *)
module Hstring : S with type elt = string

(** Hash set implementation for integers. *)
module Hint : S with type elt = int
