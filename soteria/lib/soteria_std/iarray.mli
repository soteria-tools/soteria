include module type of Stdlib.Iarray

(** Equivalent to {!fold_left}; we re-export is as [fold] for compatibility with
    {!Sigs.Foldable}. *)
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Creates a copy of this array, with the given function applied to a mutable
    copy of the underlying array, before making it immutable.

    This is more efficient than {!mapi}, as only the strictly necessary work is
    performed by [f], rather than an entire iteration. It is also more
    performant than a {!to_array} + {!of_array} roundtrip, as it only copies the
    array once. *)
val copy_and_update : ('a array -> unit) -> 'a t -> 'a t

(** Creates a copy of this array, with the given index set to the given value.
*)
val copy_and_set : int -> 'a -> 'a t -> 'a t

val pp :
  ?sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a t ->
  unit

module Infix : sig
  val ( .%() ) : 'a t -> int -> 'a
end
