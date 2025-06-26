(** Variable names are integers under the hood *)
type t

(** Iterator over variables and their types *)
type 'ty iter_vars = (t * 'ty -> unit) -> unit

(** Hashes the variable name *)
val hash : t -> int

(** Converts an integer into a variable. Identity function. *)
val of_int : int -> t

val to_int : t -> int

(** Sets up the variable name into an SMTlib-valid string name. *)
val to_string : t -> string

(** [of_string [to_string t]] is always [t], but the function may raise an
    exception if the input was not obtained through [to_string] *)
val of_string : string -> t

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val compare : t -> t -> int

module Incr_counter_mut : (_ : sig
                             val start_at : int
                           end)
  -> sig
  type var = t
  type t

  include Reversible.Mutable with type t := t

  val get_next : t -> var
end

module Hashset : Hashset.S with type elt = t
module Set : Set.S with type elt = t
module Hashtbl : Hashtbl.S with type key = t
module Map : Map.S with type key = t
