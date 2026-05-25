(** Symbolic variable identifiers.

    A {!t} is the name of a symbolic variable produced by the symbolic
    execution engine (typically by {{!Soteria.Symex.S.nondet}nondet}). Variables
    are represented as machine integers, so equality and hashing are cheap. The
    module also provides specialised hash sets, Patricia-tree sets/maps, and a
    reversible monotonic counter used by the engine to allocate fresh
    identifiers. *)

open Soteria_std

(** Variable names are integers under the hood. *)
type t

(** [iter_vars] is the type of effectful iterators yielding each variable
    together with its type. It is the shape used throughout Soteria to walk the
    free variables of an expression. *)
type 'ty iter_vars = (t * 'ty -> unit) -> unit

(** Hashes the variable name. *)
val hash : t -> int

(** Converts an integer into a variable. Identity function. *)
val of_int : int -> t

(** Converts a variable into its underlying integer. Identity function. *)
val to_int : t -> int

(** Sets up the variable name into an SMTlib-valid string name. *)
val to_string : t -> string

(** [of_string (to_string t)] is always [t], but the function may raise an
    exception if the input was not obtained through {!to_string}. *)
val of_string : string -> t

(** Pretty-prints a variable using its SMTlib-valid name (see {!to_string}). *)
val pp : Format.formatter -> t -> unit

(** Structural equality on variables. *)
val equal : t -> t -> bool

(** Total order on variables (the underlying integer order). *)
val compare : t -> t -> int

(** A reversible mutable counter producing fresh variables, starting at
    [Start_at.start_at]. The {{!Soteria_std.Reversible.Mutable} Reversible}
    interface allows symbolic execution to roll back the counter when
    backtracking across branches, ensuring that variable identifiers stay
    consistent with the path being explored. *)
module Incr_counter_mut : (_ : sig
                             val start_at : int
                           end)
  -> sig
  type var = t
  type t [@@mixins Reversible.Mutable]

  (** [get_next c] returns the next fresh variable and advances [c]. *)
  val get_next : t -> var
end

(** Hash set specialised for {!t}. *)
module Hashset : Hashset.S with type elt = t

(** Patricia-tree set specialised for {!t}. *)
module Set : PatriciaTree.SET with type elt = t

(** Hash table specialised for {!t}. *)
module Hashtbl : Hashtbl.S with type key = t

(** Patricia-tree map specialised for {!t}. *)
module Map : PatriciaTree.MAP with type key = t
