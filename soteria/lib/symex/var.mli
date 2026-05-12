open Soteria_std

(** Variable names are integers under the hood *)
type t

(** Iterator over variables and their types *)
type 'ty iter_vars = (t * 'ty -> unit) -> unit

(** Hashes the variable name *)
val hash : t -> int

(** Converts an integer into a variable. Identity function. *)
val of_int : int -> t

val to_int : t -> int

(** Sets up the variable name into an SMTlib-valid string name. This is the
    canonical, id-based encoding and is {b not} affected by user-facing names
    set through {!set_name}. *)
val to_string : t -> string

(** [of_string [to_string t]] is always [t], but the function may raise an
    exception if the input was not obtained through [to_string] *)
val of_string : string -> t

(** Pretty-print a variable. If a user-facing name has been associated with this
    variable through {!set_name}, that name is used; otherwise it falls back to
    {!to_string}. *)
val pp : Format.formatter -> t -> unit

(** {2 User-facing names and metadata}

    Variables can carry an optional user-facing name and a piece of opaque
    metadata. These are purely cosmetic: they {b never} influence the semantics
    of execution, SMT encoding, or hashing/equality on {!t}. They are intended
    to make path conditions easier to read and to let the host tool attach
    context (e.g. the source location at which a nondet value was produced).

    See {{:https://github.com/soteria-tools/soteria/issues/290}#290}. *)

(** Associates a user-facing name with [t]. Calling [set_name] twice on the same
    variable overrides the previously stored name. *)
val set_name : t -> string -> unit

(** Returns the user-facing name associated with [t], if any. *)
val name : t -> string option

(** Returns [true] if a user-facing name has been associated with [t], and
    [false] otherwise. *)
val has_name : t -> bool

(** Generic, opaque metadata attached to a variable. Encoded as a [string] here
    so that this interface can stay polymorphism-free; clients are expected to
    serialise their own structured metadata as needed. *)
val set_metadata : t -> string -> unit

val metadata : t -> string option
val equal : t -> t -> bool
val compare : t -> t -> int

module Incr_counter_mut : (_ : sig
                             val start_at : int
                           end)
  -> sig
  type var = t
  type t [@@mixins Reversible.Mutable]

  val get_next : t -> var
end

module Hashset : Hashset.S with type elt = t
module Set : PatriciaTree.SET with type elt = t
module Hashtbl : Hashtbl.S with type key = t
module Map : PatriciaTree.MAP with type key = t
