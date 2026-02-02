(** Polymorphic map interfaces for symbolic state models.

    This module defines the interfaces for polymorphic maps used in symbolic
    state models. It provides:
    - A standard map interface ({!module-type:MapS})
    - Key requirements for symbolic execution ({!Key})
    - A complete polymorphic map state model interface ({!M})

    These interfaces are used to build state models that map symbolic keys
    (e.g., addresses, indices) to symbolic values, supporting operations
    like allocation, lookup, and bi-abduction.

    {1 Architecture}

    The module is organized in three layers:
    - {b MapS}: Basic map operations (similar to [Stdlib.Map.S])
    - {b Key}: Requirements for keys in symbolic execution (equality, freshness)
    - {b M}: Full state model interface with produce/consume semantics *)

open Symex

(** {1 Basic Map Interface} *)

(** Signature for a basic polymorphic map.

    This is a simplified version of [Stdlib.Map.S] with the essential
    operations needed for state models. *)
module type MapS = sig
  (** The type of map keys. *)
  type key

  (** The type of maps from keys to values of type ['a]. *)
  type 'a t

  (** The empty map. *)
  val empty : 'a t

  (** [is_empty m] returns [true] if [m] contains no bindings. *)
  val is_empty : 'a t -> bool

  (** [add k v m] returns a map containing the same bindings as [m],
      plus a binding of [k] to [v]. If [k] was already bound in [m],
      its previous binding is replaced. *)
  val add : key -> 'a -> 'a t -> 'a t

  (** [add_seq s m] adds the bindings from sequence [s] to [m].
      Later bindings in the sequence override earlier ones for the same key. *)
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

  (** [update k f m] returns a map containing the same bindings as [m],
      except for the binding of [k]. The new binding for [k] is determined
      by [f (find_opt k m)]. *)
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  (** [mem k m] returns [true] if [m] contains a binding for [k]. *)
  val mem : key -> 'a t -> bool

  (** [find_opt k m] returns [Some v] if [k] is bound to [v] in [m],
      or [None] if [k] is not bound. *)
  val find_opt : key -> 'a t -> 'a option

  (** [iter f m] applies [f] to all bindings in [m]. *)
  val iter : (key -> 'a -> unit) -> 'a t -> unit

  (** [to_seq m] returns the sequence of all bindings in [m]. *)
  val to_seq : 'a t -> (key * 'a) Seq.t
end

(** {1 Symbolic Key Requirements} *)

(** Functor providing key signatures for symbolic execution.

    Keys in symbolic maps must support:
    - Ordering (for efficient map implementations)
    - Semantic equality (for symbolic comparison)
    - Fresh generation (for allocation)
    - Variable substitution (for serialization)

    @param Symex The symbolic execution module *)
module Key (Symex : Symex.Base) : sig
  (** Signature for symbolic map keys.

      Keys must be orderable (for the underlying map structure) and
      support symbolic operations like semantic equality and freshness. *)
  module type S = sig
    (** The type of keys. *)
    type t

    include Stdlib.Map.OrderedType with type t := t

    (** Abbreviation for symbolic boolean values. *)
    type sbool_v := Symex.Value.(sbool t)

    (** Pretty-print a key. *)
    val pp : Format.formatter -> t -> unit

    (** [sem_eq k1 k2] returns a symbolic boolean representing
        whether [k1] and [k2] are semantically equal.

        Note: This is different from structural equality ([compare]).
        Two keys may be structurally different but semantically equal
        (e.g., [x] and [x + 0]). *)
    val sem_eq : t -> t -> sbool_v

    (** [fresh ()] generates a fresh symbolic key.

        Used during allocation to create new unique keys. *)
    val fresh : unit -> t Symex.t

    (** [simplify k] simplifies a key using the current path constraints.

        May use the solver to determine constant values. *)
    val simplify : t -> t Symex.t

    (** [distinct ks] returns a symbolic boolean asserting that all
        keys in [ks] are pairwise distinct.

        Useful for asserting non-aliasing between allocations. *)
    val distinct : t list -> sbool_v

    (** [subst f k] substitutes variables in [k] according to [f]. *)
    val subst : (Var.t -> Var.t) -> t -> t

    (** [iter_vars k] iterates over all variables in key [k]. *)
    val iter_vars : t -> 'a Symex.Value.ty Var.iter_vars
  end

  (** Extended key signature for Patricia tree implementations.

      Patricia trees require keys to be convertible to integers
      for efficient trie-based storage. *)
  module type S_patricia_tree = sig
    include S

    (** [to_int k] converts key [k] to an integer for Patricia tree indexing.

        This should be a hash-like function that distributes keys evenly. *)
    val to_int : t -> int
  end
end

(** {1 Polymorphic Map State Model} *)

(** Functor for creating polymorphic map state model signatures.

    This creates a complete state model interface for maps from [Key.t]
    to some codomain type. The resulting module supports:
    - Allocation of new entries
    - Wrapped access to entries (with key lookup)
    - Serialization for bi-abduction
    - Produce/consume operations for separation logic

    @param Symex The symbolic execution module
    @param Key The key type module *)
module M
    (Symex : Symex.Base)
    (Key : sig
      type t
    end) : sig
  (** Signature for a polymorphic map state model.

      This is a complete state model that maps keys to codomains,
      supporting all the operations needed for symbolic execution
      with bi-abduction. *)
  module type S = sig
    (** {2 Types} *)

    (** The type of values stored in the map (codomain). *)
    type codom

    (** The type of the map state. *)
    type t

    (** Serialized form of a single codomain value.

        Used for bi-abduction's missing resource specifications. *)
    type codom_serialized

    (** Serialized form of a map entry (key-value pair).

        A serialized entry is a pair of the key and the serialized codomain. *)
    type serialized = Key.t * codom_serialized

    (** {2 State Monad} *)

    (** The state monad for map operations.

        Operations on the map are performed through this state monad,
        which threads the map state through computations. *)
    module SM :
      State_monad.S
        with module Symex = Symex
         and module Value = Symex.Value
         and type st = t option

    (** {2 Type Abbreviations} *)

    (** Result type for map operations.

        Equivalent to [('a, 'err, serialized list) SM.Result.t]. *)
    type ('a, 'err) res := ('a, 'err, serialized list) SM.Result.t

    (** Result type for codomain operations.

        Takes an optional codomain and returns the result plus updated codomain. *)
    type ('a, 'err) codom_res :=
      codom option ->
      (('a, 'err, codom_serialized list) Compo_res.t * codom option) Symex.t

    (** {2 Basic Operations} *)

    (** The empty map. *)
    val empty : t

    (** [syntactic_bindings m] returns all bindings in [m] as a sequence.

        Note: This is syntactic - it returns all stored bindings without
        considering symbolic equality of keys. *)
    val syntactic_bindings : t -> (Key.t * codom) Seq.t

    (** [syntactic_mem k m] returns [true] if [k] is syntactically present in [m].

        Note: This checks structural equality only, not semantic equality. *)
    val syntactic_mem : Key.t -> t -> bool

    (** {2 Pretty Printing} *)

    (** [pp' ?key ?codom ?ignore fmt m] pretty-prints map [m] with customization.

        @param key Custom key printer (optional)
        @param codom Custom codomain printer (optional)
        @param ignore Predicate to filter out bindings from output (optional) *)
    val pp' :
      ?key:(Format.formatter -> Key.t -> unit) ->
      ?codom:(Format.formatter -> codom -> unit) ->
      ?ignore:(Key.t * codom -> bool) ->
      Format.formatter ->
      t ->
      unit

    (** Pretty-print a map with default formatting. *)
    val pp : Format.formatter -> t -> unit

    (** Convert a map to a string representation. *)
    val show : t -> string

    (** Pretty-print a serialized entry. *)
    val pp_serialized : Format.formatter -> serialized -> unit

    (** Convert a serialized entry to a string. *)
    val show_serialized : serialized -> string

    (** {2 Serialization} *)

    (** [serialize m] converts map [m] to a list of serialized entries.

        Used for bi-abduction to specify missing resources. *)
    val serialize : t -> serialized list

    (** [subst_serialized f s] substitutes variables in serialized entry [s]. *)
    val subst_serialized : (Var.t -> Var.t) -> serialized -> serialized

    (** [iter_vars_serialized s f] iterates over all variables in entry [s]. *)
    val iter_vars_serialized :
      serialized -> (Var.t * 'b Symex.Value.ty -> unit) -> unit

    (** {2 Option Conversions} *)

    (** [of_opt opt] converts [None] to empty map, [Some m] to [m]. *)
    val of_opt : t option -> t

    (** [to_opt m] returns [None] if [m] is empty, [Some m] otherwise. *)
    val to_opt : t -> t option

    (** {2 Allocation} *)

    (** [alloc ~new_codom] allocates a new entry with the given codomain.

        Returns a fresh key bound to [new_codom]. *)
    val alloc : new_codom:codom -> (Key.t, 'err) res

    (** [allocs ~fn ~els] allocates multiple entries.

        For each element in [els], calls [fn el key] where [key] is freshly
        generated, and stores the resulting codomain in the map.

        @param fn Function mapping elements and keys to (result, codomain) pairs
        @param els List of elements to allocate *)
    val allocs :
      fn:('a -> Key.t -> ('k * codom) Symex.t) ->
      els:'a list ->
      ('k list, 'err) res

    (** {2 Wrapped Access} *)

    (** [wrap key f] looks up [key] in the map and applies [f] to its codomain.

        This is the primary way to access and modify entries in the map.
        The function [f] receives the codomain (if present) and returns
        a result plus potentially updated codomain. *)
    val wrap : Key.t -> ('a, 'err) codom_res -> ('a, 'err) res

    (** {2 Folding} *)

    (** [fold f acc opt_m] folds over all entries in [opt_m].

        @param f Folding function receiving accumulator and (key, codomain) pair
        @param acc Initial accumulator
        @param opt_m Optional map to fold over *)
    val fold :
      ('acc -> Key.t * codom -> ('acc, 'err, serialized list) Symex.Result.t) ->
      'acc ->
      t option ->
      ('acc, 'err, serialized list) Symex.Result.t

    (** {2 Separation Logic Operations} *)

    (** [produce s opt_m] adds serialized entry [s] to [opt_m].

        Used in bi-abduction to produce (add) resources to the state. *)
    val produce : serialized -> t option -> (unit * t option) Symex.t
  end
end
