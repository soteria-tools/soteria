(** Persistent maps and sets keyed by an integer-encodable type, implemented as
    big-endian patricia tries.

    {b Structural sharing.} Operations that do not change the container return a
    physically equal container, and operations that change it share untouched
    subtree with the input. *)

(** Keys are identified by an integer, which determines their position in the
    trie. *)
module type Key = sig
  type t

  (** Must be injective: [to_int a = to_int b] implies that [a] and [b] are
      interchangeable as keys. Lookups only ever compare keys through [to_int].
  *)
  val to_int : t -> int

  val pp : Format.formatter -> t -> unit
end

module type Map = sig
  type key
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  (** Number of bindings. For a weak map, this counts the bindings that have not
      been collected yet, and is therefore only meaningful right after
      {!compact}. *)
  val cardinal : 'a t -> int

  val singleton : key -> 'a -> 'a t
  val mem : key -> 'a t -> bool

  (** @raise Not_found if the key is unbound. *)
  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  (** Adds a binding, replacing any existing one. *)
  val add : key -> 'a -> 'a t -> 'a t

  (** [add_assert_new k v m] is [add k v m], but raises [Invalid_argument] if
      [k] is already bound in [m]. *)
  val add_assert_new : key -> 'a -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  (** [update k f m] rebinds [k] to [f (find_opt k m)], removing the binding
      when [f] returns [None]. Must return [m] itself when the result of [f] is
      physically equal to its argument. *)
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  (** [update_from guide f m] updates [m] at every key bound in [guide]: each
      binding [(k, y)] of [guide] rebinds [k] to [f k (find_opt k m) y],
      removing the binding when [f] returns [None]. Keys bound in [m] but not in
      [guide] are left untouched.

      This descends both tries simultaneously in a single pass, rather than
      iterating over [guide] and updating [m] key by key. *)
  val update_from :
    'b t -> (key -> 'a option -> 'b -> 'a option) -> 'a t -> 'a t

  (** [union f m1 m2] keeps the bindings of both maps, resolving conflicts with
      [f]. [f] must be idempotent ([f k v v = v]) so that shared subtrees can be
      returned without being traversed. *)
  val union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** [inter f m1 m2] keeps only the keys bound in both maps, combining values
      with [f]. [f] must be idempotent ([f k v v = v]), as for {!union}. *)
  val inter : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** [equal eq m1 m2] tests whether the two maps bind the same keys to
      equivalent values. [eq] must be reflexive, so that physically equal
      subtrees can be skipped. *)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val to_seq : 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

  (** Rebuilds the trie from its live bindings, discarding the internal nodes
      left behind by collected entries. This is the identity (and should be
      [O(1)]) for a strong map; for a weak map it is what makes traversals
      proportional to the number of live bindings again. *)
  val compact : 'a t -> 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type Set = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val equal : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val to_list : t -> elt list
  val compact : t -> t
  val pp : Format.formatter -> t -> unit
end

module MakeMap (Key : Key) : Map with type key = Key.t
module MakeSet (Key : Key) : Set with type elt = Key.t
module MakeWeak (Key : Key) : Map with type key = Key.t
module MakeWeakSet (Key : Key) : Set with type elt = Key.t
