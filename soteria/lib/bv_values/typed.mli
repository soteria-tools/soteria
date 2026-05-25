(** Typed wrapper around the bit-vector value representation [Svalue].

    This module is the most expressive of the two built-in value
    representations (the other being {!Soteria.Tiny_values}). It uses
    {{:https://v2.ocaml.org/manual/polyvariant.html}polymorphic variant phantom
    types} to track, in the OCaml type system, what {e kind} a symbolic value
    has — integer, boolean, pointer, location, float, or sequence — and, for
    integers, whether they are known to be zero, non-zero, or possibly the
    result of an overflowing operation. This means many type-correctness
    checks normally done at runtime (e.g. "you cannot divide by a value that
    might be zero") are caught statically.

    Operations come in two flavours: the [Bool], [BitVec], [Float] and [Ptr]
    sub-modules expose the underlying SMT-style operations explicitly, and
    {!Infix} re-exports a curated subset using overloaded operators ([+@],
    [<@], etc.) that read like plain OCaml arithmetic. *)

(** {2 Phantom types}

    The types in {!T} are used as phantom parameters of {!t}. They are not
    inhabited at runtime — they only constrain how values can be combined. *)

module T : sig
  (** A symbolic integer; can either be [`NonZero] if it is known to not be 0,
      [`Zero] if it is 0. *)
  type sint = [ `NonZero | `Zero ]

  (** Any symbolic integer; it may be the result of an overflowing operation *)
  type sint_ovf = [ `NonZero | `Zero | `Overflowed ]

  (** A symbolic integer known to be non-zero. *)
  type nonzero = [ `NonZero ]

  (** A symbolic integer known to be zero. *)
  type zero = [ `Zero ]

  (** A symbolic floating-point value. *)
  type sfloat = [ `Float ]

  (** A symbolic boolean. *)
  type sbool = [ `Bool ]

  (** A symbolic pointer (a location and an offset). *)
  type sptr = [ `Ptr ]

  (** A symbolic memory location. *)
  type sloc = [ `Loc ]

  (** A symbolic sequence of values of phantom kind ['a]. *)
  type 'a sseq = [ `List of 'a ]

  (** A "C value" — the kinds that can be stored in a C lvalue. *)
  type cval = [ sint | sptr | sfloat ]

  (** The union of all kinds, used to constrain operations that work on any
      symbolic value (such as equality). *)
  type any = [ sint_ovf | sfloat | sbool | sptr | sloc | any sseq ]

  val pp_sint : Format.formatter -> sint -> unit
  val pp_sint_ovf : Format.formatter -> sint_ovf -> unit
  val pp_nonzero : Format.formatter -> nonzero -> unit
  val pp_zero : Format.formatter -> zero -> unit
  val pp_sfloat : Format.formatter -> sfloat -> unit
  val pp_sbool : Format.formatter -> sbool -> unit
  val pp_sptr : Format.formatter -> sptr -> unit
  val pp_sloc : Format.formatter -> sloc -> unit
  val pp_cval : Format.formatter -> cval -> unit

  val pp_sseq :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a sseq -> unit

  val pp_any : Format.formatter -> any -> unit
  val hash_sint : sint -> int
  val hash_sint_ovf : sint_ovf -> int
  val hash_nonzero : nonzero -> int
  val hash_zero : zero -> int
  val hash_sfloat : sfloat -> int
  val hash_sbool : sbool -> int
  val hash_sptr : sptr -> int
  val hash_sloc : sloc -> int
  val hash_cval : cval -> int
  val hash_sseq : 'a sseq -> int
  val hash_any : any -> int
end

open T

(** {2 Types}

    A {!ty} describes the "shape" of a symbolic value at the SMT level (e.g.
    "an 8-bit integer", "a 64-bit pointer", "a boolean"). The phantom
    parameter agrees with the kind tag carried by values of that type. *)

(** Runtime representation of a symbolic type, carrying its bit-width when
    relevant. *)
type +'a ty

(** Pretty-prints a type using the supplied printer for any inner kind tag. *)
val pp_ty :
  (Format.formatter -> 'a ty -> unit) -> Format.formatter -> 'a ty -> unit

(** Same as {!pp_ty}, but uses a default printer for the inner kind tag. *)
val ppa_ty : Format.formatter -> 'a ty -> unit

(** Heterogeneous equality on types: two types are equal if they describe the
    same SMT sort, regardless of their phantom parameters. *)
val equal_ty : 'a ty -> 'b ty -> bool

(** The boolean type. *)
val t_bool : [> sbool ] ty

(** [t_int n] is the type of [n]-bit integers. *)
val t_int : int -> [> sint ] ty

(** [t_ptr n] is the type of pointers whose offset has [n] bits. *)
val t_ptr : int -> [> sptr ] ty

(** [t_loc n] is the type of locations represented on [n] bits. *)
val t_loc : int -> [> sloc ] ty

(** Sequence type, parameterised by its element type. *)
val t_seq : ([< any ] as 'a) ty -> [> 'a sseq ] ty

(** IEEE-754 binary16 float type. *)
val t_f16 : [> sfloat ] ty

(** IEEE-754 binary32 float type. *)
val t_f32 : [> sfloat ] ty

(** IEEE-754 binary64 float type. *)
val t_f64 : [> sfloat ] ty

(** IEEE-754 binary128 float type. *)
val t_f128 : [> sfloat ] ty

(** [t_float fp] is the float type at the given precision. *)
val t_float : Svalue.FloatPrecision.t -> [> sfloat ] ty

(** {2 Typed svalues}

    A value of type ['a t] is a symbolic value of phantom kind ['a]. The
    underlying representation is shared with [Svalue.t]; this module provides
    the typed view, with smart constructors that maintain the phantom-type
    invariants. *)

type +'a t
type sbool = T.sbool

(** {3 Basic value operations} *)

(** Whether the supplied type is the boolean type. *)
val is_bool_ty : 'a ty -> bool

(** Returns the underlying untyped representation of a value's type. *)
val get_ty : 'a t -> Svalue.ty

(** Wraps an untyped [Svalue.ty] as a typed {!type-ty}. The caller is
    responsible for choosing a phantom parameter consistent with the type. *)
val type_type : Svalue.ty -> 'a ty

(** Inverse of {!type_type}: forgets the phantom parameter. *)
val untype_type : 'a ty -> Svalue.ty

(** Returns the [Svalue.t_kind] of a value (whether it is a literal, a
    variable, an application of a symbol, etc.). *)
val kind : 'a t -> Svalue.t_kind

(** [mk_var v ty] constructs a symbolic variable named [v] at type [ty]. *)
val mk_var : Svalue.Var.t -> 'a ty -> 'a t

(** Iterates over the free variables of a value, yielding each variable and
    its type. *)
val iter_vars : 'a t -> (Svalue.Var.t * 'b ty -> unit) -> unit

(** Unsafely wraps an untyped [Svalue.t] as a typed value, asserting that its
    phantom kind is ['a]. Prefer {!type_checked} when the kind is not known to
    match. *)
val type_ : Svalue.t -> 'a t

(** [type_checked v ty] is [Some v'] if [v] has type [ty], and [None]
    otherwise. *)
val type_checked : Svalue.t -> 'a ty -> 'a t option

(** Unsafe cast between phantom kinds. *)
val cast : 'a t -> 'b t

(** [cast_checked v ty] checks that [v] has type [ty] before casting it,
    returning [None] otherwise. *)
val cast_checked : 'a t -> 'b ty -> 'b t option

(** [cast_checked2 a b] succeeds iff [a] and [b] have the same SMT type, and
    returns them at a common phantom kind together with that type. *)
val cast_checked2 : 'a t -> 'b t -> ('c t * 'c t * 'c ty) option

(** Casts a value to a float kind if it has one, returning [None] otherwise. *)
val cast_float : 'a t -> [> sfloat ] t option

(** Casts a value to an integer kind if it has one, returning the value and
    its bit-width. *)
val cast_int : 'a t -> ([> sint ] t * int) option

(** Whether the supplied type is a floating-point type. *)
val is_float : 'a ty -> bool

(** Bit-width of an integer value. *)
val size_of_int : [< sint ] t -> int

(** Drops the phantom type, exposing the underlying untyped value. *)
val untyped : 'a t -> Svalue.t

(** Drops the phantom type on a whole list. *)
val untyped_list : 'a t list -> Svalue.t list

(** Pretty-prints a value using the supplied printer for any inner kind tag. *)
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** Same as {!pp}, but uses a default printer for the inner kind tag. *)
val ppa : Format.formatter -> 'a t -> unit

(** Syntactic equality (not semantic — use {!sem_eq} to obtain a symbolic
    boolean asserting semantic equality). *)
val equal : ([< any ] as 'a) t -> 'a t -> bool

(** Total order consistent with {!equal}. *)
val compare : ([< any ] as 'a) t -> 'a t -> int

(** Hashes a value, given a hash function for the inner kind tag. *)
val hash : ('a -> int) -> 'a t -> int

(** Same as {!hash}, but uses a default hash function for the inner kind tag.
*)
val hasha : 'a t -> int

(** Returns a unique tag identifying a value, exploiting hash-consing. Two
    values are syntactically equal iff their tags are equal. *)
val unique_tag : [< any ] t -> int

(** {3 Typed constructors} *)

(** [sem_eq a b] is the symbolic boolean asserting that [a] and [b] are
    semantically equal. *)
val sem_eq : 'a t -> 'a t -> sbool t

(** Like {!sem_eq}, but operates on heterogeneously-typed inputs. *)
val sem_eq_untyped : 'a t -> 'b t -> sbool t

(** {3 Boolean operations}

    The local signature [Bool_] below is shared between this module (where
    booleans live alongside other kinds) and the dedicated {!module-Bool}
    sub-module. *)

module type Bool_ := sig
  (** The concrete true value. *)
  val v_true : [> sbool ] t

  (** The concrete false value. *)
  val v_false : [> sbool ] t

  (** Lifts a host boolean to a symbolic boolean. *)
  val of_bool : bool -> [> sbool ] t

  (** [to_bool v] is [Some b] if [v] is concretely the boolean [b]. *)
  val to_bool : 'a t -> bool option

  (** Symbolic conjunction. *)
  val and_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t

  (** Similar to [and_], but the rhs is only evaluated if the lhs is not the
      concrete false. In other words, this is a short-circuiting and. Avoids
      some errors, like a division by zero in [0 != x && n / x] when [x] is [0].
  *)
  val and_lazy : [< sbool ] t -> (unit -> [< sbool ] t) -> [> sbool ] t

  (** Conjunction of an arbitrary list of booleans. *)
  val conj : [< sbool ] t list -> [> sbool ] t

  (** Calls the supplied function on each top-level conjunct of the value, or
      on the value itself if it is not a conjunction. *)
  val split_ands : [< sbool ] t -> ([> sbool ] t -> unit) -> unit

  (** Symbolic disjunction. *)
  val or_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t

  (** Similar to [or_], but the rhs is only evaluated if the lhs is not the
      concrete true. In other words, this is a short-circuiting or. Avoids some
      errors, like a division by zero in [0 == x || n / x] when [x] is [0]. *)
  val or_lazy : [< sbool ] t -> (unit -> [< sbool ] t) -> [> sbool ] t

  (** Symbolic negation. *)
  val not : [< sbool ] t -> [> sbool ] t

  (** [distinct l] asserts that all values in [l] are pairwise distinct. *)
  val distinct : 'a t list -> [> sbool ] t

  (** [ite c a b] is the if-then-else: [a] when [c] holds, [b] otherwise. *)
  val ite : [< sbool ] t -> 'a t -> 'a t -> 'a t

  (** [exists_1 ~not_in ty f] introduces a fresh variable of type [ty] that
      does not occur in [not_in], and produces [f v]. *)
  val exists_1 : not_in:_ t -> 'a ty -> ('a t -> [< sbool ] t) -> [> sbool ] t

  (** Variant of {!exists_1} introducing two existentially-quantified
      variables. *)
  val exists_2 :
    not_in:_ t ->
    'a ty ->
    'b ty ->
    ('a t -> 'b t -> [< sbool ] t) ->
    [> sbool ] t

  (** Variant of {!exists_1} introducing three existentially-quantified
      variables. *)
  val exists_3 :
    not_in:_ t ->
    'a ty ->
    'b ty ->
    'c ty ->
    ('a t -> 'b t -> 'c t -> [< sbool ] t) ->
    [> sbool ] t
end

include Bool_

(** Same operations as the [Bool_] signature above, exposed as a stand-alone
    module with its own [t = sbool] alias. *)
module Bool : sig
  include Bool_

  type t = sbool
end

(** {3 Bit-vector operations}

    All integer values are bit-vectors of a fixed width. Most arithmetic
    operations return values of kind {!T.sint_ovf}, since they may overflow;
    use {!Infix.( +!@ )} (and friends) for variants that ignore overflow. *)
module BitVec : sig
  (** {4 Constructors} *)

  (** [mk n z] is the [n]-bit integer with value [z]. Raises if [z] does not
      fit in [n] bits. *)
  val mk : int -> Z.t -> [> sint ] t

  (** Like {!mk}, but masks [z] to [n] bits instead of raising. *)
  val mk_masked : int -> Z.t -> [> sint ] t

  (** Like {!mk}, but takes a host integer. *)
  val mki : int -> int -> [> sint ] t

  (** Like {!mk_masked}, but takes a host integer. *)
  val mki_masked : int -> int -> [> sint ] t

  (** [mk_nz n z] asserts statically that [z] is non-zero. *)
  val mk_nz : int -> Z.t -> [> nonzero ] t

  (** Like {!mk_nz}, but takes a host integer. *)
  val mki_nz : int -> int -> [> nonzero ] t

  (** [zero n] is the [n]-bit zero. *)
  val zero : int -> [> zero ] t

  (** [one n] is the [n]-bit one. *)
  val one : int -> [> nonzero ] t

  (** [bv_to_z signed n z] interprets the [n]-bit bitvector pattern [z] as a
      Z integer, signed or unsigned. *)
  val bv_to_z : bool -> int -> Z.t -> Z.t

  (** [to_z v] is [Some z] when [v] is concretely the bitvector [z], and
      [None] otherwise. *)
  val to_z : [< any ] t -> Z.t option

  (** {4 Arithmetic}

      All arithmetic operations produce {!T.sint_ovf} results, i.e. values
      that may have wrapped on overflow. Pass [~checked:true] to compute
      arithmetic with explicit overflow-checking semantics. *)

  val add : ?checked:bool -> [< sint ] t -> [< sint ] t -> [> sint_ovf ] t
  val sub : ?checked:bool -> [< sint ] t -> [< sint ] t -> [> sint_ovf ] t
  val mul : ?checked:bool -> [< sint ] t -> [< sint ] t -> [> sint_ovf ] t

  (** [div ~signed a b] is integer division. The phantom kind of [b] requires
      it to be statically known non-zero. *)
  val div : signed:bool -> [< sint ] t -> [< nonzero ] t -> [> sint_ovf ] t

  (** [rem ~signed a b] is the remainder of integer division, with C-style
      truncation semantics when [signed]. *)
  val rem : signed:bool -> [< sint ] t -> [< nonzero ] t -> [> sint_ovf ] t

  (** Modulo with mathematical (always-positive) semantics. *)
  val mod_ : [< sint ] t -> [< sint ] t -> [> sint_ovf ] t

  (** Two's-complement negation. *)
  val neg : [< sint ] t -> [> sint_ovf ] t

  (** {4 Overflow checks}

      Each [xxx_overflows] predicate is true iff the corresponding operation
      would overflow under the chosen signedness. *)

  val add_overflows : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val sub_overflows : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val mul_overflows : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val neg_overflows : [< sint ] t -> [> sbool ] t

  (** Unsafely asserts that an [sint_ovf] value did not overflow. Use only
      when the caller has independently checked the operation's preconditions.
  *)
  val no_ovf_unsafe : [< sint_ovf ] t -> [> sint ] t

  (** {4 Inequalities} *)

  val lt : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val leq : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val gt : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t
  val geq : signed:bool -> [< sint ] t -> [< sint ] t -> [> sbool ] t

  (** {4 Bit-vector manipulation} *)

  (** [concat hi lo] produces a wider bitvector with [hi] occupying the high
      bits and [lo] the low bits. *)
  val concat : [< sint ] t -> [< sint ] t -> [> sint ] t

  (** [extend ~signed n v] widens [v] to [n] bits, with sign- or zero-extension
      depending on [signed]. *)
  val extend : signed:bool -> int -> [< sint ] t -> [> sint ] t

  (** [extract hi lo v] selects the bits [lo..hi] (inclusive) of [v]. *)
  val extract : int -> int -> [< sint ] t -> [> sint ] t

  (** {4 Bitwise operations} *)

  val and_ : [< sint ] t -> [< sint ] t -> [> sint ] t
  val or_ : [< sint ] t -> [< sint ] t -> [> sint ] t
  val xor : [< sint ] t -> [< sint ] t -> [> sint ] t

  (** Logical left shift. *)
  val shl : [< sint ] t -> [< sint ] t -> [> sint ] t

  (** Logical right shift (zero-fill). *)
  val lshr : [< sint ] t -> [< sint ] t -> [> sint ] t

  (** Arithmetic right shift (sign-fill). *)
  val ashr : [< sint ] t -> [< sint ] t -> [> sint ] t

  (** Bitwise negation. *)
  val not : [< sint ] t -> [> sint ] t

  (** {4 Conversions} *)

  (** [of_bool n b] is the [n]-bit integer [1] when [b] holds, [0] otherwise. *)
  val of_bool : int -> [< sbool ] t -> [> sint ] t

  (** [to_bool v] is true iff [v] is non-zero. *)
  val to_bool : [< sint ] t -> [> sbool ] t

  (** Bitwise logical-not interpreted as a C-style boolean. *)
  val not_bool : [< sint ] t -> [> sint ] t

  (** Converts a float to an integer at the requested rounding mode and width.
  *)
  val of_float :
    rounding:Svalue.RoundingMode.t ->
    signed:bool ->
    size:int ->
    [< sfloat ] t ->
    [> sint ] t

  (** Converts an integer to a float, performing the rounding specified by
      [rounding] at precision [fp]. *)
  val to_float :
    rounding:Svalue.RoundingMode.t ->
    signed:bool ->
    fp:Svalue.FloatPrecision.t ->
    [< sint ] t ->
    [> sfloat ] t

  (** Reinterprets the bit pattern of an integer as a float of the same
      width, without any value conversion. *)
  val to_float_raw : [< sint ] t -> [> sfloat ] t
end

(** {3 Floating-point operations} *)

module Float : sig
  (** [mk fp s] is the float of precision [fp] obtained by parsing the
      decimal literal [s]. *)
  val mk : Svalue.FloatPrecision.t -> string -> [> sfloat ] t

  (** Constructs a binary16 float from a host [float]. *)
  val f16 : float -> [> sfloat ] t

  (** Constructs a binary32 float from a host [float]. *)
  val f32 : float -> [> sfloat ] t

  (** Constructs a binary64 float from a host [float]. *)
  val f64 : float -> [> sfloat ] t

  (** Constructs a binary128 float from a host [float]. *)
  val f128 : float -> [> sfloat ] t

  (** [like template v] constructs a float of the same precision as [template]
      from the host value [v]. *)
  val like : [< sfloat ] t -> float -> [> sfloat ] t

  (** Returns the precision of a symbolic float. *)
  val fp_of : [< sfloat ] t -> Svalue.FloatPrecision.t

  val eq : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val geq : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val gt : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val leq : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val lt : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val add : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val sub : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val mul : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val div : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val rem : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val abs : [< sfloat ] t -> [> sfloat ] t
  val neg : [< sfloat ] t -> [> sfloat ] t

  (** IEEE-754 classification predicates. *)

  val is_normal : [< sfloat ] t -> [> sbool ] t
  val is_subnormal : [< sfloat ] t -> [> sbool ] t
  val is_zero : [< sfloat ] t -> [> sbool ] t
  val is_infinite : [< sfloat ] t -> [> sbool ] t
  val is_nan : [< sfloat ] t -> [> sbool ] t

  (** Rounds a float to integral value at the requested rounding mode. *)
  val round : Svalue.RoundingMode.t -> [< sfloat ] t -> [> sfloat ] t
end

(** {3 Pointer operations}

    A pointer is the pair of a {!T.sloc} memory location and an {!T.sint}
    offset into that location. *)
module Ptr : sig
  (** [mk loc ofs] builds a pointer with location [loc] and offset [ofs]. *)
  val mk : [< sloc ] t -> [< sint ] t -> [> sptr ] t

  (** Returns the location component of a pointer. *)
  val loc : [< sptr ] t -> [> sloc ] t

  (** Returns the offset component of a pointer. *)
  val ofs : [< sptr ] t -> [> sint ] t

  (** Splits a pointer into its location and offset. *)
  val decompose : [< sptr ] t -> [> sloc ] t * [> sint ] t

  (** [add_ofs p k] is [p] shifted by [k] units. *)
  val add_ofs : [< sptr ] t -> [< sint ] t -> [> sptr ] t

  (** [loc_of_int width n] is the concrete location named by [n], represented
      on [width] bits. *)
  val loc_of_int : int -> int -> [> sloc ] t

  (** Variant of {!loc_of_int} taking a [Z.t] identifier. *)
  val loc_of_z : int -> Z.t -> [> sloc ] t

  (** [null width] is the null pointer at the given pointer width. *)
  val null : int -> [> sptr ] t

  (** [null_loc width] is the null location at the given pointer width. *)
  val null_loc : int -> [> sloc ] t

  (** True iff a location is the null location. *)
  val is_null_loc : [< sloc ] t -> [> sbool ] t

  (** True iff a pointer is exactly the null pointer (null location and zero
      offset). *)
  val is_null : [< sptr ] t -> [> sbool ] t

  (** True iff a pointer's location is the null location, regardless of its
      offset. *)
  val is_at_null_loc : [< sptr ] t -> [> sbool ] t
end

(** {3 Symbolic sequences} *)
module SSeq : sig
  (** [mk ~seq_ty elts] is the sequence containing [elts] at type [seq_ty]. *)
  val mk : seq_ty:'a sseq ty -> 'a t list -> [> 'a sseq ] t
end

(** {3 Infix operators}

    Curated overloaded operators for the most common arithmetic, comparison,
    and bitwise operations. Operators end with [@] to avoid clashing with
    the standard library. The [$] suffix means {e signed}; [!] disables
    overflow tracking; [?] returns the result paired with an overflow flag;
    [.] selects the floating-point variant. *)
module Infix : sig
  (** {4 Equality}

      [==@] requires its arguments to share a phantom kind, while [==?@]
      relaxes that constraint. *)
  val ( ==@ ) : [< any ] t -> [< any ] t -> [> sbool ] t

  val ( ==?@ ) : [< any ] t -> [< any ] t -> [> sbool ] t

  (** {4 Inequalities}

      [$] indicates signed comparison. *)

  val ( >@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >=$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <=$@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t

  (** {4 Booleans} *)

  val ( &&@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
  val ( ||@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t

  (** {4 Arithmetic with overflow tracking}

      Unsigned division and remainder cannot overflow and so return a plain
      {!T.sint}. Signed division and remainder can overflow on [MIN / -1] and
      so return {!T.sint_ovf}. *)

  val ( +@ ) : [< sint ] t -> [< sint ] t -> [> sint_ovf ] t
  val ( -@ ) : [< sint ] t -> [< sint ] t -> [> sint_ovf ] t
  val ( ~- ) : [< sint ] t -> [> sint_ovf ] t
  val ( *@ ) : [< sint ] t -> [< sint ] t -> [> sint_ovf ] t
  val ( /@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( /$@ ) : [< sint ] t -> [< nonzero ] t -> [> sint_ovf ] t
  val ( %@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( %$@ ) : [< sint ] t -> [< nonzero ] t -> [> sint_ovf ] t

  (** {4 Arithmetic ignoring overflow}

      These operators discard the overflow bit and return plain {!T.sint}. *)

  val ( +!@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( -!@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( *!@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ~-! ) : [< sint ] t -> [> sint ] t

  (** {4 Checked arithmetic with overflow ignored}

      Same as the [!@] family above but use the checked SMT operations
      internally, useful when downstream solvers benefit from explicit checks.
  *)

  val ( +!!@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( -!!@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( *!!@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t

  (** {4 Arithmetic returning the overflow flag}

      Each operator returns a [(result, overflowed?)] pair. *)

  val ( +?@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t * [> sbool ] t
  val ( +$?@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t * [> sbool ] t
  val ( -?@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t * [> sbool ] t
  val ( -$?@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t * [> sbool ] t
  val ( *?@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t * [> sbool ] t
  val ( *$?@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t * [> sbool ] t
  val ( ~-? ) : [< sint ] t -> [> sint ] t * [> sbool ] t

  (** {4 Bitwise operations} *)

  val ( <<@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( >>@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( >>>@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ^@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( &@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( |@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t

  (** {4 Floating-point operations} *)

  val ( ==.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( >.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( >=.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( <.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( <=.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sbool ] t
  val ( +.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val ( -.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val ( *.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
  val ( /.@ ) : [< sfloat ] t -> [< sfloat ] t -> [> sfloat ] t
end

(** Expression interface, providing a uniform way to build and inspect typed
    values; used to plug this representation into the {{!Soteria.Symex.Make}
    [Symex] functor}. *)
module Expr :
  Symex.Value.Expr
    with type 'a v := 'a t
     and type 'a ty := 'a ty
     and type t = Svalue.t
