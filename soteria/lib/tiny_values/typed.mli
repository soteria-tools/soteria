(** Typed wrapper around {!Svalue} for "tiny" symbolic values.

    This is the minimal-feature counterpart to {!Soteria.Bv_values}. It models
    only mathematical integers (unbounded, no overflow) and booleans, with no
    floats, pointers, or sequences. Use this when the language you are
    analysing maps cleanly onto unbounded integers and you want lighter SMT
    queries; reach for {!Soteria.Bv_values} when you need bit-precise
    semantics. *)

(** {2 Phantom types}

    The types in {!T} are used as phantom parameters of {!t}. They are not
    inhabited at runtime — they only constrain how values can be combined. *)

module T : sig
  (** A symbolic integer, statically tagged as known-zero, known-non-zero, or
      either. *)
  type sint = [ `NonZero | `Zero ]

  (** A symbolic integer known to be non-zero. *)
  type nonzero = [ `NonZero ]

  (** A symbolic boolean. *)
  type sbool = [ `Bool ]

  (** The union of all kinds. *)
  type any = [ `Bool | `NonZero | `Zero ]

  val pp_sint : Format.formatter -> sint -> unit
  val pp_nonzero : Format.formatter -> nonzero -> unit
  val pp_sbool : Format.formatter -> sbool -> unit
  val pp_any : Format.formatter -> any -> unit
end

open T

(** {2 Types} *)

(** Runtime representation of a symbolic type. *)
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

(** The (unbounded) integer type. *)
val t_int : [> sint ] ty

(** {2 Typed svalues}

    A value of type ['a t] is a symbolic value of phantom kind ['a]. *)

type +'a t
type sbool = T.sbool

(** Whether the supplied type is the boolean type. *)
val is_bool_ty : 'a ty -> bool

(** {3 Basic value operations} *)

(** Returns the underlying untyped representation of a value's type. *)
val get_ty : 'a t -> Svalue.ty

(** Inverse of [type_type] (when known): forgets the phantom parameter. *)
val untype_type : 'a ty -> Svalue.ty

(** Returns the kind tag of a value (whether it is a literal, a variable,
    etc.). *)
val kind : 'a t -> Svalue.t_kind

(** [mk_var v ty] constructs a symbolic variable named [v] at type [ty]. *)
val mk_var : Svalue.Var.t -> 'a ty -> 'a t

(** Iterates over the free variables of a value. *)
val iter_vars : 'a t -> (Svalue.Var.t * 'b ty -> unit) -> unit

(** Unsafely wraps an untyped value at phantom kind ['a]. *)
val type_ : Svalue.t -> 'a t

(** [type_checked v ty] is [Some v'] if [v] has type [ty]. *)
val type_checked : Svalue.t -> 'a ty -> 'a t option

(** Unsafe cast between phantom kinds. *)
val cast : 'a t -> 'b t

(** Checked variant of {!cast}, returning [None] on type mismatch. *)
val cast_checked : 'a t -> 'b ty -> 'b t option

(** [cast_checked2 a b] succeeds iff [a] and [b] have the same SMT type. *)
val cast_checked2 : 'a t -> 'b t -> ('c t * 'c t * 'c ty) option

(** Drops the phantom type, exposing the underlying untyped value. *)
val untyped : 'a t -> Svalue.t

(** Drops the phantom type on a whole list. *)
val untyped_list : 'a t list -> Svalue.t list

(** Pretty-prints a value using the supplied printer for any inner kind tag. *)
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** Same as {!pp}, but uses a default printer for the inner kind tag. *)
val ppa : Format.formatter -> 'a t -> unit

(** Syntactic equality (not semantic — use {!sem_eq}). *)
val equal : ([< any ] as 'a) t -> 'a t -> bool

(** Total order consistent with {!equal}. *)
val compare : ([< any ] as 'a) t -> 'a t -> int

(** Structural hash of a value. *)
val hash : [< any ] t -> int

(** {3 Typed constructors} *)

(** [sem_eq a b] is the symbolic boolean asserting that [a] and [b] are
    semantically equal. *)
val sem_eq : 'a t -> 'a t -> sbool t

(** Like {!sem_eq}, but operates on heterogeneously-typed inputs. *)
val sem_eq_untyped : 'a t -> 'b t -> [> sbool ] t

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

(** Conjunction of an arbitrary list of booleans. *)
val conj : [< sbool ] t list -> [> sbool ] t

(** Calls the supplied function on each top-level conjunct of the value, or on
    the value itself if it is not a conjunction. *)
val split_ands : [< sbool ] t -> ([> sbool ] t -> unit) -> unit

(** Symbolic disjunction. *)
val or_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t

(** Symbolic negation. *)
val not : sbool t -> sbool t

(** [distinct l] asserts that all values in [l] are pairwise distinct. *)
val distinct : 'a t list -> [> sbool ] t

(** [ite c a b] is the if-then-else: [a] when [c] holds, [b] otherwise. *)
val ite : [< sbool ] t -> 'a t -> 'a t -> 'a t

(** Lifts a [Z.t] integer to a symbolic value. *)
val int_z : Z.t -> [> sint ] t

(** Lifts a host integer to a symbolic value. *)
val int : int -> [> sint ] t

(** Like {!int_z}, but asserts statically that the value is non-zero. *)
val nonzero_z : Z.t -> [> nonzero ] t

(** Like {!int}, but asserts statically that the value is non-zero. *)
val nonzero : int -> [> nonzero ] t

(** The integer zero. *)
val zero : [> sint ] t

(** The integer one. *)
val one : [> nonzero ] t

(** {3 Integer operations}

    Tiny-values integers are mathematical (unbounded) integers, so none of
    these operations can overflow. *)

val geq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val gt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val leq : [< sint ] t -> [< sint ] t -> [> sbool ] t
val lt : [< sint ] t -> [< sint ] t -> [> sbool ] t
val add : [< sint ] t -> [< sint ] t -> [> sint ] t
val sub : [< sint ] t -> [< sint ] t -> [> sint ] t
val mul : [< sint ] t -> [< sint ] t -> [> sint ] t

(** [div a b] is integer division. The phantom kind of [b] requires it to be
    statically known non-zero. *)
val div : [< sint ] t -> [< nonzero ] t -> [> sint ] t

(** Remainder of integer division. *)
val rem : [< sint ] t -> [< nonzero ] t -> [> sint ] t

(** Mathematical modulo (always non-negative when the divisor is positive). *)
val mod_ : [< sint ] t -> nonzero t -> [> sint ] t

(** Integer negation. *)
val neg : [< sint ] t -> [> sint ] t

(** Infix operators for the most common operations. Operators end with [@] to
    avoid clashing with the standard library. *)
module Infix : sig
  val ( ==@ ) : ([< any ] as 'a) t -> 'a t -> [> sbool ] t
  val ( ==?@ ) : 'a t -> 'b t -> [> sbool ] t
  val ( >@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( >=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( <=@ ) : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val ( &&@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
  val ( ||@ ) : [< sbool ] t -> [< sbool ] t -> [> sbool ] t
  val ( +@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( -@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( ~- ) : [< sint ] t -> [> sint ] t
  val ( *@ ) : [< sint ] t -> [< sint ] t -> [> sint ] t
  val ( /@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
  val ( %@ ) : [< sint ] t -> [< nonzero ] t -> [> sint ] t
end

(** Auxiliary syntactic helpers exposed for PPX integration. *)
module Syntax : sig
  module Sym_int_syntax : sig
    val mk_nonzero : int -> [> nonzero ] t
    val zero : unit -> [> sint ] t
    val one : unit -> [> sint ] t
  end
end

(** Expression interface, providing a uniform way to build and inspect typed
    values; used to plug this representation into the {{!Soteria.Symex.Make}
    [Symex] functor}. *)
module Expr :
  Symex.Value.Expr
    with type 'a v := 'a t
     and type 'a ty := 'a ty
     and type t = Svalue.t
