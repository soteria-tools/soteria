(** {1 Svalue - Core Symbolic Value Representation}

    This module provides the core representation for symbolic values used in
    symbolic execution. Values are hash-consed for efficient comparison and
    memory usage, with extensive simplification rules applied during construction.

    {2 Key Concepts}

    {b Hash-Consing}: All symbolic values are uniquely represented in memory.
    Two structurally equal values share the same memory location, enabling O(1)
    equality checks via physical equality of tags.

    {b Simplification}: Operations automatically simplify results when possible.
    For example, [x + 0 = x], [x && true = x], [extract(concat(a,b)) = a or b].
    This reduces the complexity of values sent to the SMT solver.

    {b Type Safety}: Values carry their type ([ty]) and operations enforce type
    compatibility at runtime.

    {2 Value Kinds}

    - {b Var}: Symbolic variable (unconstrained)
    - {b Bool}: Concrete boolean
    - {b Float}: Concrete floating-point (stored as string)
    - {b BitVec}: Concrete bitvector (arbitrary precision via Zarith)
    - {b Ptr}: Symbolic pointer (location + offset pair)
    - {b Seq}: Symbolic sequence/list
    - {b Unop}: Unary operation on a value
    - {b Binop}: Binary operation on two values
    - {b Nop}: N-ary operation on multiple values
    - {b Ite}: If-then-else conditional

    {2 Example}

    {[
      open Svalue
      open Infix

      (* Create a 32-bit symbolic variable *)
      let x = mk_var (Var.of_int 0) (t_bv 32)

      (* Arithmetic: (x + 5) * 2 *)
      let expr = (x +@ BitVec.mki 32 5) *@ BitVec.mki 32 2

      (* Comparison: x > 10 *)
      let cond = x >@ BitVec.mki 32 10

      (* Conditional: if x > 10 then x else 0 *)
      let result = Bool.ite cond x (BitVec.zero 32)
    ]}
*)

open Soteria_std
module Var = Symex.Var

(** {2 Float Precision} *)

module FloatPrecision : sig
  type t = F16 | F32 | F64 | F128

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit

  (** [size fp] returns the bit width of the float precision. *)
  val size : t -> int

  (** [of_size n] returns the precision for bit width [n].
      @raise Failure if [n] is not 16, 32, 64, or 128. *)
  val of_size : int -> t
end

(** {2 Float Classification} *)

module FloatClass : sig
  type t = Normal | Subnormal | Zero | Infinite | NaN

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit

  (** Converts to OCaml's [fpclass]. *)
  val as_fpclass : t -> fpclass
end

(** {2 Rounding Modes} *)

module RoundingMode : sig
  type t =
    | NearestTiesToEven  (** Round to nearest, ties to even (default) *)
    | NearestTiesToAway  (** Round to nearest, ties away from zero *)
    | Ceil               (** Round toward positive infinity *)
    | Floor              (** Round toward negative infinity *)
    | Truncate           (** Round toward zero *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

(** {2 Types} *)

(** Type of symbolic values. *)
type ty =
  | TBool
  | TFloat of FloatPrecision.t
  | TLoc of int          (** Location (abstract address), parameterized by bit width *)
  | TPointer of int      (** Pointer (location + offset), parameterized by bit width *)
  | TSeq of ty           (** Sequence of values *)
  | TBitVector of int    (** Bitvector with given bit width *)

val equal_ty : ty -> ty -> bool
val compare_ty : ty -> ty -> int
val pp_ty : Format.formatter -> ty -> unit

(** Type constructors *)
val t_bool : ty
val t_float : FloatPrecision.t -> ty
val t_f16 : ty
val t_f32 : ty
val t_f64 : ty
val t_f128 : ty
val t_loc : int -> ty
val t_ptr : int -> ty
val t_seq : ty -> ty
val t_bv : int -> ty

(** Type predicates *)
val is_float : ty -> bool
val is_bv : ty -> bool

(** [precision_of_f ty] returns the float precision.
    @raise Failure if [ty] is not a float type. *)
val precision_of_f : ty -> FloatPrecision.t

(** [size_of ty] returns the bit width.
    @raise Failure if [ty] is not a bit-valued type. *)
val size_of : ty -> int

(** {2 Operators} *)

(** N-ary operators (currently only [Distinct]). *)
module Nop : sig
  type t = Distinct

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

(** Unary operators. *)
module Unop : sig
  type t =
    | Not                           (** Boolean negation *)
    | GetPtrLoc                     (** Extract location from pointer *)
    | GetPtrOfs                     (** Extract offset from pointer *)
    | BvOfBool of int               (** Bool to bitvector (1 -> one, 0 -> zero) *)
    | BvOfFloat of RoundingMode.t * bool * int  (** Float to BV (rounding, signed, size) *)
    | FloatOfBv of RoundingMode.t * bool * FloatPrecision.t  (** BV to float (rounding, signed, prec) *)
    | FloatOfBvRaw of FloatPrecision.t  (** Raw bits to float (reinterpret cast) *)
    | BvExtract of int * int        (** Bit extraction [from, to] inclusive *)
    | BvExtend of bool * int        (** Extension (signed, by N bits) *)
    | BvNot                         (** Bitwise NOT *)
    | Neg                           (** Arithmetic negation *)
    | FAbs                          (** Float absolute value *)
    | FIs of FloatClass.t           (** Float classification test *)
    | FRound of RoundingMode.t      (** Float rounding *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

(** Binary operators. *)
module Binop : sig
  type t =
    (* Boolean *)
    | And
    | Or
    (* Comparison *)
    | Eq
    (* Float comparison *)
    | FEq | FLeq | FLt
    (* Float arithmetic *)
    | FAdd | FSub | FMul | FDiv | FRem
    (* Bitvector arithmetic *)
    | Add of { checked : bool }  (** Addition; checked means overflow was verified *)
    | Sub of { checked : bool }  (** Subtraction *)
    | Mul of { checked : bool }  (** Multiplication *)
    | Div of bool                (** Division (signed flag) *)
    | Rem of bool                (** Remainder (signed flag) *)
    | Mod                        (** Modulo (signed, result takes sign of divisor) *)
    | AddOvf of bool             (** Addition overflow check (signed flag) *)
    | MulOvf of bool             (** Multiplication overflow check *)
    | Lt of bool                 (** Less than (signed flag) *)
    | Leq of bool                (** Less than or equal (signed flag) *)
    (* Bitvector bitwise *)
    | BvConcat                   (** Concatenation *)
    | BitAnd | BitOr | BitXor
    | Shl | LShr | AShr          (** Shifts: logical left, logical right, arithmetic right *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

(** {2 Value Representation} *)

(** The kind of a symbolic value (the actual data). *)
type t_kind =
  | Var of Var.t
  | Bool of bool
  | Float of string              (** Float stored as string for precision *)
  | Ptr of t * t                 (** Pointer: (location, offset) *)
  | BitVec of Z.t                (** Bitvector with arbitrary precision *)
  | Seq of t list                (** Sequence of values *)
  | Unop of Unop.t * t
  | Binop of Binop.t * t * t
  | Nop of Nop.t * t list
  | Ite of t * t * t             (** If-then-else *)

(** Value node containing kind and type. *)
and t_node = { kind : t_kind; ty : ty }

(** Hash-consed symbolic value. Two values are equal iff they have the same tag. *)
and t

(** {2 Basic Operations} *)

val pp : Format.formatter -> t -> unit
val pp_full : Format.formatter -> t -> unit

(** Fast equality via tag comparison. O(1). *)
val equal : t -> t -> bool

(** Comparison via tag. *)
val compare : t -> t -> int

(** Hash of the value (its unique tag). *)
val hash : t -> int

(** The unique tag of this value. *)
val unique_tag : t -> int

(** The kind of this value. *)
val kind : t -> t_kind

(** [iter f v] calls [f] on [v] and all its subvalues. *)
val iter : t -> (t -> unit) -> unit

(** [iter_vars v f] calls [f (var, ty)] for each variable in [v]. *)
val iter_vars : t -> (Var.t * ty -> unit) -> unit

(** [subst f v] substitutes variables using [f]. *)
val subst : (Var.t -> Var.t) -> t -> t

(** {2 Constructors} *)

(** [mk_var var ty] creates a symbolic variable. *)
val mk_var : Var.t -> ty -> t

(** [sure_neq a b] returns [true] if [a] and [b] are definitely not equal
    (can be determined without solver). *)
val sure_neq : t -> t -> bool

(** {2 Boolean Operations}

    Module signature for boolean operations. *)
module type Bool = sig
  val v_true : t
  val v_false : t
  val as_bool : t -> bool option
  val bool : bool -> t
  val and_ : t -> t -> t
  val and_lazy : t -> (unit -> t) -> t
  val or_ : t -> t -> t
  val or_lazy : t -> (unit -> t) -> t
  val conj : t list -> t
  val not : t -> t
  val split_ands : t -> t Iter.t
  val distinct : t list -> t
  val ite : t -> t -> t -> t
  val sem_eq : t -> t -> t
  val sem_eq_untyped : t -> t -> t
end

(** {2 Bitvector Operations} *)
module type BitVec = sig
  val mk : int -> Z.t -> t
  val mk_masked : int -> Z.t -> t
  val mki : int -> int -> t
  val zero : int -> t
  val one : int -> t
  val bv_to_z : bool -> int -> Z.t -> Z.t
  val to_z : t -> Z.t option
  val msb_of : t -> int

  (* Arithmetic *)
  val add : ?checked:bool -> t -> t -> t
  val sub : ?checked:bool -> t -> t -> t
  val mul : ?checked:bool -> t -> t -> t
  val div : signed:bool -> t -> t -> t
  val rem : signed:bool -> t -> t -> t
  val mod_ : t -> t -> t
  val neg : t -> t

  (* Overflow checks *)
  val add_overflows : signed:bool -> t -> t -> t
  val sub_overflows : signed:bool -> t -> t -> t
  val mul_overflows : signed:bool -> t -> t -> t
  val neg_overflows : t -> t

  (* Comparisons *)
  val lt : signed:bool -> t -> t -> t
  val leq : signed:bool -> t -> t -> t
  val gt : signed:bool -> t -> t -> t
  val geq : signed:bool -> t -> t -> t

  (* Manipulation *)
  val concat : t -> t -> t
  val extend : signed:bool -> int -> t -> t
  val extract : int -> int -> t -> t

  (* Bitwise *)
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor : t -> t -> t
  val shl : t -> t -> t
  val lshr : t -> t -> t
  val ashr : t -> t -> t
  val not : t -> t

  (* Conversions *)
  val of_bool : int -> t -> t
  val to_bool : t -> t
  val not_bool : t -> t
  val of_float : rounding:RoundingMode.t -> signed:bool -> size:int -> t -> t
  val to_float : rounding:RoundingMode.t -> signed:bool -> fp:FloatPrecision.t -> t -> t
  val to_float_raw : t -> t
end

(** {2 Float Operations} *)
module type Float = sig
  val mk : FloatPrecision.t -> string -> t
  val f16 : float -> t
  val f32 : float -> t
  val f64 : float -> t
  val f128 : float -> t
  val like : t -> float -> t
  val fp_of : t -> FloatPrecision.t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val round : RoundingMode.t -> t -> t

  val eq : t -> t -> t
  val lt : t -> t -> t
  val leq : t -> t -> t
  val gt : t -> t -> t
  val geq : t -> t -> t

  val is_floatclass : FloatClass.t -> t -> t
  val is_normal : t -> t
  val is_subnormal : t -> t
  val is_zero : t -> t
  val is_infinite : t -> t
  val is_nan : t -> t
end

(** {2 Module Implementations} *)

module Bool : Bool
module BitVec : BitVec
module Float : Float

(** {2 Pointer Operations} *)

module Ptr : sig
  (** [mk loc ofs] creates a pointer from location and offset. *)
  val mk : t -> t -> t

  (** [loc ptr] extracts the location component. *)
  val loc : t -> t

  (** [ofs ptr] extracts the offset component. *)
  val ofs : t -> t

  (** [decompose ptr] returns [(loc, ofs)]. *)
  val decompose : t -> t * t

  (** [add_ofs ptr delta] adds [delta] to the pointer's offset. *)
  val add_ofs : t -> t -> t

  (** [null n] creates a null pointer with bit width [n]. *)
  val null : int -> t

  (** [null_loc n] creates a null location with bit width [n]. *)
  val null_loc : int -> t

  (** [loc_of_int n i] creates a location from integer [i]. *)
  val loc_of_int : int -> int -> t

  (** [loc_of_z n z] creates a location from arbitrary-precision integer [z]. *)
  val loc_of_z : int -> Z.t -> t

  (** [is_null ptr] checks if pointer is null (location and offset are zero). *)
  val is_null : t -> t

  (** [is_null_loc loc] checks if location is null. *)
  val is_null_loc : t -> t

  (** [is_at_null_loc ptr] checks if pointer's location is null. *)
  val is_at_null_loc : t -> t
end

(** {2 Sequence Operations} *)

module SSeq : sig
  (** [mk ~seq_ty elements] creates a sequence with the given type and elements. *)
  val mk : seq_ty:ty -> t list -> t

  (** [inner_ty ty] extracts the element type from a sequence type.
      @raise Failure if [ty] is not a sequence type. *)
  val inner_ty : ty -> ty
end

(** {2 Infix Operators}

    Convenient infix operators for building expressions. *)

module Infix : sig
  (** Semantic equality *)
  val ( ==@ ) : t -> t -> t

  (** Untyped semantic equality (returns false for type mismatch) *)
  val ( ==?@ ) : t -> t -> t

  (** Boolean and *)
  val ( &&@ ) : t -> t -> t

  (** Boolean or *)
  val ( ||@ ) : t -> t -> t

  (** Unsigned comparisons *)
  val ( >@ ) : t -> t -> t
  val ( >=@ ) : t -> t -> t
  val ( <@ ) : t -> t -> t
  val ( <=@ ) : t -> t -> t

  (** Signed comparisons ($ suffix) *)
  val ( >$@ ) : t -> t -> t
  val ( >=$@ ) : t -> t -> t
  val ( <$@ ) : t -> t -> t
  val ( <=$@ ) : t -> t -> t

  (** Arithmetic (unchecked) *)
  val ( +@ ) : t -> t -> t
  val ( -@ ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( *@ ) : t -> t -> t

  (** Division and remainder (unsigned) *)
  val ( /@ ) : t -> t -> t
  val ( %@ ) : t -> t -> t

  (** Division and remainder (signed, $ suffix) *)
  val ( /$@ ) : t -> t -> t
  val ( %$@ ) : t -> t -> t

  (** Bitwise operations *)
  val ( <<@ ) : t -> t -> t   (** Left shift *)
  val ( >>@ ) : t -> t -> t   (** Logical right shift *)
  val ( >>>@ ) : t -> t -> t  (** Arithmetic right shift *)
  val ( ^@ ) : t -> t -> t    (** XOR *)
  val ( &@ ) : t -> t -> t    (** AND *)
  val ( |@ ) : t -> t -> t    (** OR *)

  (** Float comparisons (. suffix) *)
  val ( ==.@ ) : t -> t -> t
  val ( >.@ ) : t -> t -> t
  val ( >=.@ ) : t -> t -> t
  val ( <.@ ) : t -> t -> t
  val ( <=.@ ) : t -> t -> t

  (** Float arithmetic *)
  val ( +.@ ) : t -> t -> t
  val ( -.@ ) : t -> t -> t
  val ( *.@ ) : t -> t -> t
  val ( /.@ ) : t -> t -> t
end
