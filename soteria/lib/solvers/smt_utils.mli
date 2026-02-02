(** SMT-LIB utilities for floating-point and bitvector operations.

    This module provides helper functions for constructing SMT-LIB
    S-expressions for:
    - IEEE 754 floating-point types and operations
    - Bitvector overflow detection
    - Conversions between floats, bitvectors, and integers

    These utilities are used by the solver backends to encode
    symbolic values into SMT-LIB format.

    {1 Floating-Point Support}

    The module supports IEEE 754 floating-point types:
    - Float16 (half precision): 5 exponent bits, 11 significand bits
    - Float32 (single precision): 8 exponent bits, 24 significand bits
    - Float64 (double precision): 11 exponent bits, 53 significand bits
    - Float128 (quad precision): 15 exponent bits, 113 significand bits

    See the {{:https://smt-lib.org/theories-FloatingPoint.shtml}SMT-LIB
    Floating-Point Theory} for the full specification.

    {1 Rounding Modes}

    Floating-point operations use rounding modes to handle precision loss.
    The default rounding mode is {!RoundingMode.NearestTiesToAway} (RNA). *)

open Simple_smt

(** {1 Rounding Modes} *)

(** IEEE 754 rounding modes for floating-point operations. *)
module RoundingMode : sig
  (** The type of rounding modes. *)
  type t =
    | NearestTiesToEven  (** Round to nearest, ties to even (RNE) *)
    | NearestTiesToAway  (** Round to nearest, ties away from zero (RNA) *)
    | Ceil               (** Round toward positive infinity (RTP) *)
    | Floor              (** Round toward negative infinity (RTN) *)
    | Truncate           (** Round toward zero (RTZ) *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val compare : t -> t -> int

  (** [to_sexp rm] converts a rounding mode to its SMT-LIB atom.

      {[
        to_sexp NearestTiesToEven  (* => (atom "RNE") *)
        to_sexp Ceil               (* => (atom "RTP") *)
      ]} *)
  val to_sexp : t -> sexp
end

(** {1 Float Type Shapes} *)

(** [float_shape n] returns the exponent and significand widths for
    an IEEE 754 float of [n] bits.

    Returns [[exp; sig]] where:
    - [exp] is the number of exponent bits
    - [sig] is the number of significand bits (including the hidden bit)

    Always holds that [n = exp + sig].

    @raise Failure if [n] is not one of 16, 32, 64, or 128

    {[
      float_shape 32   (* => [8; 24] *)
      float_shape 64   (* => [11; 53] *)
    ]} *)
val float_shape : int -> int list

(** {1 Default Rounding Mode} *)

(** Default rounding mode S-expression (RNA - round nearest ties away). *)
val rm : sexp

(** {1 Float Type S-expressions} *)

(** SMT-LIB Float16 type. *)
val t_f16 : sexp

(** SMT-LIB Float32 type. *)
val t_f32 : sexp

(** SMT-LIB Float64 type. *)
val t_f64 : sexp

(** SMT-LIB Float128 type. *)
val t_f128 : sexp

(** {1 Float Constants} *)

(** [f16_k f] creates a Float16 constant from OCaml float [f].

    The value is converted via Float32 using Z3's [to_fp] function. *)
val f16_k : float -> sexp

(** [f32_k f] creates a Float32 constant from OCaml float [f].

    Directly encodes the IEEE 754 binary32 representation. *)
val f32_k : float -> sexp

(** [f64_k f] creates a Float64 constant from OCaml float [f].

    Directly encodes the IEEE 754 binary64 representation. *)
val f64_k : float -> sexp

(** [f128_k f] creates a Float128 constant from OCaml float [f].

    The value is converted via Float64 using Z3's [to_fp] function. *)
val f128_k : float -> sexp

(** {1 Float Operations} *)

(** [fp_abs f] returns the absolute value of [f]. *)
val fp_abs : sexp -> sexp

(** [fp_eq f1 f2] returns true if [f1] equals [f2] (IEEE 754 equality). *)
val fp_eq : sexp -> sexp -> sexp

(** [fp_leq f1 f2] returns true if [f1 <= f2]. *)
val fp_leq : sexp -> sexp -> sexp

(** [fp_lt f1 f2] returns true if [f1 < f2]. *)
val fp_lt : sexp -> sexp -> sexp

(** [fp_add f1 f2] returns [f1 + f2] using the default rounding mode. *)
val fp_add : sexp -> sexp -> sexp

(** [fp_sub f1 f2] returns [f1 - f2] using the default rounding mode. *)
val fp_sub : sexp -> sexp -> sexp

(** [fp_mul f1 f2] returns [f1 * f2] using the default rounding mode. *)
val fp_mul : sexp -> sexp -> sexp

(** [fp_div f1 f2] returns [f1 / f2] using the default rounding mode. *)
val fp_div : sexp -> sexp -> sexp

(** [fp_rem f1 f2] returns the IEEE 754 remainder of [f1 / f2]. *)
val fp_rem : sexp -> sexp -> sexp

(** [fp_is fc f] tests if [f] belongs to floating-point class [fc].

    @param fc One of [FP_normal], [FP_subnormal], [FP_zero], [FP_infinite], [FP_nan] *)
val fp_is : fpclass -> sexp -> sexp

(** [fp_round rm f] rounds [f] to an integer using rounding mode [rm]. *)
val fp_round : RoundingMode.t -> sexp -> sexp

(** {1 Float/Bitvector Conversions} *)

(** [float_of_bv size bv] interprets bitvector [bv] as a float of [size] bits.

    This is a bitwise reinterpretation, not a numeric conversion. *)
val float_of_bv : int -> sexp -> sexp

(** [float_of_ubv rm size bv] converts unsigned bitvector [bv] to a float.

    @param rm Rounding mode for the conversion
    @param size Target float size in bits *)
val float_of_ubv : RoundingMode.t -> int -> sexp -> sexp

(** [float_of_sbv rm size bv] converts signed bitvector [bv] to a float.

    @param rm Rounding mode for the conversion
    @param size Target float size in bits *)
val float_of_sbv : RoundingMode.t -> int -> sexp -> sexp

(** [ubv_of_float rm n f] converts float [f] to an unsigned bitvector of [n] bits.

    @param rm Rounding mode for the conversion
    @param n Target bitvector width *)
val ubv_of_float : RoundingMode.t -> int -> sexp -> sexp

(** [sbv_of_float rm n f] converts float [f] to a signed bitvector of [n] bits.

    @param rm Rounding mode for the conversion
    @param n Target bitvector width *)
val sbv_of_float : RoundingMode.t -> int -> sexp -> sexp

(** {1 Integer/Bitvector Conversions} *)

(** [int_of_bv signed bv] converts bitvector [bv] to an integer.

    @param signed If true, interprets [bv] as signed (two's complement) *)
val int_of_bv : bool -> sexp -> sexp

(** [bv_of_int size n] converts integer [n] to a bitvector of [size] bits. *)
val bv_of_int : int -> sexp -> sexp

(** {1 Bitvector Overflow Detection} *)

(** [bv_nego x] returns true if negating [x] would overflow (signed). *)
val bv_nego : sexp -> sexp

(** [bv_uaddo l r] returns true if [l + r] would overflow (unsigned). *)
val bv_uaddo : sexp -> sexp -> sexp

(** [bv_saddo l r] returns true if [l + r] would overflow (signed). *)
val bv_saddo : sexp -> sexp -> sexp

(** [bv_umulo l r] returns true if [l * r] would overflow (unsigned). *)
val bv_umulo : sexp -> sexp -> sexp

(** [bv_smulo l r] returns true if [l * r] would overflow (signed). *)
val bv_smulo : sexp -> sexp -> sexp

(** {1 Solver Commands} *)

(** SMT-LIB [(reset)] command to reset the solver state. *)
val reset : command
