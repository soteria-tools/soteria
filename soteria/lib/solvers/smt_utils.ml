(** SMT-LIB utilities for operations missing from [Simple_smt].

    Contains utilities for SMT-LIB's
    {{:https://smt-lib.org/theories-FloatingPoint.shtml} FloatingPoint theory}
    and
    {{:https://smt-lib.org/theories-FixedSizeBitVecs.shtml} FixedSizeBitVecs
     theory}, as well as some solver commands. *)

open Simple_smt

(** {2 Rounding Modes} *)

module RoundingMode = struct
  type t =
    | NearestTiesToEven  (** Round to nearest, ties to even (RNE) *)
    | NearestTiesToAway  (** Round to nearest, ties away from zero (RNA) *)
    | Ceil  (** Round toward positive infinity (RTP) *)
    | Floor  (** Round toward negative infinity (RTN) *)
    | Truncate  (** Round toward zero (RTZ) *)
  [@@deriving eq, show { with_path = false }, ord]

  let to_sexp = function
    | NearestTiesToEven -> atom "RNE"
    | NearestTiesToAway -> atom "RNA"
    | Ceil -> atom "RTP"
    | Floor -> atom "RTN"
    | Truncate -> atom "RTZ"

  (** Equal toquivalent to NearestTiesToAway; default mode for FloatingPointe
      operations. *)
  let default = to_sexp NearestTiesToAway
end

(** {1 FloatingPoint} *)

(** [float_shape n] is the shape of a IEEE float of a given size in bits.
    Returns a two element list [[exp, mant]], where [exp] is the number of
    exponent bits, and [mant] is the number of mantissa/significand bits. [mant]
    {b includes the hidden bit} which is always [1], as per SMT-lib's
    expectations. Always holds that [n = mant + exp]. Only implemented for
    [n = 16, 32, 64, 128]. *)
let float_shape = function
  | 16 -> [ 5; 11 ]
  | 32 -> [ 8; 24 ]
  | 64 -> [ 11; 53 ]
  | 128 -> [ 15; 113 ]
  | n -> Fmt.failwith "Unsupported float size: %d" n

(** SMT-LIB Float16 type. *)
let t_f16 = atom "Float16"

(** SMT-LIB Float32 type. *)
let t_f32 = atom "Float32"

(** SMT-LIB Float64 type. *)
let t_f64 = atom "Float64"

(** SMT-LIB Float128 type. *)
let t_f128 = atom "Float128"

(** [f32_k f] creates a Float32 constant from an OCaml float [f].

    Directly encodes the IEEE 754 binary32 representation. This may incur some
    loss of precision. *)
let f32_k f =
  let bin = Int32.bits_of_float f in
  (* a Float32 has 8 exponent bits, 23 explicit mantissa bits *)
  app_ "fp"
    [
      bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
      bv_nat_bin 8
        (Z.of_int32 @@ Int32.logand 0xffl @@ Int32.shift_right_logical bin 23);
      bv_nat_bin 23 (Z.of_int32 @@ Int32.logand bin 0x7fffffl);
    ]

(** [f64_k f] creates a Float64 constant from an OCaml float [f].

    Directly encodes the IEEE 754 binary64 representation, so no precision is
    lost. *)
let f64_k f =
  let bin = Int64.bits_of_float f in
  (* a Float64 has 11 exponent bits, 52 mantissa bits, with a 53rd implicit 1 *)
  app_ "fp"
    [
      bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
      bv_nat_bin 11
        (Z.of_int64 @@ Int64.logand 0x7ffL @@ Int64.shift_right_logical bin 52);
      bv_nat_bin 52 (Z.of_int64 @@ Int64.logand bin 0xfffffffffffffL);
    ]

(** [f128_k f] creates a Float128 constant from OCaml float [f].

    The value is first converted into a Float64, and we then use the [to_fp]
    SMT-LIB function to convert it to a Float128, using [RoundingMode.default].
    Necessarily implies a loss of precision. *)
let f128_k f =
  let f64 = f64_k f in
  let fam = ifam "to_fp" (float_shape 128) in
  app fam [ RoundingMode.default; f64 ]

(** [f16_k f] creates a Float16 constant from OCaml float [f].

    The value is first converted into a Float32, and we then use the [to_fp]
    SMT-LIB function to convert it to a Float16, using [RoundingMode.default].
    Necessarily implies a loss of precision. *)
let f16_k f =
  let f32 = f32_k f in
  let fam = ifam "to_fp" (float_shape 16) in
  app fam [ RoundingMode.default; f32 ]

(** [fp_abs f] returns the absolute value of [f]. *)
let fp_abs f = app_ "fp.abs" [ f ]

(** [fp_eq f1 f2] returns true if [f1] equals [f2] (IEEE 754 equality). *)
let fp_eq f1 f2 = app_ "fp.eq" [ f1; f2 ]

(** [fp_leq f1 f2] returns true if [f1 <= f2]. *)
let fp_leq f1 f2 = app_ "fp.leq" [ f1; f2 ]

(** [fp_lt f1 f2] returns true if [f1 < f2]. *)
let fp_lt f1 f2 = app_ "fp.lt" [ f1; f2 ]

(** [fp_add f1 f2] returns [f1 + f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_add f1 f2 = app_ "fp.add" [ RoundingMode.default; f1; f2 ]

(** [fp_sub f1 f2] returns [f1 - f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_sub f1 f2 = app_ "fp.sub" [ RoundingMode.default; f1; f2 ]

(** [fp_mul f1 f2] returns [f1 * f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_mul f1 f2 = app_ "fp.mul" [ RoundingMode.default; f1; f2 ]

(** [fp_div f1 f2] returns [f1 / f2] using the default rounding mode (see
    [RoundingMode.default]). *)
let fp_div f1 f2 = app_ "fp.div" [ RoundingMode.default; f1; f2 ]

(** [fp_rem f1 f2] returns [f1 % f2] (no rounding mode is involved here). *)
let fp_rem f1 f2 = app_ "fp.rem" [ f1; f2 ]

(** [fp_is fc f] tests if [f] belongs to floating-point class [fc] (which is of
    OCaml's builtin [fpclass] type).*)
let fp_is (fc : fpclass) f =
  match fc with
  | FP_normal -> app_ "fp.isNormal" [ f ]
  | FP_subnormal -> app_ "fp.isSubnormal" [ f ]
  | FP_zero -> app_ "fp.isZero" [ f ]
  | FP_infinite -> app_ "fp.isInfinite" [ f ]
  | FP_nan -> app_ "fp.isNaN" [ f ]

(** [fp_round rm f] rounds [f] to an integer using rounding mode [rm]. *)
let fp_round (rm : RoundingMode.t) f =
  app_ "fp.roundToIntegral" [ RoundingMode.to_sexp rm; f ]

(** {1 FloatingPoint - BitVec conversions} *)

(** [float_of_bv size bv] interprets bitvector [bv] as a float of [size] bits.
    [size] must be one of 16, 32, 64 or 128.

    This is a bitwise reinterpretation, not a numeric conversion. *)
let float_of_bv size bv = app (ifam "to_fp" (float_shape size)) [ bv ]

(** [float_of_ubv rm size bv] converts unsigned bitvector [bv] to a float, with
    rounding mode [rm]. [size] must be one of 16, 32, 64 or 128. *)
let float_of_ubv rm size bv =
  app (ifam "to_fp_unsigned" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

(** [float_of_ubv rm size bv] converts signed bitvector [bv] to a float, with
    rounding mode [rm]. [size] must be one of 16, 32, 64 or 128. *)
let float_of_sbv rm size bv =
  app (ifam "to_fp" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

(** [ubv_of_float rm size f] converts float [f] to an unsigned bitvector of
    [size] bits, with rounding mode [rm]. This is a numeric conversion, not a
    bitwise reinterpretation. If [f] is out of range for the target bitvector,
    or NaN or inf, the result is undefined. *)
let ubv_of_float rm n f =
  app (ifam "fp.to_ubv" [ n ]) [ RoundingMode.to_sexp rm; f ]

(** [ubv_of_float rm size f] converts float [f] to a signed bitvector of [size]
    bits, with rounding mode [rm]. This is a numeric conversion, not a bitwise
    reinterpretation. If [f] is out of range for the target bitvector, or NaN or
    inf, the result is undefined. *)
let sbv_of_float rm n f =
  app (ifam "fp.to_sbv" [ n ]) [ RoundingMode.to_sexp rm; f ]

(** {1 Int - BitVec conversions} *)

(** [int_of_bv signed bv] converts bitvector [bv] to an integer. If [signed] is
    true, [bv] is interpreted as a signed bitvector; otherwise, it is
    interpreted as unsigned. *)
let int_of_bv signed bv =
  if signed then app_ "sbv_to_int" [ bv ] else app_ "ubv_to_int" [ bv ]

(** [bv_of_int size n] converts integer [n] to a bitvector of [size] bits. *)
let bv_of_int size n = app (ifam "int_to_bv" [ size ]) [ n ]

(** {1 BitVec} *)

(** [bv_nego x] is true if negating [x] would overflow (signed). *)
let bv_nego x = app_ "bvnego" [ x ]

(** [bv_uaddo l r] returns true if [l + r] would overflow (unsigned). *)
let bv_uaddo l r = app_ "bvuaddo" [ l; r ]

(** [bv_saddo l r] returns true if [l + r] would overflow (signed). *)
let bv_saddo l r = app_ "bvsaddo" [ l; r ]

(** [bv_usubo l r] returns true if [l - r] would underflow (unsigned). *)
let bv_usubo l r = app_ "bvusubo" [ l; r ]

(** [bv_ssubo l r] returns true if [l - r] would underflow or overflow (signed).
*)
let bv_ssubo l r = app_ "bvssubo" [ l; r ]

(** [bv_umulo l r] returns true if [l * r] would overflow (unsigned). *)
let bv_umulo l r = app_ "bvumulo" [ l; r ]

(** [bv_umulo l r] returns true if [l * r] would overflow (signed). *)
let bv_smulo l r = app_ "bvsmulo" [ l; r ]

(** {1 Commands} *)

(** SMT-LIB [(reset)] command to reset the solver state. *)
let reset = simple_command [ "reset" ]
