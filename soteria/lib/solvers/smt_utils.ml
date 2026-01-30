open Simple_smt
open Floatml

(* Float types and constants *)
(* Helpful: https://smt-lib.org/theories-FloatingPoint.shtml *)

module RoundingMode = struct
  type t = NearestTiesToEven | NearestTiesToAway | Ceil | Floor | Truncate
  [@@deriving eq, show { with_path = false }, ord]

  let to_sexp = function
    | NearestTiesToEven -> atom "RNE"
    | NearestTiesToAway -> atom "RNA"
    | Ceil -> atom "RTP"
    | Floor -> atom "RTN"
    | Truncate -> atom "RTZ"
end

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

let rm = atom "RNA" (* equivalent to roundNearestTiesToAway; default mode *)
let t_f16 = atom "Float16"
let t_f32 = atom "Float32"
let t_f64 = atom "Float64"
let t_f128 = atom "Float128"
let z_of_bool b = if b then Z.one else Z.zero
let z_u32 = Z.of_int32_unsigned
let z_u64 = Z.of_int64_unsigned

let f32_k f =
  let bin = F32.to_bits f in
  (* a Float32 has 8 exponent bits, 23 explicit mantissa bits *)
  app_ "fp"
    [
      bv_nat_bin 1 (z_of_bool (Int32.logand bin 0x80000000l <> 0l));
      bv_nat_bin 8
        (z_u32 @@ Int32.logand 0xffl @@ Int32.shift_right_logical bin 23);
      bv_nat_bin 23 (z_u32 @@ Int32.logand bin 0x7fffffl);
    ]

let f64_k f =
  let bin = F64.to_bits f in
  (* a Float64 has 11 exponent bits, 52 mantissa bits, with a 53rd implicit 1 *)
  app_ "fp"
    [
      bv_nat_bin 1 (z_of_bool (Int64.logand bin 0x8000000000000000L <> 0L));
      bv_nat_bin 11
        (z_u64 @@ Int64.logand 0x7ffL @@ Int64.shift_right_logical bin 52);
      bv_nat_bin 52 (z_u64 @@ Int64.logand bin 0xfffffffffffffL);
    ]

let f128_k f =
  let lo, hi = F128.to_bits f in
  (* a Float128 has 15 exponent bits, 112 mantissa bits, with a 113th implicit 1 *)
  app_ "fp"
    [
      bv_nat_bin 1 (z_of_bool (Int64.logand hi 0x8000000000000000L <> 0L));
      bv_nat_bin 15
        (z_u64 @@ Int64.logand 0x7fffL @@ Int64.shift_right_logical hi 48);
      bv_nat_bin 112
        (Z.logor
           (Z.shift_left (z_u64 @@ Int64.logand hi 0xffffffffffffL) 64)
           (z_u64 lo));
    ]

let f16_k f =
  let bin : int = F16.to_bits f in
  (* a Float16 has 5 exponent bits, 10 mantissa bits, with an 11th implicit 1 *)
  app_ "fp"
    [
      bv_nat_bin 1 (z_of_bool (bin land 0x8000 <> 0));
      bv_nat_bin 5 (Z.of_int ((bin land 0x7c00) lsr 10));
      bv_nat_bin 10 (Z.of_int (bin land 0x03ff));
    ]

(* Float ops *)

let fp_abs f = app_ "fp.abs" [ f ]
let fp_eq f1 f2 = app_ "fp.eq" [ f1; f2 ]
let fp_leq f1 f2 = app_ "fp.leq" [ f1; f2 ]
let fp_lt f1 f2 = app_ "fp.lt" [ f1; f2 ]
let fp_add f1 f2 = app_ "fp.add" [ rm; f1; f2 ]
let fp_sub f1 f2 = app_ "fp.sub" [ rm; f1; f2 ]
let fp_mul f1 f2 = app_ "fp.mul" [ rm; f1; f2 ]
let fp_div f1 f2 = app_ "fp.div" [ rm; f1; f2 ]
let fp_rem f1 f2 = app_ "fp.rem" [ f1; f2 ]

let fp_is (fc : fpclass) f =
  match fc with
  | FP_normal -> app_ "fp.isNormal" [ f ]
  | FP_subnormal -> app_ "fp.isSubnormal" [ f ]
  | FP_zero -> app_ "fp.isZero" [ f ]
  | FP_infinite -> app_ "fp.isInfinite" [ f ]
  | FP_nan -> app_ "fp.isNaN" [ f ]

let fp_round (rm : RoundingMode.t) f =
  app_ "fp.roundToIntegral" [ RoundingMode.to_sexp rm; f ]

(* Float{Of,To}Bv *)

let float_of_bv size bv = app (ifam "to_fp" (float_shape size)) [ bv ]

let float_of_ubv rm size bv =
  app (ifam "to_fp_unsigned" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

let float_of_sbv rm size bv =
  app (ifam "to_fp" (float_shape size)) [ RoundingMode.to_sexp rm; bv ]

let ubv_of_float rm n f =
  app (ifam "fp.to_ubv" [ n ]) [ RoundingMode.to_sexp rm; f ]

let sbv_of_float rm n f =
  app (ifam "fp.to_sbv" [ n ]) [ RoundingMode.to_sexp rm; f ]

(* Int{Of,To}Bv *)

let int_of_bv signed bv =
  if signed then app_ "sbv_to_int" [ bv ] else app_ "ubv_to_int" [ bv ]

let bv_of_int size n = app (ifam "int_to_bv" [ size ]) [ n ]

(* BitVector overflow operators *)

let bv_nego x = app_ "bvnego" [ x ]
let bv_uaddo l r = app_ "bvuaddo" [ l; r ]
let bv_saddo l r = app_ "bvsaddo" [ l; r ]
let bv_umulo l r = app_ "bvumulo" [ l; r ]
let bv_smulo l r = app_ "bvsmulo" [ l; r ]

(* Solver commands *)

let reset = simple_command [ "reset" ]
