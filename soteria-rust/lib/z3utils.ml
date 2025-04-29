open Simple_smt

(* Float types and constants *)
(* Helpful: https://smt-lib.org/theories-FloatingPoint.shtml *)

let rm = atom "RNA" (* equivalent to roundNearestTiesToAway *)
let t_f16 = atom "Float16"
let t_f32 = atom "Float32"
let t_f64 = atom "Float64"
let t_f128 = atom "Float128"

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

let f128_k f =
  (* a Float128 has 15 exponent bits, 112 explicit mantissa bits *)
  (* we let Z3 handle the conversion *)
  let f64 = f64_k f in
  let fam = ifam "to_fp" [ 15; 113 ] in
  app fam [ rm; f64 ]

let f16_k f =
  (* a Float16 has 5 exponent bits, 10 explicit mantissa bits *)
  (* we let Z3 handle the conversion *)
  let f32 = f32_k f in
  let fam = ifam "to_fp" [ 5; 11 ] in
  app fam [ rm; f32 ]

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

(* Float{Of,To}Bv *)

let f16_of_bv bv = app (ifam "to_fp" [ 5; 11 ]) [ bv ]
let f32_of_bv bv = app (ifam "to_fp" [ 8; 24 ]) [ bv ]
let f64_of_bv bv = app (ifam "to_fp" [ 11; 53 ]) [ bv ]
let f128_of_bv bv = app (ifam "to_fp" [ 15; 113 ]) [ bv ]
let bv_of_f16 f = app (ifam "fp.to_sbv" [ 16 ]) [ rm; f ]
let bv_of_f32 f = app (ifam "fp.to_sbv" [ 32 ]) [ rm; f ]
let bv_of_f64 f = app (ifam "fp.to_sbv" [ 64 ]) [ rm; f ]
let bv_of_f128 f = app (ifam "fp.to_sbv" [ 128 ]) [ rm; f ]

(* Int{Of,To}Bv *)

let int_of_bv signed bv =
  if signed then app_ "sbv_to_int" [ bv ] else app_ "ubv_to_int" [ bv ]

let bv_of_int size n = app (ifam "int_to_bv" [ size ]) [ n ]
