open Simple_smt

(* Helpful: https://smt-lib.org/theories-FloatingPoint.shtml *)

let rm = atom "RNA" (* equivalent to roundNearestTiesToAway *)
let t_f64 = atom "Float64"
let t_f32 = atom "Float32"

let f64_k f =
  let bin = Int64.bits_of_float f in
  (* a Float64 has 11 exponent bits, 52 mantissa bits, with a 53rd implicit 1 *)
  app_ "fp"
    [
      bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
      bv_nat_bin 11 (Z.of_int64 @@ Int64.shift_right_logical bin 52);
      bv_nat_bin 52 (Z.of_int64 @@ Int64.logand bin 0xfffffffffffffL);
    ]

let f32_k f =
  let bin = Int32.bits_of_float f in
  (* a Float64 has 8 exponent bits, 23 explicit mantissa bits *)
  app_ "fp"
    [
      bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
      bv_nat_bin 8 (Z.of_int32 @@ Int32.shift_right_logical bin 23);
      bv_nat_bin 23 (Z.of_int32 @@ Int32.logand bin 0x7fffffl);
    ]

let fp_abs f = app_ "fp.abs" [ f ]
let fp_leq f1 f2 = app_ "fp.leq" [ f1; f2 ]
let fp_lt f1 f2 = app_ "fp.lt" [ f1; f2 ]
let fp_add f1 f2 = app_ "fp.add" [ rm; f1; f2 ]
let fp_sub f1 f2 = app_ "fp.sub" [ rm; f1; f2 ]
let fp_mul f1 f2 = app_ "fp.mul" [ rm; f1; f2 ]
let fp_div f1 f2 = app_ "fp.div" [ rm; f1; f2 ]
let fp_rem f1 f2 = app_ "fp.rem" [ f1; f2 ]

let f64_of_int i =
  let bv_fam = ifam "int_to_bv" [ 64 ] in
  let bv = app bv_fam [ i ] in
  let fam = ifam "to_fp" [ 11; 53 ] in
  app fam [ rm; bv ]

let f32_of_int i =
  (* there is no method to go from Int to FloatN, we must go through
     either Real or BitVec. Doing Int->Real->FloatN is super slow, so we
     need to go through BitVec instead.  *)
  let bv_fam = ifam "int_to_bv" [ 32 ] in
  let bv = app bv_fam [ i ] in
  let fam = ifam "to_fp" [ 8; 24 ] in
  app fam [ rm; bv ]

let int_of_f64 f =
  let fam = ifam "fp.to_sbv" [ 64 ] in
  let bv = app fam [ f ] in
  app_ "sbv_to_int" [ bv ]

let int_of_f32 f =
  let fam = ifam "fp.to_sbv" [ 32 ] in
  let bv = app fam [ f ] in
  app_ "sbv_to_int" [ bv ]
