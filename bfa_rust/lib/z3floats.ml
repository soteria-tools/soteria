open Simple_smt

(* Helpful: https://smt-lib.org/theories-FloatingPoint.shtml *)

let rm = atom "roundNearestTiesToAway"
let t_f64 = atom "Float64"

let f64_k f =
  let bin = Int64.bits_of_float f in
  (* a Float64 has 11 exponent bits, "53" mantissa bits, though the 53rd bit is implicitly 1 *)
  app_ "fp"
    [
      bv_nat_bin 1 (if Float.sign_bit f then Z.one else Z.zero);
      bv_nat_bin 11 (Z.of_int64 @@ Int64.shift_right_logical bin 52);
      bv_nat_bin 52 (Z.of_int64 @@ Int64.logand bin 0xfffffffffffffL);
    ]

let fp_abs f = app_ "fp.abs" [ f ]
let fp_leq f1 f2 = app_ "fp.leq" [ f1; f2 ]
let fp_lt f1 f2 = app_ "fp.lt" [ f1; f2 ]
let fp_add f1 f2 = app_ "fp.add" [ rm; f1; f2 ]
let fp_sub f1 f2 = app_ "fp.sub" [ rm; f1; f2 ]
let fp_mul f1 f2 = app_ "fp.mul" [ rm; f1; f2 ]
let fp_div f1 f2 = app_ "fp.div" [ rm; f1; f2 ]
let fp_rem f1 f2 = app_ "fp.rem" [ rm; f1; f2 ]
