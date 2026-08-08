//! Float operations Soteria evaluates exactly, at all four IEEE-754 precisions
#![feature(f16, f128, float_minimum_maximum)]

#[soteria::test]
fn arithmetic() {
    assert!(1.5f16 + 2.25 == 3.75);
    assert!(1.5f32 - 2.25 == -0.75);
    assert!(1.5f64 * 2.25 == 3.375);
    assert!(4.5f128 / 2.0 == 2.25);

    // negation, absolute value, fused multiply-add
    assert!(-(1.5f16) == -1.5);
    assert!((-1.5f32).abs() == 1.5);
    assert!((2.0f64).mul_add(3.0, 1.0) == 7.0);
    assert!(-(4.5f128) + 4.5 == 0.0);

    // `%` is fmod (sign of the dividend), not the IEEE remainder
    assert!(5.0f16 % 3.0 == 2.0);
    assert!(-5.0f32 % 3.0 == -2.0);
    assert!(5.0f64 % -3.0 == 2.0);
    assert!(1.5f128 % 1.0 == 0.5);
    assert!((5.0f32 % 0.0).is_nan() && (f32::INFINITY % 3.0).is_nan());
    assert!(5.0f32 % f32::INFINITY == 5.0);
    assert!((-0.0f64 % 1.0).is_sign_negative());

    // division by zero is infinite, not a trap
    assert!(1.0f16 / 0.0 == f16::INFINITY);
    assert!(-1.0f32 / 0.0 == f32::NEG_INFINITY);
    assert!((0.0f64 / 0.0).is_nan());
    assert!((f128::INFINITY - f128::INFINITY).is_nan());
}

/// Cases that distinguish the four precisions from one another
#[soteria::test]
fn precision() {
    // 0.1 + 0.2 == 0.3 holds in f32, but not in f64
    assert!(0.1f32 + 0.2 == 0.3);
    assert!(0.1f64 + 0.2 != 0.3);

    // rounds up at f32, but lands back on 1.0 if rounded via f64 first
    assert!(1.0000000596046448f32 != 1.0);

    // the first integer that is not representable, per format
    assert!(2048.0f16 + 1.0 == 2048.0); // 2^11
    assert!(16777216.0f32 + 1.0 == 16777216.0); // 2^24
    assert!(9007199254740992.0f64 + 1.0 == 9007199254740992.0); // 2^53

    // ties round to even, not away: each sum is exactly halfway to the next float
    assert!(1.0f32 + f32::from_bits(0x3380_0000) == 1.0); // + 2^-24
    assert!(1.0f64 + f64::from_bits(0x3CA0_0000_0000_0000) == 1.0); // + 2^-53

    // 1e-20 is below half an ulp of 1.0 in f64, but not in f128
    assert!(1.0f64 + 1.0e-20 == 1.0);
    assert!(1.0f128 + 1.0e-20 != 1.0);

    // a literal too large for the format is read as infinity
    assert!(1.0e40f16.is_infinite());
    assert!(1.0e40f32.is_infinite());
    assert!(1.0e40f64.is_finite());

    // ... and overflowing arithmetic saturates to infinity too
    assert!((f16::MAX * 2.0).is_infinite());
    assert!((f32::MAX * 2.0).is_infinite());
    assert!((f64::MAX * 2.0).is_infinite());
    assert!((f128::MAX * 2.0).is_infinite());
}

#[soteria::test]
fn classification() {
    assert!(f16::NAN.is_nan() && !f16::NAN.is_finite());
    assert!(f32::INFINITY.is_infinite() && !f32::INFINITY.is_normal());
    assert!(1.0f64.is_normal() && 1.0f64.is_finite());
    assert!(!0.0f128.is_normal() && 0.0f128.is_finite());

    // halving the smallest normal of a format lands in its subnormal range
    assert!((f16::MIN_POSITIVE / 2.0).is_subnormal());
    assert!((f32::MIN_POSITIVE / 2.0).is_subnormal());
    assert!((f64::MIN_POSITIVE / 2.0).is_subnormal());
    assert!((f128::MIN_POSITIVE / 2.0).is_subnormal());

    // a value subnormal in one format is perfectly normal in a wider one
    assert!(1.0e-6f16.is_subnormal());
    assert!(1.0e-6f32.is_normal());
    assert!(1.0e-40f32.is_subnormal());
    assert!(1.0e-40f64.is_normal());
    assert!(1.0e-320f64.is_subnormal());

    assert!(1.0f16.is_sign_positive() && (-1.0f16).is_sign_negative());
    assert!((3.0f32).copysign(-2.0) == -3.0);
    assert!((-3.0f64).copysign(2.0) == 3.0);
}

/// min/max ignore NaN; minimum/maximum propagate it and order the zeros
#[soteria::test]
fn min_max() {
    assert!((1.0f16).max(2.0) == 2.0 && (1.0f16).min(2.0) == 1.0);
    assert!((f32::NAN).max(2.0) == 2.0 && (2.0f32).max(f32::NAN) == 2.0);
    assert!((f64::NAN).min(2.0) == 2.0 && (2.0f64).min(f64::NAN) == 2.0);
    assert!((1.0f128).max(2.0) == 2.0 && (1.0f128).min(2.0) == 1.0);
    assert!((-1.0f32).max(f32::INFINITY) == f32::INFINITY);

    assert!((f16::NAN).maximum(2.0).is_nan());
    assert!((2.0f32).maximum(f32::NAN).is_nan());
    assert!((f64::NAN).minimum(2.0).is_nan());
    assert!((2.0f128).minimum(f128::NAN).is_nan());
    assert!((1.0f64).maximum(2.0) == 2.0 && (1.0f64).minimum(2.0) == 1.0);

    // the signed zeros are ordered by minimum/maximum, unlike by min/max
    assert!((0.0f32).maximum(-0.0).is_sign_positive());
    assert!((-0.0f32).maximum(0.0).is_sign_positive());
    assert!((0.0f64).minimum(-0.0).is_sign_negative());
    assert!((-0.0f64).minimum(0.0).is_sign_negative());
}

/// A zero's sign lives in its sign bit, not its value: `-0.0 == +0.0`
#[soteria::test]
fn signed_zeros() {
    assert!((-0.0f16).is_sign_negative() && !(-0.0f16).is_sign_positive());
    assert!((-0.0f32).is_sign_negative() && !(-0.0f32).is_sign_positive());
    assert!((-0.0f64).is_sign_negative() && !(-0.0f64).is_sign_positive());
    assert!((-0.0f128).is_sign_negative() && !(-0.0f128).is_sign_positive());

    assert!((0.0f16).is_sign_positive() && !(0.0f16).is_sign_negative());
    assert!((0.0f32).is_sign_positive() && !(0.0f32).is_sign_negative());
    assert!((0.0f64).is_sign_positive() && !(0.0f64).is_sign_negative());
    assert!((0.0f128).is_sign_positive() && !(0.0f128).is_sign_negative());

    // negation flips the sign bit, unlike `0.0 - x`
    assert!((-(0.0f16)).is_sign_negative());
    assert!((-(0.0f32)).is_sign_negative());
    assert!((-(0.0f64)).to_bits() == 0x8000_0000_0000_0000);
    assert!((-(-0.0f128)).is_sign_positive());
    assert!(0.0f32 - 0.0 == 0.0 && (0.0f32 - 0.0).is_sign_positive());

    // copysign takes the sign bit of its argument, zeros included
    assert!((1.0f32).copysign(-0.0) == -1.0);
    assert!((-1.0f64).copysign(0.0) == 1.0);
    assert!((0.0f16).copysign(-1.0).is_sign_negative());
    assert!((-0.0f128).copysign(1.0).is_sign_positive());
    assert!((-0.0f64).to_bits() == 0x8000_0000_0000_0000);
}

#[soteria::test]
fn comparisons() {
    // NaN is unordered: every comparison is false, including with itself
    let nan = f32::NAN;
    assert!(!(nan == nan) && nan != nan);
    assert!(!(nan < 0.0) && !(nan > 0.0) && !(nan <= 0.0) && !(nan >= 0.0));

    // the two zeros compare equal despite differing bit patterns
    assert!(0.0f16 == -0.0);
    assert!(0.0f32 == -0.0);
    assert!(0.0f64 == -0.0);
    assert!(0.0f128 == -0.0);
    assert!((0.0f64).to_bits() != (-0.0f64).to_bits());

    assert!(f16::NEG_INFINITY < f16::MIN);
    assert!(f32::MAX < f32::INFINITY);
    assert!(1.0f64 <= 1.0 && 1.0f64 >= 1.0);
    assert!(-1.5f128 < -1.25);
}

#[soteria::test]
fn rounding() {
    assert!((1.5f16).ceil() == 2.0 && (1.5f16).floor() == 1.0);
    assert!((-1.5f32).trunc() == -1.0 && (-1.5f32).ceil() == -1.0);
    assert!((2.5f64).round() == 3.0 && (-2.5f64).round() == -3.0);
    assert!((1.75f128).floor() == 1.0 && (1.75f128).round() == 2.0);

    // rounding preserves infinities and NaN
    assert!(f32::INFINITY.floor().is_infinite());
    assert!(f64::NAN.round().is_nan());
}

/// `sqrt` is exactly specified, so these pin the correctly-rounded result
#[soteria::test]
fn square_root() {
    assert!((4.0f16).sqrt() == 2.0);
    assert!((2.25f32).sqrt() == 1.5);
    assert!((9.0f64).sqrt() == 3.0);
    assert!((6.25f128).sqrt() == 2.5);

    // irrational roots are correctly rounded in their own format
    assert!((2.0f32).sqrt() == 1.4142135);
    assert!((2.0f64).sqrt() == 1.4142135623730951);

    // negatives give NaN; the zeros and infinities are fixed points
    assert!((-1.0f32).sqrt().is_nan() && f32::NAN.sqrt().is_nan());
    assert!(f64::INFINITY.sqrt() == f64::INFINITY);
    assert!((0.0f32).sqrt() == 0.0 && (-0.0f64).sqrt().is_sign_negative());
}

#[soteria::test]
fn bit_patterns() {
    // 1.5 is sign 0, biased exponent all-but-top-bit set, leading mantissa bit
    assert!((1.5f16).to_bits() == 0x3E00);
    assert!((1.5f32).to_bits() == 0x3FC0_0000);
    assert!((1.5f64).to_bits() == 0x3FF8_0000_0000_0000);
    assert!((1.5f128).to_bits() == 0x3FFF_8000_0000_0000_0000_0000_0000_0000);

    assert!(f16::from_bits(0x3E00) == 1.5);
    assert!(f32::from_bits(0x3FC0_0000) == 1.5);
    assert!(f64::from_bits(0x3FF8_0000_0000_0000) == 1.5);
    assert!(f128::from_bits(0x3FFF_8000_0000_0000_0000_0000_0000_0000) == 1.5);

    // an all-ones exponent with a non-zero mantissa is a NaN
    assert!(f32::from_bits(0x7FC0_0001).is_nan());
    assert!(f64::from_bits(0x7FF0_0000_0000_0000).is_infinite());
}

#[soteria::test]
fn int_conversions() {
    // int -> float, at both signednesses
    assert!(-1i8 as f16 == -1.0);
    assert!(300u32 as f32 == 300.0);
    assert!(i64::MIN as f64 == -9223372036854775808.0);
    assert!(u128::MAX as f128 > 0.0);

    // float -> int truncates toward zero
    assert!(3.99f16 as i8 == 3);
    assert!(-3.99f32 as i32 == -3);
    assert!(1e18f64 as i64 == 1000000000000000000);
    assert!(2.5f128 as u8 == 2);

    // a value not exactly representable is rounded on the way in
    assert!(16777217i32 as f32 == 16777216.0);
}

/// `float as int` saturates: NaN to zero, out of range to the nearest bound
#[soteria::test]
fn saturating_int_casts() {
    assert!(f16::NAN as u8 == 0);
    assert!(f32::NAN as i32 == 0);
    assert!(f64::NAN as u64 == 0);
    assert!(f128::NAN as i8 == 0);

    assert!(f32::INFINITY as i32 == i32::MAX);
    assert!(f32::NEG_INFINITY as i32 == i32::MIN);
    assert!(f64::INFINITY as u8 == 255);
    assert!(f64::NEG_INFINITY as u8 == 0);
    assert!(f16::MAX as i8 == 127);
    assert!(1.0e40f128 as u16 == 65535);

    // finite values just outside the range saturate as well
    assert!(256.0f32 as u8 == 255);
    assert!(-1.0f32 as u8 == 0);
    assert!(-1.0e30f64 as i16 == i16::MIN);

    // ... while those inside it still truncate toward zero
    assert!(255.9f32 as u8 == 255);
    assert!(-0.9f64 as i8 == 0);
    assert!(-2147483648.0f32 as i32 == i32::MIN);

    // i32::MAX is not an f32; this literal is really 2^31, so it saturates
    assert!(2147483647.0f32 as i32 == i32::MAX);
}

/// Widening is exact; narrowing rounds to nearest and overflows to infinity
#[soteria::test]
fn float_casts() {
    assert!(1.5f16 as f32 == 1.5 && 1.5f16 as f128 == 1.5);
    assert!(1.5f32 as f64 == 1.5);
    assert!(1.5f128 as f64 == 1.5 && 1.5f64 as f16 == 1.5);

    // widening keeps the value, not the wider format's reading of the decimal
    assert!(0.1f32 as f64 != 0.1f64);
    assert!(0.1f64 as f32 == 0.1f32);

    // narrowing rounds to nearest, ties to even
    assert!(16777217.0f64 as f32 == 16777216.0);
    assert!(2049.0f32 as f16 == 2048.0);

    // ... and saturates to infinity when the value does not fit
    assert!((1.0e300f64 as f32).is_infinite());
    assert!((f32::MAX as f16).is_infinite());
    assert!((f128::MAX as f64).is_infinite());
    assert!((-1.0e300f64 as f32) == f32::NEG_INFINITY);

    // infinities and NaN survive in both directions
    assert!((f32::INFINITY as f64).is_infinite());
    assert!((f64::NAN as f32).is_nan());
    assert!((f16::NAN as f128).is_nan());
}

/// The same operations, on unpinned values so they reach the solver
#[soteria::test]
fn symbolic() {
    let x: f32 = soteria::nondet_bytes();
    soteria::assume(x > 1.0 && x < 2.0);
    assert!(x + x > 2.0 && x + x < 4.0);

    let y: f32 = soteria::nondet_bytes();
    soteria::assume(y * y == 4.0 && y > 0.0);
    assert!(y == 2.0);

    let z: f64 = soteria::nondet_bytes();
    soteria::assume(z.is_nan());
    assert!(z != z && !(z < 0.0));

    let s: f32 = soteria::nondet_bytes();
    soteria::assume(s < 0.0);
    assert!(s.sqrt().is_nan());

    // only one symbolic `%`, at f16: fp.rem is the costliest operator to blast
    let m: f16 = soteria::nondet_bytes();
    soteria::assume(m > 0.0 && m < 3.0);
    assert!(m % 3.0 == m);

    // nothing above 1.0 is subnormal, whatever the format
    let w: f128 = soteria::nondet_bytes();
    soteria::assume(w > 1.0);
    assert!(!w.is_subnormal() && !w.is_nan());
}

/// Casts and sign tests on values rustc cannot constant-fold
#[soteria::test]
fn symbolic_casts() {
    let a: f32 = soteria::nondet_bytes();
    soteria::assume(a.is_nan());
    assert!(a as u8 == 0 && a as i64 == 0);

    let b: f32 = soteria::nondet_bytes();
    soteria::assume(b.is_infinite() && b > 0.0);
    assert!(b as i32 == i32::MAX && b as u8 == 255);

    let c: f64 = soteria::nondet_bytes();
    soteria::assume(c < -1.0e30);
    assert!(c as i16 == i16::MIN && c as u32 == 0);

    // widening to f64 and back is the identity on every finite f32
    let d: f32 = soteria::nondet_bytes();
    soteria::assume(d.is_finite());
    assert!((d as f64) as f32 == d);

    // a symbolic negative zero is still negative
    let e: f64 = soteria::nondet_bytes();
    soteria::assume(e.is_sign_negative() && e == 0.0);
    assert!(e.to_bits() == 0x8000_0000_0000_0000);
}
