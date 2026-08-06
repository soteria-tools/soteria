//! Exercises the floating-point operations Soteria evaluates exactly, at all
//! four IEEE-754 precisions. Each assertion below must hold *in the precision
//! it is written at*, so evaluating any of them at a wider format (as Soteria
//! did when floats were kept as strings and reduced through `f64`) fails.
#![feature(f16, f128)]

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

    // division by zero is infinite, not a trap
    assert!(1.0f16 / 0.0 == f16::INFINITY);
    assert!(-1.0f32 / 0.0 == f32::NEG_INFINITY);
    assert!((0.0f64 / 0.0).is_nan());
    assert!((f128::INFINITY - f128::INFINITY).is_nan());
}

/// Each width rounds in its own format: these are the cases that distinguish
/// the four precisions from one another.
#[soteria::test]
fn precision() {
    // 0.1 + 0.2 == 0.3 holds in f32, but not in f64
    assert!(0.1f32 + 0.2 == 0.3);
    assert!(0.1f64 + 0.2 != 0.3);

    // this decimal rounds up to the f32 after 1.0, but rounding it to f64
    // first and only then to f32 lands back on 1.0
    assert!(1.0000000596046448f32 != 1.0);

    // the first integer that is not representable, per format
    assert!(2048.0f16 + 1.0 == 2048.0); // 2^11
    assert!(16777216.0f32 + 1.0 == 16777216.0); // 2^24
    assert!(9007199254740992.0f64 + 1.0 == 9007199254740992.0); // 2^53

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

/// The sign of a zero is carried by its sign bit, not by its value: `-0.0` and
/// `+0.0` compare equal, so anything deciding a sign by comparing against zero
/// gets every assertion here wrong.
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

    // negation flips the sign bit, so it does not fix a zero's sign the way
    // `0.0 - x` would
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
    // NaN is unordered: every comparison with it is false, and it is not
    // even equal to itself
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

/// `float as int` saturates in Rust rather than being undefined: NaN maps to
/// zero, and anything out of range to the nearest bound.
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

    // i32::MAX is not representable in f32: the literal below is really 2^31,
    // which is out of range and so saturates
    assert!(2147483647.0f32 as i32 == i32::MAX);
}

/// Casts between the float formats: widening is exact, narrowing rounds to
/// nearest and overflows to infinity.
#[soteria::test]
fn float_casts() {
    assert!(1.5f16 as f32 == 1.5 && 1.5f16 as f128 == 1.5);
    assert!(1.5f32 as f64 == 1.5);
    assert!(1.5f128 as f64 == 1.5 && 1.5f64 as f16 == 1.5);

    // widening keeps the value, which is *not* the same as the wider format's
    // rendering of the same decimal
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

/// The same operations, but on values the solver has to reason about rather
/// than ones that fold away.
#[soteria::test]
fn symbolic() {
    let x: f16 = soteria::nondet_bytes();
    soteria::assume(x == 2.5);
    assert!(x + x == 5.0);

    let y: f32 = soteria::nondet_bytes();
    soteria::assume(y * y == 4.0 && y > 0.0);
    assert!(y == 2.0);

    let z: f64 = soteria::nondet_bytes();
    soteria::assume(z.is_nan());
    assert!(z != z && !(z < 0.0));

    // nothing above 1.0 is subnormal, whatever the format
    let w: f128 = soteria::nondet_bytes();
    soteria::assume(w > 1.0);
    assert!(!w.is_subnormal() && !w.is_nan());
}

/// The casts and sign tests above, on values rustc cannot constant-fold, so
/// they are decided by Soteria and the solver rather than at compile time.
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
