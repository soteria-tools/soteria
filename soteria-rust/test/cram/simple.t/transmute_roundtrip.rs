use std::cmp::PartialEq;
macro_rules! test {
    ($from:ty, $to:ty) => {{
        let x1: $from = rusteria::nondet();
        let x2: $to = unsafe { core::mem::transmute(x1) };
        let x3: $from = unsafe { core::mem::transmute(x2) };
        assert_eq!(x1, x3);
    }};
}

macro_rules! test_two_way {
    ($from:ty, $to:ty) => {{
        test!($from, $to);
        test!($to, $from);
    }};
}

#[rusteria::test]
fn one_way_u32_f32() {
    // we custom write this, as this test does *not* work in the case of NaN values
    let x1: u32 = rusteria::nondet();
    let x2: f32 = unsafe { core::mem::transmute(x1) };
    let x3: u32 = unsafe { core::mem::transmute(x2) };
    rusteria::assume(!x2.is_nan());
    assert_eq!(x1, x3);
}

#[rusteria::test]
fn one_way_f32_u32() {
    // we custom write this, as this test does *not* work in the case of NaN values
    let x1: f32 = rusteria::nondet();
    let x2: u32 = unsafe { core::mem::transmute(x1) };
    let x3: f32 = unsafe { core::mem::transmute(x2) };
    if x1.is_nan() {
        assert!(x3.is_nan());
    } else {
        assert_eq!(x1, x3);
    }
}

#[rusteria::test]
fn two_way_u32_i32() {
    test_two_way!(u32, i32);
}

#[rusteria::test]
fn two_way_u8x4_u16x2() {
    test_two_way!([u8; 4], [u16; 2]);
}
