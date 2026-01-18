fn check_if_zero<const C: usize>() {
    rusteria::assume(C == 0);
}

#[rusteria::test]
fn test_concrete_const_generic() {
    check_if_zero::<0>();
}

#[rusteria::test]
fn test_concrete_const_generic_vanish() {
    check_if_zero::<5>();
}

#[rusteria::test]
fn test_poly_const_generic<const C: usize>() {
    check_if_zero::<C>();
}

#[rusteria::test]
fn test_poly_const_generic2<const C: usize>() {
    match C {
        0 => {}
        1 => {}
        8 => {}
        15 => {}
        255 => {}
        _ => {}
    }
}
