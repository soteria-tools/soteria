fn main() {
    let x: f32 = soteria::nondet_bytes();
    soteria::assume(x.is_finite());
    let cos = x.cos();

    assert!(cos >= -1.0 && cos <= 1.0);
}
