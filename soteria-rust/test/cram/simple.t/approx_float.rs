fn main() {
    let x: f32 = rusteria::nondet_bytes();
    rusteria::assume(x.is_finite());
    let cos = x.cos();

    assert!(cos >= -1.0 && cos <= 1.0);
}
