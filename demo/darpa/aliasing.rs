// Kani: kani demo/darpa/aliasing.rs
// Rusteria: dune exec -- soteria-rust obol demo/darpa/aliasing.rs --kani

/// An example of Tree Borrows violation, according to the paper.
/// We may only own one mutable reference to a memory location at a time;
/// here, using `x` invalidates `y`; and use of `y` is then UB.
#[kani::proof]
fn aliasing_error() {
    let mut root = 42;
    let ptr = &mut root as *mut i32;
    let (x, y) = unsafe { (&mut *ptr, &mut *ptr) };
    *x = 13;
    *y = 20; // UB: y is disabled
    let val = *x;
}
