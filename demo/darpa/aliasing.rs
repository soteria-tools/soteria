// Kani: kani demo/darpa/aliasing.rs
// Rusteria: dune exec -- soteria-rust obol demo/darpa/aliasing.rs --kani

/// An example of Tree Borrows violation, according to the paper.
/// We may only own one mutable reference to a memory location at a time;
/// here, using `x` invalidates `y`; and use of `y` is then UB.
#[kani::proof]
#[kani::should_panic]
fn aliasing_error() {
    let mut root = 42;
    let ptr = &mut root as *mut i32;
    let (x, y) = unsafe { (&mut *ptr, &mut *ptr) };
    *x = 13;
    *y = 20; // UB: y is disabled
    let val = *x;
}

use std::cell::UnsafeCell;

/// Of course Rust has an escape hatch, if this sort of behaviour is desired;
/// `UnsafeCell`s allow "interor mutability".
#[kani::proof]
fn aliasing_ok_unsafe_cell() {
    let mut root = &mut UnsafeCell::new(42u32);
    let (x, y) = (&*root, &*root);
    unsafe { *x.get() = 13 };
    unsafe { *y.get() = 20 }; // this is ok, because we're using unsafe cell!
    let val = unsafe { *x.get() }; // = 20
}
