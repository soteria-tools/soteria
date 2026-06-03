use std::num::NonZero;
use std::ptr::NonNull;

#[soteria::test]
fn nonnull_ok() {
    let _ptr = unsafe { NonNull::new_unchecked(1 as *mut i32) };
}

#[soteria::test]
fn nonnull() {
    let x: usize = soteria::nondet_bytes();
    let _ptr = unsafe { NonNull::new_unchecked(x as *mut i32) };
}

#[soteria::test]
fn nonzero_ok() {
    let _nz = unsafe { NonZero::new_unchecked(1isize) };
}

#[soteria::test]
fn nonzero_isize() {
    // NOTE: we use transmute because NonZero::new_unchecked transmutes into an
    // option and then manually UB-traps, so the diagnostic is a bit misleading
    let x = soteria::nondet_bytes();
    let _nz = unsafe { std::mem::transmute::<isize, NonZero<isize>>(x) };
}
