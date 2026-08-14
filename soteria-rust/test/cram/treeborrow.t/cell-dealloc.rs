// Deallocating memory that is strongly protected by an interior-mutable
// reference is allowed; see https://github.com/rust-lang/rust/issues/55005.
use std::cell::UnsafeCell;
use std::mem;

fn f(x: &UnsafeCell<i32>) {
    let b: Box<i32> = unsafe { Box::from_raw(x as *const _ as *mut i32) };
    drop(b)
}

fn main() {
    let b = Box::new(0i32);
    f(unsafe { mem::transmute(Box::into_raw(b)) });
}
