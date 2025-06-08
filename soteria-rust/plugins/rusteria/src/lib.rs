#![feature(register_tool)]
#![register_tool(rusteriatool)]

extern crate self as rusteria;

pub use rusteria_macros::*;

#[inline(never)]
pub const fn panic(_message: &'static str) -> ! {
    unreachable!()
}

#[inline(never)]
pub const fn assert(_cond: bool, _msg: &'static str) {}

#[inline(never)]
pub const fn assume(_cond: bool) {}

#[inline(never)]
pub const fn nondet<T>() -> T {
    unreachable!()
}
