#![feature(register_tool)]
#![register_tool(soteriatool)]

extern crate self as soteria;

pub use soteria_macros::*;

#[inline(never)]
pub const fn panic(_message: &'static str) -> ! {
    unreachable!()
}

#[inline(never)]
pub const fn assert(_cond: bool, _msg: &'static str) {}

#[inline(never)]
pub const fn assume(_cond: bool) {}

#[inline(never)]
pub const fn nondet_bytes<T>() -> T {
    unreachable!()
}
