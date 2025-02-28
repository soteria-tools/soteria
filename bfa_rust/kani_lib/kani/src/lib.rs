#![feature(register_tool)]
#![register_tool(kanitool)]

extern crate self as kani;

pub use kani_macros::*;

pub fn any<T>() -> T {
    panic!();
}

#[inline(never)]
#[kanitool::fn_marker = "AssertHook"]
pub const fn assert(_cond: bool, _msg: &'static str) {}
