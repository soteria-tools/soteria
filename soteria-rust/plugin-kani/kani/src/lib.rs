// SPDX-License-Identifier: Apache-2.0 OR MIT

#![feature(f16)]
#![feature(f128)]
#![feature(register_tool)]
#![register_tool(kanitool)]

extern crate self as kani;

pub use kani_macros::*;

// Used to bind `core::assert` to a different name to avoid possible name conflicts if a
// crate uses `extern crate std as core`.
pub use core::assert as __kani__workaround_core_assert;

#[inline(never)]
#[kanitool::fn_marker = "panic"]
pub const fn panic(_message: &'static str) -> ! {
    unreachable!()
}

#[inline(never)]
#[kanitool::fn_marker = "assert"]
pub const fn assert(_cond: bool, _msg: &'static str) {}

#[inline(never)]
#[kanitool::fn_marker = "assume"]
pub const fn assume(_cond: bool) {}

#[inline(never)]
#[kanitool::fn_marker = "nondet"]
pub const fn nondet<T>() -> T {
    unreachable!()
}

pub mod invariant;
pub use invariant::Invariant;

mod arbitrary;
pub use arbitrary::*;

pub fn any<T: Arbitrary>() -> T {
    T::any()
}
