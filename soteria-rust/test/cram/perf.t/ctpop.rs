#![feature(core_intrinsics)]

use std::intrinsics::ctpop;

fn main() {
    let x: u8 = soteria::nondet_bytes();
    let mut count: u32 = 0;
    for i in 0..u8::BITS {
        let bit = x & (1 << i);
        if bit != 0 {
            count += 1;
        }
    }
    assert!(count == ctpop(x));
}
