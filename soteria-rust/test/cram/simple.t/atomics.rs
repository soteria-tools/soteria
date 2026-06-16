#![feature(strict_provenance_atomic_ptr)]
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU32, Ordering::SeqCst};

#[soteria::test]
fn bitwise() {
    let a = AtomicU32::new(0b1100);
    assert!(a.fetch_and(0b1010, SeqCst) == 0b1100);
    assert!(a.fetch_or(0b0001, SeqCst) == 0b1000);
    assert!(a.fetch_xor(0b1111, SeqCst) == 0b1001);
    assert!(a.load(SeqCst) == 0b0110);

    let b = AtomicBool::new(true);
    assert!(b.fetch_nand(true, SeqCst) == true);
    assert!(b.load(SeqCst) == false);
}

#[soteria::test]
fn min_max_sub() {
    let a = AtomicU32::new(10);
    assert!(a.fetch_sub(3, SeqCst) == 10);
    assert!(a.fetch_max(20, SeqCst) == 7);
    assert!(a.fetch_min(5, SeqCst) == 20);
    assert!(a.load(SeqCst) == 5);

    // unsigned comparison: u32::MAX is the largest value
    let u = AtomicU32::new(1);
    assert!(u.fetch_max(u32::MAX, SeqCst) == 1);
    assert!(u.load(SeqCst) == u32::MAX);

    // signed comparison: -10 < 2
    let s = AtomicI32::new(-3);
    assert!(s.fetch_max(2, SeqCst) == -3);
    assert!(s.fetch_min(-10, SeqCst) == 2);
    assert!(s.load(SeqCst) == -10);
}

#[soteria::test]
fn pointer() {
    // Bitwise atomics on a pointer operate on its address.
    let q = AtomicPtr::<u8>::new(std::ptr::without_provenance_mut(0b1100));
    assert!(q.fetch_or(0b0011, SeqCst).addr() == 0b1100);
    assert!(q.fetch_and(0b1010, SeqCst).addr() == 0b1111);
    assert!(q.fetch_xor(0b0101, SeqCst).addr() == 0b1010);
    assert!(q.load(SeqCst).addr() == 0b1111);
}
