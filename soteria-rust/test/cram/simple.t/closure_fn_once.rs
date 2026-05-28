// Minimal repro for an obol translation bug.
//
// `Atomic<usize>` desugars to `<usize as AtomicPrimitive>::AtomicInner`, an
// associated-type projection that obol's `translate_ty` does not yet handle
// (translate_types.rs ~line 153). When obol fails to translate the body of a
// closure, it substitutes an `error(...)` body but still emits a signature
// for the function -- and that fallback signature drops the args tuple from
// `signature.inputs`. The closure body ends up declared with 1 input (just
// the closure self), but the FnOnce ABI calls it with 2 (self, args_tuple),
// causing soteria-rust to panic on `List.combine3 args in_tys exp_tys` in
// `interp.ml`.
//
// The same shape is what blows up on `std::thread::lifecycle::spawn_unchecked
// ::{closure#0}` in real code, because that closure transitively touches
// `Atomic<usize>` via `MIN`.

#![feature(generic_atomic)]

use std::sync::atomic::{Atomic, AtomicUsize, Ordering};

fn invoke<F: FnOnce() -> i32>(f: F) -> i32 {
    f()
}

fn main() {
    static MIN: Atomic<usize> = AtomicUsize::new(0);
    let _ = invoke(|| {
        MIN.fetch_add(1, Ordering::Relaxed);
        42
    });
}
