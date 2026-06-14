// A `*const u8` constant that points into a larger, over-aligned allocation,
// then reads through it at a wider type. Ensure we align it correctly.
//
// `ALIGNED` and `PTR` point into the *same* allocation, so they must compare
// equal: the allocation is a single object, not duplicated per reference.
#[repr(C, align(8))]
struct Aligned([u8; 8]);

const ALIGNED: &Aligned = &Aligned([0; 8]);
const PTR: *const u8 = ALIGNED.0.as_ptr();

fn main() {
    let q = PTR as *const u64;
    let v = unsafe { *q };
    assert_eq!(v, 0);

    assert!(ALIGNED as *const Aligned as *const u8 == PTR);
}
