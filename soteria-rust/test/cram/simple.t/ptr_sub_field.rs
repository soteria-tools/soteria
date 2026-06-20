// Regression test: a field access through a pointer derived from `ptr.sub` with
// a *symbolic* index. `base.sub(idx)` stores a wrapping ("negative") offset; a
// subsequent field projection used to be wrongly flagged as a dangling pointer
// because the offset was checked with unsigned overflow. This is the pattern
// hashbrown uses to address buckets, so symbolic-key lookups depend on it.
fn main() {
    let arr = [(1u8, 10u8), (2, 20), (3, 30), (4, 40)];
    let h: usize = soteria::nondet_bytes();
    let idx = h & 3; // in [0, 3]
    unsafe {
        let base = arr.as_ptr().add(4); // one-past-end
        let bucket = base.sub(idx + 1); // arr[3 - idx]
        let val = &(*bucket).1; // field projection on a sub-derived pointer
        assert!(*val >= 10);
    }
}
