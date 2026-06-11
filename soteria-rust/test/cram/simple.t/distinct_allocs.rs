static A: u8 = 1;
static B: u8 = 2;

// Distinct allocations have distinct base addresses, so pointers at the start
// of two different allocations can never compare equal. This used to be a false
// positive: the decay map handed out unconstrained addresses, letting the
// solver pick the same address for two provably-distinct allocations.
#[soteria::test]
fn distinct_allocs_dont_alias() {
    let local: u8 = 0;
    let p_local: *const u8 = &local;
    assert!(p_local != &A as *const u8);
    assert!(&A as *const u8 != &B as *const u8);
}
