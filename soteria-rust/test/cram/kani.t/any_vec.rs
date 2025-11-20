// Small test for kani::vec::any_vec, which can trigger errors
// if we don't do pointer offsets properly

#[kani::proof]
fn len_capacity_invariant() {
    let v = kani::vec::any_vec::<i32, 16>();
    assert!(v.capacity() >= v.len());
}
