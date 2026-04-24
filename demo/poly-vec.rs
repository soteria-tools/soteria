// dune exec -- soteria-rust exec demo/poly-vec.rs --frontend charon --poly --pcs

use std::mem;

#[soteria::test]
fn with_capacity_wrong<T>() {
    let my_vec: Vec<T> = Vec::with_capacity(10);
    assert!(my_vec.capacity() == 10);
}

#[soteria::test]
fn with_capacity<T>() {
    let my_vec: Vec<T> = Vec::with_capacity(10);
    if mem::size_of::<T>() == 0 {
        assert!(my_vec.capacity() == usize::MAX);
    } else {
        assert!(my_vec.capacity() == 10);
    }
}
