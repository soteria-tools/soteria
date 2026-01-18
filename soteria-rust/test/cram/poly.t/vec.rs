use std::mem;

#[rusteria::test]
fn with_vec<T>() {
    let my_vec: Vec<T> = Vec::with_capacity(10);
    if mem::size_of::<T>() == 0 {
        assert!(my_vec.capacity() == usize::MAX);
    } else {
        assert!(my_vec.capacity() == 10);
    }
}

#[rusteria::test]
fn with_vec_wrong<T>() {
    let my_vec: Vec<T> = Vec::with_capacity(10);
    assert!(my_vec.capacity() == 10);
}
