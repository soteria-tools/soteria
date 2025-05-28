#[kani::proof]
fn main() {
    let arr = [1, 2, 3];
    // Any one of:
    // {[], [1], [2], [3], [1, 2], [2, 3], [1, 2, 3]}
    let slice = kani::slice::any_slice_of_array(&arr);
    let len = slice.len();
    assert!(
        len >= 0 && len <= 3,
        "Expected slice length to be between 0 and 3. Got {}.",
        len
    );
    let mut i = 0;
    while i < len {
        let elem = slice[i];
        assert!(
            elem == 1 || elem == 2 || elem == 3,
            "Expected a value of 1, 2, or 3 for the element at index {}. Got {}.",
            i,
            elem
        );
        i += 1;
    }
}
