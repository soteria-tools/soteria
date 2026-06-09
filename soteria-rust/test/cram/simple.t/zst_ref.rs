enum Z {
    A,
}

fn main() {
    let x = Z::A;
    let zst_ref: *const Z = &x as *const Z;
    assert!(zst_ref == zst_ref);
}
