fn non_manifest(x: i32, y: i32) -> i32 {
    x + y // can overflow, but not on all inputs
}

fn manifest(x: i32) -> i32 {
    let mut z = x;
    z /= z - x; // always a division by zero
    z
}

fn half_manifest() -> i32 {
    if rusteria::nondet_bytes() {
        return 11;
    } else {
        panic!("aha!");
    }
}
