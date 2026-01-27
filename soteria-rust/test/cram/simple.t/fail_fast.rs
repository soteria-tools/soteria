fn main() {
    let cond: bool = rusteria::nondet_bytes();
    if cond {
        panic!("ok");
    } else {
        panic!("shouldn't appear");
    }
}
