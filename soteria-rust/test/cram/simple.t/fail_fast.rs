fn main() {
    let cond: bool = soteria::nondet_bytes();
    if cond {
        panic!("ok");
    } else {
        panic!("shouldn't appear");
    }
}
