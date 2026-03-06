fn main() {
    let cond: bool = soteria::nondet_bytes();
    if cond {
        soteria::assert(cond, "This holds");
    } else {
        soteria::assume(cond);
        soteria::panic("Unreachable");
    }
}
