fn main() {
    let cond: bool = rusteria::nondet();
    if cond {
        rusteria::assert(cond, "This holds");
    } else {
        rusteria::assume(cond);
        rusteria::panic("Unreachable");
    }
}
