fn main() {
    let x: bool = soteria::nondet_bytes();
    let y: bool = soteria::nondet_bytes();
    let output: bool = x | y;
    assert!(output);
}
