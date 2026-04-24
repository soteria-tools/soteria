fn f() {}

fn main() {
    let ptr: fn() = f;
    let addr = ptr as usize;
    // we assume functions have alignment 16
    assert!(addr % 16 == 0);
}
