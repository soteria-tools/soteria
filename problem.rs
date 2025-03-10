#[kani::proof]
fn init_int() {
    let a = [4u8; 1];
    let i: usize = kani::any();
    kani::assume(i < 1);
    assert_eq!(a[i], 4);
}

#[kani::proof]
fn init_option() {
    let a = [Some(4u8); 1];
    let i: usize = kani::any();
    kani::assume(i < 1);
    assert_eq!(a[i], Some(4));
}
