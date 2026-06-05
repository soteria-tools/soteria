const REF: &u8 = &11;

#[soteria::test]
fn write_to_const() {
    let ptr = REF as *const u8 as *mut u8;
    unsafe { *ptr = 67 };
}

#[soteria::test]
fn write_to_str() {
    let ptr = "hello" as *const str as *mut u8;
    unsafe { *ptr = 67 };
}
