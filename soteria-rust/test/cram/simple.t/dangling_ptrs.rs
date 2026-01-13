#[rusteria::test]
fn null_ptr_zst() {
    let ptr: *const () = std::ptr::null();
    // this shouldn't fail; reading a ZST through a null pointer is allowed
    let zst: () = unsafe { *ptr };
}

#[rusteria::test]
fn null_ptr_not_zst() {
    let ptr: *const u32 = std::ptr::null();
    let _val: u32 = unsafe { *ptr };
}

#[rusteria::test]
fn dangling_ptr_not_zst() {
    let ptr: *const u8 = 0xdeadbeef as *const u8;
    let _val: u8 = unsafe { *ptr };
}
