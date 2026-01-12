#[rusteria::test]
fn null_ptr_zst() {
    let ptr: *const () = std::ptr::null();
    // this shouldn't fail; reading a ZST through a null pointer is allowed
    let zst: () = unsafe { *ptr };
}
