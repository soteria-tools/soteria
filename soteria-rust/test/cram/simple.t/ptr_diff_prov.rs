fn main() {
    let x = 11u8;
    let y = 12u8;
    // provenance of x
    let ptr1 = &x as *const u8;
    // provenance of y
    let ptr2 = &y as *const u8;
    // still has provenance of y, but address of x
    let ptr2 = ptr2.wrapping_sub((ptr2 as usize).wrapping_sub(ptr1 as usize));
    assert!(ptr1 == ptr2) // diff provenance, same address.
}
