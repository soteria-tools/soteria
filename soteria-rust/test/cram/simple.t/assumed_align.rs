fn main() {
    let x = &mut [0u8; 3];
    let base_addr = x as *mut _ as usize;
    let base_addr_aligned = if base_addr % 2 == 0 {
        base_addr
    } else {
        base_addr + 1
    };
    let u16_ptr = base_addr_aligned as *mut u16;
    unsafe {
        *u16_ptr = 2;
    }
}
