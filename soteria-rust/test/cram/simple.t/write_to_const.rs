const REF: &u8 = &11;

fn main() {
    let ptr = REF as *const u8 as *mut u8;
    unsafe { *ptr = 67 };
}
