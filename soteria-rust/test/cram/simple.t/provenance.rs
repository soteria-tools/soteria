fn main() {
    let mut x: u8 = 0;
    let p = &mut x as *mut u8;
    let p_int = p.expose_provenance();
    let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
    unsafe {
        *p_back = 1;
        assert!(*p == 1);
    }
}
