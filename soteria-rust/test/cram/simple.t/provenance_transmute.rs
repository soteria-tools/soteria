#[rusteria::test]
fn addr_doesnt_expose() {
    let mut x: u8 = 0;
    let p = &mut x as *mut u8;
    let p_int = p.addr();
    // this will not return the provenance information, since it was never exposed!
    let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
    unsafe {
        *p_back = 1;
        assert!(*p == 1);
    }
}

#[rusteria::test]
fn transmute_doesnt_restore_provenance() {
    let mut x: u8 = 0;
    let p = &mut x as *mut u8;
    let p_int = p.expose_provenance();
    // this will not return the provenance information, because it's a transmute!
    let p_back = unsafe { std::mem::transmute::<usize, *mut u8>(p_int) };
    unsafe {
        *p_back = 1;
        assert!(*p == 1);
    }
}
