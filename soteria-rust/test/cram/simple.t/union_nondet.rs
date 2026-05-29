#[repr(C)]
union Vec128Storage {
    d: [u32; 4],
    q: [u64; 2],
}

#[soteria::test]
fn read_d0() -> u32 {
    let s: Vec128Storage = soteria::nondet_bytes();
    unsafe { s.d[0] }
}
