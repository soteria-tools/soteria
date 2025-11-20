fn add(l: u8, r: u8) -> u8 {
    l + r
}

fn sub(l: u8, r: u8) -> u8 {
    l - r
}

fn call(f: fn(u8, u8) -> u8, l: u8, r: u8) -> u8 {
    f(l, r)
}

#[rusteria::test]
fn fn_ptr_call() {
    assert_eq!(call(add, 2, 3), 5);
    assert_eq!(call(sub, 5, 3), 2);
    assert_eq!(call(add, 10, 15), 25);
}

#[rusteria::test]
fn fn_ptr_read() {
    let add: fn(u8, u8) -> u8 = add;
    let ptr = add as *const u8;
    unsafe {
        let _b = *ptr;
    }
}

#[rusteria::test]
fn fn_ptr_write() {
    let add: fn(u8, u8) -> u8 = add;
    let ptr = add as *mut u8;
    unsafe {
        *ptr = 0;
    }
}
