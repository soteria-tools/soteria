#![feature(never_type)]

fn edge_cases<T: Default>(check: fn(*mut T)) {
    // reading a ZST through a null pointer is allowed
    check(std::ptr::null_mut());

    // through a dangling pointer too
    check(11 as *mut T);

    // through a freed-allocation
    check(unsafe {
        let b = Box::new(42);
        let ptr = Box::into_raw(b) as *mut T;
        let _ = Box::from_raw(ptr as *mut i32); // free the allocation
        ptr
    });

    // through a function pointer
    check((|| {}) as fn() as *mut T);

    // writing through a const address
    let const_addr = &mut 42 as *mut i32 as *mut T;
    unsafe { *const_addr = T::default() };
}

#[soteria::test]
fn access_zst() {
    edge_cases(|ptr: *mut ()| {
        let _zst = unsafe { *ptr };
        unsafe { *ptr = () };
    });
}

#[soteria::test]
fn get_discriminant_zst() {
    #[derive(Default, Clone, Copy)]
    enum MyZst {
        #[default]
        A,
    }

    edge_cases(|ptr: *mut MyZst| match unsafe { *ptr } {
        MyZst::A => {}
    });

    #[derive(Default, Clone, Copy)]
    enum ZstFromUninhabited {
        #[default]
        A,
        #[allow(dead_code)]
        B(!),
    }

    edge_cases(|ptr: *mut ZstFromUninhabited| match unsafe { *ptr } {
        ZstFromUninhabited::A => {}
        ZstFromUninhabited::B(_) => unreachable!(),
    });
}

#[soteria::test]
fn null_ptr_not_zst() {
    let ptr: *const u32 = std::ptr::null();
    let _val: u32 = unsafe { *ptr };
}

#[soteria::test]
fn dangling_ptr_not_zst() {
    let ptr: *const u8 = 0xdeadbeef as *const u8;
    let _val: u8 = unsafe { *ptr };
}
