fn ptr_dangling<T>() -> *const T {
    std::mem::align_of::<T>() as *const T
}

#[rusteria::test]
fn nondet_t<T>() {
    let mut x: T = rusteria::nondet_bytes();
    let mut y: T = x;
    if std::mem::size_of::<T>() == 0 {
        let ptr = ptr_dangling::<T>();
        let my_t: T = unsafe { ptr.read() };
        std::mem::drop(my_t);
    } else {
        let mut z: T = rusteria::nondet_bytes();
        x = z;
        z = y;
    }
}
