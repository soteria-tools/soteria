use std::cell::UnsafeCell;

struct MyCell<T> {
    value: UnsafeCell<T>,
}

impl<T> MyCell<T> {
    fn new(value: T) -> Self {
        Self {
            value: UnsafeCell::new(value),
        }
    }
}

fn main() {
    let c: MyCell<u32> = MyCell::new(1);
    let rmc: &MyCell<u32> = &c;
    let ruc: &UnsafeCell<u32> = &rmc.value;
    let p: *mut u32 = ruc as *const UnsafeCell<u32> as *const u32 as *mut u32;
    unsafe { std::ptr::replace(p, 2) };
}
