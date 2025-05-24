use std::cell::UnsafeCell;

fn main() {
    unsafe {
        let data = &mut UnsafeCell::new(0u8);
        let x = &*data;
        let y = &*data;
        // y and x tolerate alternating Writes
        *y.get() = 1;
        *x.get() = 2;
        *y.get() = 3;
        *x.get() = 4;
    }
}
