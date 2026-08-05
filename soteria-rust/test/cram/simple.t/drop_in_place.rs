static mut DROPPED: u8 = 0;

struct D;

impl Drop for D {
    fn drop(&mut self) {
        unsafe { DROPPED += 1 }
    }
}

trait T {}
impl T for D {}

fn main() {
    unsafe {
        let sized = Box::into_raw(Box::new(D));
        std::ptr::drop_in_place(sized);
        assert!(DROPPED == 1);

        let unsized_: *mut dyn T = Box::into_raw(Box::new(D) as Box<dyn T>);
        std::ptr::drop_in_place(unsized_);
        assert!(DROPPED == 2);

        // The values are dropped, but the allocations are still ours to free.
        drop(Box::from_raw(sized as *mut std::mem::ManuallyDrop<D>));
        drop(Box::from_raw(unsized_ as *mut std::mem::ManuallyDrop<D>));
    }
}
