struct PanickingDrop;

impl Drop for PanickingDrop {
    fn drop(&mut self) {
        panic!("panic in drop");
    }
}

#[soteria::test]
fn panic_in_drop_while_unwinding() {
    let _d = PanickingDrop;
    panic!("first panic");
}

struct CallingDrop;

impl Drop for CallingDrop {
    fn drop(&mut self) {
        panic_in_another_frame();
    }
}

fn panic_in_another_frame() {
    panic!("panic in another frame");
}

#[soteria::test]
fn nested_unwind_in_catch_unwind() {
    let _ = std::panic::catch_unwind(|| {
        let _d = CallingDrop;
        panic!("first panic");
    });
}
