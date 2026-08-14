// `f` may mutate through the raw pointer, but it may not deallocate, as `x` is
// strongly protected.
fn inner(x: &mut i32, f: fn(*mut i32)) {
    f(x)
}

fn main() {
    inner(Box::leak(Box::new(0)), |raw| {
        drop(unsafe { Box::from_raw(raw) });
    });
}
