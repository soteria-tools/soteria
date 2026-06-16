fn main() {
    // Direct cast to `Box<dyn FnOnce()>` and call (the original reproducer).
    (Box::new(&|| {}) as Box<dyn FnOnce()>)();

    // `FnOnce` capturing a value and returning one.
    let x = 42i32;
    let f: Box<dyn FnOnce() -> i32> = Box::new(move || x + 1);
    assert!(f() == 43);

    // `FnOnce` taking arguments.
    let g: Box<dyn FnOnce(i32, i32) -> i32> = Box::new(|a, b| a * b);
    assert!(g(6, 7) == 42);

    // `Fn` and `FnMut` trait objects are called through the same path.
    let h: Box<dyn Fn(i32) -> i32> = Box::new(|y| y * 2);
    assert!(h(10) == 20);

    let mut counter = 0;
    let mut i: Box<dyn FnMut() -> i32> = Box::new(move || {
        counter += 1;
        counter
    });
    assert!(i() == 1);
    assert!(i() == 2);
}
