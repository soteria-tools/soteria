struct Wrapper<T> {
    x: T,
}

fn wrap<T>(x: T) -> Wrapper<T> {
    Wrapper { x }
}

#[rusteria::test]
fn wrap_stuff() {
    wrap(32u8);
    wrap(0.0f32);
    wrap(());
}
