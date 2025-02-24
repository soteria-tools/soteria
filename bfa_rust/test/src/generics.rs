enum Foo<T> {
    Bar(T),
    Baz,
}

fn is_bar<T>(f: Foo<T>) -> bool {
    match f {
        Foo::Bar(_) => true,
        _ => false,
    }
}

pub fn any<T>(concrete: T) -> T {
    concrete
}

fn main() {
    let a: Foo<i64> = any(Foo::Bar(1));
    let b: Foo<u128> = any(Foo::Baz);

    is_bar(a);
    is_bar(b);
}
