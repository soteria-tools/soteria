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

fn main() {
    let a: Foo<i64> = Foo::Bar(1);
    let b: Foo<u128> = Foo::Baz;
    // let c = Foo::Bar('L');

    is_bar(a);
    is_bar(b);
    // is_bar(c);
}
