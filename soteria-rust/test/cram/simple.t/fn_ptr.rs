fn add(l: u8, r: u8) -> u8 {
    l + r
}

fn sub(l: u8, r: u8) -> u8 {
    l - r
}

fn call(f: fn(u8, u8) -> u8, l: u8, r: u8) -> u8 {
    f(l, r)
}

fn main() {
    assert_eq!(call(add, 2, 3), 5);
    assert_eq!(call(sub, 5, 3), 2);
    assert_eq!(call(add, 10, 15), 25);
}
