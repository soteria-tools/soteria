fn coinflip(b: &bool) -> i32 {
    if rusteria::nondet_bytes() {
        if *b {
            42
        } else {
            0
        }
    } else {
        panic!("tough luck");
    }
}

struct Foo {
    b: bool,
    c: char,
    i: i32,
}

fn coinflip_but_struct(f1: &Foo, f2: &Foo) -> i32 {
    if rusteria::nondet_bytes() {
        if f1.b || !f2.b {
            f1.i
        } else if f2.b {
            f2.i
        } else {
            0
        }
    } else {
        panic!("tough luck again!");
    }
}
