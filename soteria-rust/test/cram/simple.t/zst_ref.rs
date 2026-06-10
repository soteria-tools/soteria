#![feature(never_type)]

enum Z {
    A,
}

enum X {
    A(!),
    B,
}

fn main() {
    let z = Z::A;
    let zst_ref: *const Z = &z as *const Z;
    assert!(zst_ref == zst_ref);

    let x = X::B;
    match x {
        X::A(..) => unreachable!(),
        X::B => (),
    }
}
