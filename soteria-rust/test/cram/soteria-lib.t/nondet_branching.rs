struct B(bool);

enum Pair<A, B> {
    A(A),
    B(B),
}

struct S {
    x: u32,
    y: B,
    z: Pair<char, bool>,
}

enum BigEnum {
    A(u8),
    B(i8),
    C(u16),
    D(i16),
    E(u32),
    F(i32),
    G(u64),
    H(i64),
    I(u128),
    J(i128),
}

#[soteria::test]
fn scalar_enum() {
    let mut x: S = soteria::nondet_bytes();
    x.x = 0;
    x.y.0 = true;
    let r = &mut x;
    r.x = 1;
    r.y.0 = false;
    let rx = &mut x.x;
    *rx = 2;
    let ry = &mut x.y;
    ry.0 = true;
}

#[soteria::test]
fn enum_large() {
    let mut e: [BigEnum; 3] = soteria::nondet_bytes();
    let r = &mut e;
    let discr_0 = std::mem::discriminant(&r[0]);
    e[1] = BigEnum::A(1);
}
