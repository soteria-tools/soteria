struct A {
    x: i32,
    y: i32,
}

struct K {
    a: A,
    z: u32,
}

enum B {
    C,
    D,
}

struct Zst1;

enum Zst0 {
    Z(Zst1),
}

enum DeepZst {
    D(Zst0),
}

fn main() {
    let mut a = A { x: 0, y: 0 };
    a.x = 1;
    a.y = 2;

    let mut k = K {
        a: A { x: 0, y: 0 },
        z: 0,
    };
    k.a.x = 1;

    let mut b = B::C;
    b = B::D;
    let z = DeepZst::D(Zst0::Z(Zst1));
    let mut c: [u32; 4] = [0; 4];
    c[0] = 1;
    let mut d: [A; 2] = [A { x: 0, y: 0 }, A { x: 1, y: 1 }];
    d[0].x = 3;

    assert!(a.x == 1);
    assert!(a.y == 2);
    assert!(k.a.x == 1);
    assert!(k.a.y == 0);
    assert!(c[0] == 1);
    assert!(c[1] == 0);
    assert!(c[2] == 0);
    assert!(c[3] == 0);
    assert!(d[0].x == 3);
    assert!(d[0].y == 0);
    assert!(d[1].x == 1);
    assert!(d[1].y == 1);
}
