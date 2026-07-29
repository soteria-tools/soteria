fn loop_incr<const N: u32>(r: &mut u32) {
    let _old = *r;
    *r = 0;
    for _ in 0..N {
        *r += 1;
    }
    assert_eq!(*r, N);
}

fn main() {
    loop_incr::<2000>(&mut 0);
}
