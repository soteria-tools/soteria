fn access(x: &mut u32) {
    for _ in 0..2000 {
        *x += 1;
    }
}

fn main() {
    let mut x = 0;
    access(&mut x);
    soteria::assert(x == 2000, "ok");
}
