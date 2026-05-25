macro_rules! access {
    ($x:expr, 1) => {
        *$x += 1;
    };
    ($x:expr, 2) => {
        access!($x, 1);
        access!($x, 1);
    };
    ($x:expr, 4) => {
        access!($x, 2);
        access!($x, 2);
    };
    ($x:expr, 8) => {
        access!($x, 4);
        access!($x, 4);
    };
    ($x:expr, 16) => {
        access!($x, 8);
        access!($x, 8);
    };
    ($x:expr, 32) => {
        access!($x, 16);
        access!($x, 16);
    };
    ($x:expr, 64) => {
        access!($x, 32);
        access!($x, 32);
    };
    ($x:expr, 128) => {
        access!($x, 64);
        access!($x, 64);
    };
    ($x:expr, 256) => {
        access!($x, 128);
        access!($x, 128);
    };
    ($x:expr, 512) => {
        access!($x, 256);
        access!($x, 256);
    };
    ($x:expr, 1000) => {
        access!($x, 512);
        access!($x, 256);
        access!($x, 128);
        access!($x, 64);
        access!($x, 32);
        access!($x, 8);
    };
    ($x:expr, 2000) => {
        access!($x, 1000);
        access!($x, 1000);
    };
    ($x:expr, 4000) => {
        access!($x, 2000);
        access!($x, 2000);
    };
    ($x:expr, 8000) => {
        access!($x, 4000);
        access!($x, 4000);
    };
    ($x:expr, 10000) => {
        access!($x, 8000);
        access!($x, 2000);
    };
}

fn access(x: &mut u32) {
    access!(x, 10000);
}

fn main() {
    let mut x = 0;
    access(&mut x);
    assert_eq!(x, 10_000);
}
