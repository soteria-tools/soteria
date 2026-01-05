pub struct Even {
    value: i32,
}

pub fn zero() -> Even {
    Even { value: 0 }
}

pub fn new(n: i32) -> Even {
    Even { value: n - n % 2 }
}

fn succ(x: &mut Even) {
    if (*x).value < i32::MAX - 1 {
        (*x).value += 1
    }
}

pub fn next(x: &mut Even) {
    unsafe {
        succ(x);
        succ(x);
    }
}

pub fn noop(x: Even) -> () {
    let mut value = x.value;
    let ofs = (value % 2) as isize;
    let p = &raw mut value;
    unsafe { *p.offset(ofs) = value }
}
