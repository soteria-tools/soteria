pub struct Even {
    value: i32,
}

pub fn zero() -> Even {
    Even { value: 0 }
}

pub fn new(n: i32) -> Even {
    Even { value: n - n % 2 }
}

unsafe fn succ(x: Even) -> Even {
    if x.value < i32::MAX - 1 {
        Even { value: x.value + 1 }
    } else {
        x
    }
}

pub fn next(x: Even) -> Even {
    unsafe { succ(succ(x)) }
}

pub fn noop(x: Even) -> () {
    if x.value % 2 != 0 {
        unsafe { *(0 as *mut i32) = 1 }
    }
}
