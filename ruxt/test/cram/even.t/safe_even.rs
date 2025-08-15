pub struct Even {
    value: Box<i32>,
}

pub fn zero() -> Even {
    let value = Box::new(0);
    Even { value }
}

pub fn new(n: i32) -> Even {
    let value = Box::new(n - n % 2);
    Even { value }
}

pub unsafe fn succ(x: Even) -> Even {
    if *(x.value) < i32::MAX - 1 {
        let value = Box::new(*(x.value) + 1);
        Even { value }
    } else {
        x
    }
}

pub fn next(x: Even) -> Even {
    unsafe { succ(succ(x)) }
}

pub fn noop(x: Even) -> () {
    if *(x.value) % 2 != 0 {
        unsafe { *(0 as *mut i32) = 1 }
    }
}
