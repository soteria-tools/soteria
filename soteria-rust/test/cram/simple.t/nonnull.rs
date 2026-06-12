use std::ptr::NonNull;

type ON = Option<NonNull<usize>>;
type RN = Result<NonNull<usize>, usize>;

fn make_nonnull_opt() -> ON {
    let b = Box::new(67);
    Some(NonNull::new(Box::into_raw(b)).unwrap())
}

fn make_nonnull_res() -> RN {
    let b = Box::new(42);
    Ok(NonNull::new(Box::into_raw(b)).unwrap())
}

fn main() {
    match make_nonnull_opt() {
        Some(ptr) => unsafe {
            let b = Box::from_raw(ptr.as_ptr());
            assert!(*b == 67)
        },
        None => {
            unreachable!()
        }
    }
    match make_nonnull_res() {
        Ok(ptr) => unsafe {
            let b = Box::from_raw(ptr.as_ptr());
            assert!(*b == 42)
        },
        Err(_) => {
            unreachable!()
        }
    }
}

#[soteria::test]
fn null_is_none() {
    assert!(NonNull::<usize>::new(std::ptr::null_mut()).is_none());
}

#[soteria::test]
fn niche_ok() {
    let b = Box::new(91);
    let res: Result<NonNull<usize>, ()> = Ok(NonNull::new(Box::into_raw(b)).unwrap());
    match res {
        Ok(ptr) => unsafe {
            let b = Box::from_raw(ptr.as_ptr());
            assert!(*b == 91)
        },
        Err(()) => {
            unreachable!()
        }
    }
}

#[soteria::test]
fn niche_err() {
    let res: Result<NonNull<usize>, ()> = Err(());
    assert!(res.is_err());
}

#[soteria::test]
fn transmuted_discriminant() {
    let addr: usize = soteria::nondet_bytes();
    let opt: ON = unsafe { std::mem::transmute(addr) };
    match opt {
        Some(_) => assert!(addr != 0),
        None => assert!(addr == 0),
    }
}
