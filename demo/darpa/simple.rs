// Kani: kani ./demo/darpa/simple.rs -Zuninit-checks --output-format terse
// Rusteria: dune exec -- soteria-rust rustc ./demo/darpa/simple.rs --kani --summary

/// A classic overflow error when checking before adding two numbers
#[kani::proof]
#[kani::should_panic]
fn overflow() -> u32 {
    let a: u32 = kani::any();
    let b: u32 = kani::any();
    if a + b < u32::MAX {
        a + b
    } else {
        u32::MAX
    }
}

/// A fix to this, which passes the analysis
#[kani::proof]
fn overflow_fixed() -> u32 {
    let a: u32 = kani::any();
    let b: u32 = kani::any();
    if a < u32::MAX - b {
        a + b
    } else {
        u32::MAX
    }
}

/// Rusteria can detect memory leaks, caused by dynamically allocated memory
#[kani::proof]
#[kani::should_panic]
fn memory_leak() {
    let allocated = Box::new(11);
    std::mem::forget(allocated);
}

extern crate kani;
use kani::Arbitrary;

#[repr(u32)]
enum MyOption<T> {
    None,
    Some(T),
}

impl<T: Arbitrary> Arbitrary for MyOption<T> {
    fn any() -> Self {
        if bool::any() {
            MyOption::Some(T::any())
        } else {
            MyOption::None
        }
    }
}

/// Rusteria can reliably detected uninitialised memory access
#[kani::proof]
#[kani::should_panic]
fn uninit_access() {
    unsafe {
        let any_option: MyOption<u32> = kani::any();
        let addr: *const u32 = &any_option as *const MyOption<u32> as *const u32;
        let addr_value = addr.offset(1);
        let value: u32 = *addr_value;
    }
}
