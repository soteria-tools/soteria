use std::cell::{Cell, RefCell};

// Examples taken from
// https://doc.rust-lang.org/std/macro.thread_local.html#syntax

#[rusteria::test]
fn pub_static_cell() {
    thread_local! {
        pub static FOO: Cell<u32> = const { Cell::new(1) };
    }

    assert_eq!(FOO.get(), 1);
}

#[rusteria::test]
fn static_ref_cell() {
    thread_local! {
       static BAR: RefCell<Vec<f32>> = RefCell::new(vec![1.0, 2.0]);
    }
    BAR.with_borrow(|v| assert_eq!(v[1], 2.0));
}

#[rusteria::test]
fn pub_static_from_const_expr() {
    thread_local! {
        pub static FOO_CONST: RefCell<Vec<u32>> = const { RefCell::new(Vec::new()) };
    }

    FOO_CONST.with_borrow(|v| assert_eq!(v.len(), 0));
}
