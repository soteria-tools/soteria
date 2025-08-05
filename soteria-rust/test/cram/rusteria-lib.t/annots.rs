#[rusteria::test]
fn test1() {}
#[rusteria::test]
fn test2() {}
#[rusteria::test]
fn test3() {}

#[rusteria::test]
#[rusteria::branch_fuel(10)]
fn test_branch_fuel() {
    let n: u8 = rusteria::nondet();
    rusteria::assume(n < 10);
    let mut i = 0;
    while i < n {
        i += 1;
    }
}

#[rusteria::test]
#[rusteria::step_fuel(10000)]
fn test_step_fuel() {
    let mut i = 0;
    while i < 1000 {
        i += 1;
    }
}

#[rusteria::test]
#[rusteria::expect_fail]
fn test_expect_fail() {
    panic!("Oh no!");
}
