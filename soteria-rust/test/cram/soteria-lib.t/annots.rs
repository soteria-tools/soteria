#[soteria::test]
fn test1() {}
#[soteria::test]
fn test2() {}
#[soteria::test]
fn test3() {}

#[soteria::test]
#[soteria::branch_fuel(10)]
fn test_branch_fuel() {
    let n: u8 = soteria::nondet_bytes();
    soteria::assume(n < 10);
    let mut i = 0;
    while i < n {
        i += 1;
    }
}

#[soteria::test]
#[soteria::step_fuel(10000)]
fn test_step_fuel() {
    let mut i = 0;
    while i < 1000 {
        i += 1;
    }
}

#[soteria::test]
#[soteria::expect_fail]
fn test_expect_fail() {
    panic!("Oh no!");
}
