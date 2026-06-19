// A mix of Soteria harnesses (#[soteria::test], discovered and run by default)
// and an ordinary #[test] (discovered only under --libtest).
#[cfg(soteria)]
mod tests {
    use my_crate::get_answer;

    // Harness: passes — x is constrained to 42 before the assertion.
    #[soteria::test]
    fn harness_ok() {
        let x: i32 = soteria::nondet_bytes();
        soteria::assume(x == 42);
        assert_eq!(x, get_answer());
    }

    // Harness: finds a bug — x is unconstrained, so x != 42 is reachable.
    #[soteria::test]
    fn harness_nok() {
        let x: i32 = soteria::nondet_bytes();
        assert_eq!(x, get_answer());
    }

    // Harness expected to fail: the failing branch is the expected outcome.
    #[soteria::test]
    #[soteria::expect_fail]
    fn harness_nok_ok() {
        let x: i32 = soteria::nondet_bytes();
        assert_eq!(x, get_answer());
    }

    // Ordinary unit test: only discovered under --libtest.
    #[test]
    fn unit_in_test_target() {
        assert_eq!(get_answer(), 42);
    }
}
