#[cfg(soteria)]
mod tests {
    use my_crate::get_answer;

    #[test]
    fn test_ok() {
        let x: i32 = soteria::nondet_bytes();
        soteria::assume(x == 42);
        assert_eq!(x, get_answer());
    }

    #[test]
    fn test_nok() {
        let x: i32 = soteria::nondet_bytes();
        assert_eq!(x, get_answer());
    }

    #[test]
    #[soteria::expect_fail]
    fn test_nok_ok() {
        let x: i32 = soteria::nondet_bytes();
        assert_eq!(x, get_answer());
    }
}
