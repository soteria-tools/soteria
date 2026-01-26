fn main() {
    let cond: bool = rusteria::nondet_bytes();
    let result = std::panic::catch_unwind(|| if cond { 11 } else { panic!("oh no!") });

    if cond {
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 11);
    } else {
        assert!(result.is_err());
    }
}
