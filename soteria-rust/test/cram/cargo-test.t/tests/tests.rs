use my_crate::get_answer;

#[test]
fn test_ok() {
    assert_eq!(get_answer(), 42);
}

#[test]
fn test_nok() {
    assert_eq!(get_answer(), 67);
}

// Currently, we do not support "should_panic"
#[test]
#[should_panic]
fn test_nok_expected() {
    assert_eq!(get_answer(), 67);
}
