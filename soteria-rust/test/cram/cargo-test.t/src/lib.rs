extern crate self as my_crate;

/// Returns 42.
pub fn get_answer() -> i32 {
    42
}

#[cfg(test)]
#[cfg(feature = "my_feature")]
mod test {
    #[test]
    fn test_in_src() {
        assert_eq!(super::get_answer(), 42);
    }
}
