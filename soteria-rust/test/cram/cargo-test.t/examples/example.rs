use my_crate::get_answer;

fn main() {
    println!("{}", get_answer());
}

#[test]
fn example_ok() {
    assert_eq!(get_answer(), 42);
}

#[test]
fn example_nok() {
    assert_eq!(get_answer(), 67);
}
