// we define our own non-generic option type to ensure
// Charon properly parses the layout too
enum MyOption<'a> {
    None,
    Some(&'a u32),
}

impl MyOption<'_> {
    fn is_none(&self) -> bool {
        matches!(self, MyOption::None)
    }

    fn is_some(&self) -> bool {
        matches!(self, MyOption::Some(_))
    }
}

const TUPLE: (u32, u32) = (1, 2);

fn none() -> MyOption<'static> {
    MyOption::None
}

fn some() -> MyOption<'static> {
    MyOption::Some(&42)
}

fn some_2() -> MyOption<'static> {
    MyOption::Some(&TUPLE.1)
}

fn main() {
    assert_eq!(size_of::<&u32>(), size_of::<MyOption>());
    assert!(none().is_none());
    assert!(some().is_some());
    // if we don't have the right checks, the sum of the allocation's address and
    // the offset could overflow and become 0, making the Option None.
    assert!(some_2().is_some());

    let mut x: MyOption = MyOption::None;
    assert!(x.is_none());
    x = MyOption::Some(&100);
    assert!(x.is_some());
    unsafe {
        let ptr = &mut x as *mut MyOption as *mut usize;
        *ptr = 0;
    }
    assert!(x.is_none());
}
