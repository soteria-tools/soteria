extern crate self as my_crate;

pub enum MyOpt<T> {
    None,
    Some(T),
}

impl<T> MyOpt<T> {
    pub fn is_some(&self) -> bool {
        matches!(self, MyOpt::Some(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, MyOpt::None)
    }

    pub fn unwrap(self) -> T {
        match self {
            MyOpt::Some(value) => value,
            MyOpt::None => panic!("called `MyOpt::unwrap()` on a `None` value"),
        }
    }
}

mod tests;
