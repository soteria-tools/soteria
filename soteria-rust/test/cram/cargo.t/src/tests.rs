#[cfg(rusteria)]
mod tests {
    use crate::MyOpt;

    fn any_my_opt() -> MyOpt<i32> {
        if rusteria::nondet() {
            MyOpt::Some(42)
        } else {
            MyOpt::None
        }
    }

    #[rusteria::test]
    fn my_test() {
        let opt = any_my_opt();
        rusteria::assert(opt.is_some() || opt.is_none(), "Must be something");
    }
}
