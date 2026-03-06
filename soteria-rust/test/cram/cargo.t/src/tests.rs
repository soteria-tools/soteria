#[cfg(soteria)]
mod tests {
    use crate::MyOpt;

    fn any_my_opt() -> MyOpt<i32> {
        if soteria::nondet_bytes() {
            MyOpt::Some(42)
        } else {
            MyOpt::None
        }
    }

    #[soteria::test]
    fn my_test() {
        let opt = any_my_opt();
        soteria::assert(opt.is_some() || opt.is_none(), "Must be something");
    }
}
