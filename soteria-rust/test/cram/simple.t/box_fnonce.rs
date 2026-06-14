fn main() {
    (Box::new(&|| {}) as Box<dyn FnOnce()>)()
}
