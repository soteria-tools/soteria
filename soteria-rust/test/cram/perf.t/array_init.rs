use std::marker::PhantomData;
use std::ptr::NonNull;

struct LinkedList<T> {
    head: Option<NonNull<T>>,
    tail: Option<NonNull<T>>,
    _marker: PhantomData<*const T>,
}

fn main() {
    // A pattern that gets executed during tokio's runtime initialisation and caused performance issues in Soteria in the past.
    let arr: [LinkedList<u32>; 64] = std::array::from_fn(|_| LinkedList {
        head: None,
        tail: None,
        _marker: PhantomData,
    });
}
