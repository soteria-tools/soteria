use std::any::{Any, TypeId};

fn main() {
    assert_eq!(TypeId::of::<u64>(), TypeId::of::<u64>());
    assert_ne!(TypeId::of::<u64>(), TypeId::of::<usize>());

    let boxed: Box<dyn Any> = Box::new(42u32);
    assert!(boxed.is::<u32>());
    assert!(!boxed.is::<u64>());
    assert_eq!(*boxed.downcast::<u32>().unwrap(), 42);

    let borrowed: &dyn Any = &"hello";
    assert_eq!(borrowed.downcast_ref::<&str>(), Some(&"hello"));
    assert!(borrowed.downcast_ref::<u8>().is_none());
}
