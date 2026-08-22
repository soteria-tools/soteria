#![feature(allocator_api)]

use std::alloc::{AllocError, Allocator, Global, Layout};
use std::ptr::NonNull;

struct MyAlloc(u64);

unsafe impl Allocator for MyAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        Global.allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { Global.deallocate(ptr, layout) }
    }
}

#[soteria::test]
fn unsize_array_with_a_non_zst_allocator() {
    let boxed: Box<[u8; 3], MyAlloc> = Box::new_in([1, 2, 3], MyAlloc(0));
    let unsized_: Box<[u8], MyAlloc> = boxed;
    assert_eq!(unsized_.len(), 3);
    assert_eq!(unsized_[2], 3);
}

trait Trait {}
trait SubTrait: Trait {}
struct Unit;
impl Trait for Unit {}
impl SubTrait for Unit {}

#[soteria::test]
fn upcast_with_a_pointer_allocator() {
    let alloc = MyAlloc(0);
    let sub: Box<dyn SubTrait, &MyAlloc> = Box::new_in(Unit, &alloc);
    let _up: Box<dyn Trait, &MyAlloc> = sub;
}
