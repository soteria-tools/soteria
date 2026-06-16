#![feature(ptr_metadata)]

trait Trait {}
struct Struct;
impl Trait for Struct {}

fn main() {
    // Check we can get the metadata of a trait object pointer.
    let trait_object: *const dyn Trait = &Struct;
    let _vt = core::ptr::metadata(trait_object);
}
