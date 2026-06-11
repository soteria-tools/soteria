#![feature(ptr_metadata)]

fn main() {
    // Slice metadata (the length) is read straight from the pointer in the
    // store, without spilling it to the heap.
    let slice: *const [u32] = core::ptr::slice_from_raw_parts(core::ptr::null::<u32>(), 5);
    assert!(core::ptr::metadata(slice) == 5);

    // The metadata of a thin pointer is the unit value.
    let thin: *const u32 = core::ptr::null();
    let () = core::ptr::metadata(thin);

    // Check we can get the metadata of a trait object pointer.
    trait Trait {}
    struct Struct;
    impl Trait for Struct {}
    let trait_object: *const dyn Trait = &Struct;
    let _vt = core::ptr::metadata(trait_object);
}
