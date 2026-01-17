#![feature(allocator_api)]

#[derive(Clone)]
pub struct Zst;

fn main() {
    let zst = Zst;
    let _zst_clone = zst.clone();

    let alloc = std::alloc::Global;
    let _alloc_clone = alloc.clone();
}
