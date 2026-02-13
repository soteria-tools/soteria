use std::mem::MaybeUninit;

#[rusteria::test]
fn test_uninit_ref() {
    let b32 = Box::new(MaybeUninit::<u32>::uninit());
    let as_ptr = b32.as_ptr() as *const u32;
    let as_ref: &u32 = unsafe { &*as_ptr };
    // the backing memory is uninit; whether this is UB is under discussion, though
    // the answer seems to be "this is ok"
    // see: https://github.com/rust-lang/unsafe-code-guidelines/issues/346
}

#[rusteria::test]
fn test_dangling_ref() {
    let b32 = Box::new(MaybeUninit::<u32>::uninit());
    let as_ptr = b32.as_ptr() as *const u32 as *const [u32; 2];
    let as_ref: &[u32; 2] = unsafe { &*as_ptr };
    // dangling! this must always be caught
}

#[rusteria::test]
fn test_unaligned_ref() {
    let b32 = Box::new(MaybeUninit::<[u32; 2]>::uninit());
    let as_ptr = b32.as_ptr() as *const [u32; 2] as *const u64;
    let as_ref: &u64 = unsafe { &*as_ptr };
    // unaligned! this must always be caught
}
