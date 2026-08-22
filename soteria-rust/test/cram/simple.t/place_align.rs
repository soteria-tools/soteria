#[repr(C, packed)]
struct Packed {
    fill: u8,
    field: u32,
}

#[repr(C)]
struct Aligned {
    fill: u8,
    field: u32,
}

// A packed struct is 1-aligned, so its fields can sit at any offset.
#[soteria::test]
fn access_packed_field() {
    let mut p = Packed { fill: 0, field: 42 };
    // Force `p` onto the heap, so the accesses below go through memory.
    let _ = &p.fill;
    assert_eq!({ p.field }, 42);
    p.field = 7;
    assert_eq!({ p.field }, 7);
}

// The relaxed alignment doesn't survive the raw pointer: reading a u32 through
// `q` requires a 4-aligned pointer.
#[soteria::test]
fn deref_ptr_to_packed_field() {
    let p = Packed { fill: 0, field: 42 };
    let q: *const u32 = std::ptr::addr_of!(p.field);
    let _ = unsafe { *q };
}

// Conversely, `fill` is 1-aligned but lives in a 4-aligned struct, so reaching
// it through a misaligned `*const Aligned` is UB.
#[soteria::test]
fn access_field_of_misaligned_struct() {
    let buf = [0u64; 4];
    let p = unsafe { buf.as_ptr().cast::<u8>().add(1) } as *const Aligned;
    let _ = unsafe { (*p).fill };
}

// Taking the address of a field doesn't access it, so it is allowed even when
// the place is misaligned for its struct.
#[soteria::test]
fn addr_of_field_of_misaligned_struct() {
    let buf = [0u64; 4];
    let p = unsafe { buf.as_ptr().cast::<u8>().add(1) } as *const Aligned;
    let q: *const u32 = unsafe { std::ptr::addr_of!((*p).field) };
    assert_eq!(unsafe { q.read_unaligned() }, 0);
}
