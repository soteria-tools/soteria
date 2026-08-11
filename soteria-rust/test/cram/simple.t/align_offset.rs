// The alignment of an allocation tells us the address of a pointer into it
// modulo that alignment, so `align_offset` can be answered exactly, without
// ever looking at the (symbolic) address.
#[soteria::test]
fn offset_within_the_allocation_alignment() {
    let arr = [0u32; 4];
    let bytes = unsafe { std::slice::from_raw_parts(arr.as_ptr() as *const u8, 16) };

    assert_eq!(bytes.as_ptr().align_offset(4), 0);
    assert_eq!(bytes[1..].as_ptr().align_offset(4), 3);
    assert_eq!(bytes[2..].as_ptr().align_offset(2), 0);
    assert_eq!(arr.as_ptr().align_offset(4), 0);

    // `align_to` builds on `align_offset`.
    let (head, body, tail) = unsafe { bytes.align_to::<u32>() };
    assert_eq!(head.len(), 0);
    assert_eq!(body.len(), 4);
    assert_eq!(tail.len(), 0);
}

// More alignment than the allocation guarantees: nothing pins its address down,
// so we assume it sits somewhere we can answer for rather than branch on it.
#[soteria::test]
fn offset_beyond_the_allocation_alignment() {
    let arr = [0u32; 4];
    let bytes = unsafe { std::slice::from_raw_parts(arr.as_ptr() as *const u8, 16) };

    let offset = bytes.as_ptr().align_offset(8);
    assert_eq!(bytes.as_ptr().wrapping_add(offset).addr() % 8, 0);
}

// Once an allocation's address has been assumed to sit somewhere, asking again
// at a different offset must be answered from that assumption rather than
// assumed afresh -- assuming twice would be assuming a contradiction, and the
// path would vanish without a word.
#[soteria::test]
fn offset_asked_twice_for_one_allocation() {
    let data = [1u8; 64];
    let first = data[0..].as_ptr().align_offset(8);
    let second = data[1..].as_ptr().align_offset(8);
    assert_eq!(second, (8 - (first + 1) % 8) % 8);
}

// A pointer whose address is known outright needs no guessing at all.
#[soteria::test]
fn offset_for_a_known_address() {
    let p = std::ptr::without_provenance::<u8>(0x4000_0004);
    assert_eq!(p.align_offset(4), 0);
    assert_eq!(p.align_offset(8), 4);
    assert_eq!(p.align_offset(64), 60);
}

// The offset is a number of elements, not of bytes, so it solves a congruence:
// stepping by the stride may have to wrap around the alignment several times
// before landing on an aligned address.
#[soteria::test]
fn offset_wrapping_around_the_alignment() {
    // The gap to the next 8-aligned address is 7 bytes, which is not a multiple
    // of the 3-byte stride -- but 5 steps, i.e. 15 bytes, get there.
    let p = std::ptr::without_provenance::<[u8; 3]>(0x1001);
    assert_eq!(p.align_offset(8), 5);
    assert_eq!(p.wrapping_add(5).addr() % 8, 0);
}

// An odd address stepping by an even stride stays odd, so nothing aligns it;
// same for a byte offset that isn't a whole number of elements.
#[soteria::test]
fn offset_that_cannot_be_given() {
    let p = std::ptr::without_provenance::<[u8; 2]>(0x1001);
    assert_eq!(p.align_offset(8), usize::MAX);

    let arr = [0u32; 4];
    let bytes = unsafe { std::slice::from_raw_parts(arr.as_ptr() as *const u8, 16) };
    assert_eq!(bytes[1..].as_ptr().cast::<u32>().align_offset(4), usize::MAX);
}

// Stubbing `align_offset` must not swallow the power-of-two check, which lives
// in the caller.
#[soteria::test]
fn alignment_is_not_a_power_of_two() {
    let arr = [0u8; 8];
    let _ = arr.as_ptr().align_offset(std::hint::black_box(3));
}
