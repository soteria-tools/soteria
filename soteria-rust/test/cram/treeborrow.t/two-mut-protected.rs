// This function checks that there is no issue with having two mutable references
// from the same allocation both under a protector.
// This is safe code, it must absolutely not be UB.
// This test failing is a symptom of forgetting to check that only initialized
// locations can cause protector UB.
fn main() {
    fn write_second(_x: &mut u8, y: &mut u8) {
        // write through `y` will make some locations of `x` (protected)
        // become Disabled. Those locations are outside of the range on which
        // `x` is initialized, and the protector must not trigger.
        *y = 1;
    }

    let mut data = (0u8, 1u8);
    write_second(&mut data.0, &mut data.1);
}
