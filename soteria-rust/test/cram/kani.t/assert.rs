#[kani::proof]
fn assert_false() {
    let b: bool = kani::any();
    kani::assert(b, "Expected true!");
}

#[kani::proof]
fn fancy_assert_false() {
    let b: bool = kani::any();
    kani::assert(b, "👻 unicode is 𝒮𝒞𝒜ℛ𝒴");
}
