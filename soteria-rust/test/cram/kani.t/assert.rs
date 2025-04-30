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

#[kani::proof]
fn override_assert_macro() {
    let b: bool = kani::any();
    assert!(b, "I used \"assert!\"");
}

#[kani::proof]
fn override_asserteq_macro() {
    let a: u32 = kani::any();
    let b: u32 = kani::any();
    assert_eq!(a, b, "I used \"assert_eq!\"");
}
