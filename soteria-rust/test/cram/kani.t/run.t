Test kani::any
  $ soteria-rust exec any.rs --kani
  Compiling... done in <time>
  => Running any_bool...
  note: any_bool: done in <time>, ran 2 branches
  PC 1: (V|1| == 0x01) /\ (V|1| == 0x01)
  PC 2: (V|1| == 0x00) /\ (V|1| == 0x00)
  
  => Running any_i8...
  note: any_i8: done in <time>, ran 3 branches
  PC 1: (V|1| == 0x00) /\ (V|1| == 0x00)
  PC 2: (0x80 <=u V|1|)
  PC 3: (0x01 <=u V|1|) /\ (V|1| <=u 0x7f)
  
Test kani::assume
  $ soteria-rust exec assume.rs --kani
  Compiling... done in <time>
  => Running assume_bool...
  note: assume_bool: done in <time>, ran 1 branch
  PC 1: (V|1| == 0x01) /\ (V|1| == 0x01)
  
  => Running assume_i32...
  note: assume_i32: done in <time>, ran 1 branch
  PC 1: (0x00000001 <=u V|1|)
  
Test #[kani::should_panic]
  $ soteria-rust exec should_panic.rs --kani
  Compiling... done in <time>
  => Running when_at_the_disco...
  note: when_at_the_disco: done in <time>, ran 1 branch
  PC 1: empty
  
Test kani::assert
  $ soteria-rust exec assert.rs --kani
  Compiling... done in <time>
  => Running assert_false...
  error: assert_false: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: Expected true! in assert_false
      ┌─ $TESTCASE_ROOT/assert.rs:4:5
    2 │  fn assert_false() {
      │  ----------------- 1: Entry point
    3 │      let b: bool = kani::any();
    4 │      kani::assert(b, "Expected true!");
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │      │
      │      Triggering operation
      │      2: Call trace
  PC 1: (V|1| == 0x00) /\ (V|1| == 0x00)
  
  => Running fancy_assert_false...
  error: fancy_assert_false: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: 👻 unicode is 𝒮𝒞𝒜ℛ𝒴 in fancy_assert_false
      ┌─ $TESTCASE_ROOT/assert.rs:10:5
    8 │  fn fancy_assert_false() {
      │  ----------------------- 1: Entry point
    9 │      let b: bool = kani::any();
   10 │      kani::assert(b, "👻 unicode is 𝒮𝒞𝒜ℛ𝒴");
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │      │
      │      Triggering operation
      │      2: Call trace
  PC 1: (V|1| == 0x00) /\ (V|1| == 0x00)
  
  => Running override_assert_macro...
  error: override_assert_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert!" in override_assert_macro
      ┌─ $TESTCASE_ROOT/assert.rs:16:5
   14 │  fn override_assert_macro() {
      │  -------------------------- 1: Entry point
   15 │      let b: bool = kani::any();
   16 │      assert!(b, "I used \"assert!\"");
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │      │
      │      Triggering operation
      │      2: Call trace
  PC 1: (V|1| == 0x00) /\ (V|1| == 0x00)
  
  => Running override_asserteq_macro...
  error: override_asserteq_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert_eq!" in override_asserteq_macro
      ┌─ $TESTCASE_ROOT/assert.rs:23:5
   20 │  fn override_asserteq_macro() {
      │  ---------------------------- 1: Entry point
      ·  
   23 │      assert_eq!(a, b, "I used \"assert_eq!\"");
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │      │
      │      Triggering operation
      │      2: Call trace
  PC 1: (V|1| != V|2|)
  
  [1]

Test kani::slice::any_slice_of_array
  $ echo "Skipped; can't read symbolic slice" # soteria-rust exec any_slice.rs --kani
  Skipped; can't read symbolic slice

Test kani::vec::any_vec
  $ soteria-rust exec any_vec.rs --kani
  Compiling... done in <time>
  => Running len_capacity_invariant...
  note: len_capacity_invariant: done in <time>, ran 17 branches
  PC 1: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (V|1| == 0x0000000000000010) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (V|1| == 0x0000000000000010) /\
        (extract[0-1](V|18|) == 0b00)
  PC 2: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (0x0000000000000000 == V|1|) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000000 == V|1|) /\
        (extract[0-1](V|18|) == 0b00)
  PC 3: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (V|1| == 0x000000000000000f) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
        (V|19| <=u 0x7fffffffffffffc2) /\ (V|1| == 0x000000000000000f) /\
        (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 4: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (V|1| == 0x000000000000000e) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
        (V|19| <=u 0x7fffffffffffffc6) /\ (V|1| == 0x000000000000000e) /\
        (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 5: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (V|1| == 0x000000000000000d) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
        (V|19| <=u 0x7fffffffffffffca) /\ (V|1| == 0x000000000000000d) /\
        (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 6: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (V|1| == 0x000000000000000c) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
        (V|19| <=u 0x7fffffffffffffce) /\ (V|1| == 0x000000000000000c) /\
        (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 7: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (V|1| == 0x000000000000000b) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
        (V|19| <=u 0x7fffffffffffffd2) /\ (V|1| == 0x000000000000000b) /\
        (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 8: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (V|1| == 0x000000000000000a) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
        (V|19| <=u 0x7fffffffffffffd6) /\ (V|1| == 0x000000000000000a) /\
        (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 9: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
        (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
        (V|1| == 0x0000000000000009) /\ (0x0000000000000004 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
        (V|19| <=u 0x7fffffffffffffda) /\ (V|1| == 0x0000000000000009) /\
        (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 10: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (V|1| == 0x0000000000000008) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7fffffffffffffde) /\ (V|1| == 0x0000000000000008) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 11: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (V|1| == 0x0000000000000007) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7fffffffffffffe2) /\ (V|1| == 0x0000000000000007) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 12: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (V|1| == 0x0000000000000006) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7fffffffffffffe6) /\ (V|1| == 0x0000000000000006) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 13: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (V|1| == 0x0000000000000005) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7fffffffffffffea) /\ (V|1| == 0x0000000000000005) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 14: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (V|1| == 0x0000000000000004) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7fffffffffffffee) /\ (V|1| == 0x0000000000000004) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 15: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (V|1| == 0x0000000000000003) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7ffffffffffffff2) /\ (V|1| == 0x0000000000000003) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 16: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (V|1| == 0x0000000000000002) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7ffffffffffffff6) /\ (V|1| == 0x0000000000000002) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  PC 17: ((0x0000000000000010 - V|1|) <=u 0x3fffffffffffffff) /\
         (0x0000000000000000 <s (0x0000000000000010 - V|1|)) /\
         (0x0000000000000001 == V|1|) /\ (0x0000000000000004 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe) /\ (0x0000000000000004 <=u V|19|) /\
         (V|19| <=u 0x7ffffffffffffffa) /\ (0x0000000000000001 == V|1|) /\
         (extract[0-1](V|18|) == 0b00) /\ (0b00 == extract[0-1](V|19|))
  
Test our simple Kani demo works
  $ soteria-rust exec demo.rs --kani
  Compiling... done in <time>
  => Running saturating_add_overflow...
  error: saturating_add_overflow: found issues in <time>, errors in 1 branch (out of 3)
  error: Overflow in saturating_add_overflow
      ┌─ $TESTCASE_ROOT/demo.rs:11:8
    8 │  fn saturating_add_overflow() -> u32 {
      │  ----------------------------------- 1: Entry point
      ·  
   11 │      if a + b < u32::MAX {
      │         ^^^^^ Triggering operation
  PC 1: (V|1| +u_ovf V|2|)
  
  => Running saturating_add...
  note: saturating_add: done in <time>, ran 2 branches
  PC 1: (V|1| <u (0xffffffff -ck V|2|)) /\ !((V|1| +u_ovf V|2|))
  PC 2: ((0xffffffff -ck V|2|) <=u V|1|)
  
  => Running memory_leak...
  error: memory_leak: found issues in <time>, errors in 1 branch (out of 1)
  warning: Memory leak in memory_leak
      ┌─ $RUSTLIB/src/rust/library/alloc/src/alloc.rs:251:9
  251 │          self.alloc_impl(layout, false)
      │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          4: Allocation
      ┌─ $RUSTLIB/src/rust/library/alloc/src/boxed.rs:266:16
  266 │          return box_new(x);
      │                 ---------- 3: Call trace
      ┌─ $TESTCASE_ROOT/demo.rs:33:21
   32 │  fn memory_leak() {
      │  ---------------- 1: Leaking function
   33 │      let allocated = Box::new(11);
      │                      ------------ 2: Call trace
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running uninit_access...
  error: uninit_access: found issues in <time>, errors in 1 branch (out of 2)
  bug: Uninitialized memory access in uninit_access
      ┌─ $TESTCASE_ROOT/demo.rs:63:26
   58 │  fn uninit_access() {
      │  ------------------ 1: Entry point
      ·  
   63 │          let value: u32 = *addr_value;
      │                           ^^^^^^^^^^^ Memory load
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  [1]
