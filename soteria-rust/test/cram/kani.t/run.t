Test kani::any
  $ soteria-rust rustc any.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: any_bool: done in <time>, ran 2 branches
  PC 1: (0x01 == V|1|) /\ (0x01 == V|1|)
  PC 2: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  note: any_i8: done in <time>, ran 3 branches
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  PC 2: (0x80 <=u V|1|)
  PC 3: (0x01 <=u V|1|) /\ (V|1| <=u 0x7f)
  


Test kani::assume
  $ soteria-rust rustc assume.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: assume_bool: done in <time>, ran 1 branch
  PC 1: (0x01 == V|1|) /\ (0x01 == V|1|)
  
  note: assume_i32: done in <time>, ran 1 branch
  PC 1: (0x00000001 <=u V|1|)
  


Test #[kani::should_panic]
  $ soteria-rust rustc should_panic.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: when_at_the_disco: done in <time>, ran 1 branch
  PC 1: empty
  


Test kani::assert
  $ soteria-rust rustc assert.rs --clean --no-timing --kani
  Compiling... done in <time>
  error: assert_false: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: Expected true! in assert_false
      ┌─ $TESTCASE_ROOT/assert.rs:4:6
    2 │  fn assert_false() {
      │   ----------------- 1: Entry point
    3 │      let b: bool = kani::any();
    4 │      kani::assert(b, "Expected true!");
      │       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │       │
      │       Triggering memory operation
      │       2: Call trace
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  error: fancy_assert_false: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: 👻 unicode is 𝒮𝒞𝒜ℛ𝒴 in fancy_assert_false
      ┌─ $TESTCASE_ROOT/assert.rs:10:6
    8 │  fn fancy_assert_false() {
      │   ----------------------- 1: Entry point
    9 │      let b: bool = kani::any();
   10 │      kani::assert(b, "👻 unicode is 𝒮𝒞𝒜ℛ𝒴");
      │       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │       │
      │       Triggering memory operation
      │       2: Call trace
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  error: override_assert_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert!" in override_assert_macro
      ┌─ $SOTERIA-RUST/std/src/lib.rs:23:10
   23 │          rusteria::assert(!!$cond, concat!(stringify!($($arg)+)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering memory operation
      │           2: Call trace
      ┌─ $TESTCASE_ROOT/assert.rs:14:2
   14 │  fn override_assert_macro() {
      │   -------------------------- 1: Entry point
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  error: override_asserteq_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert_eq!" in override_asserteq_macro
      ┌─ $SOTERIA-RUST/std/src/lib.rs:23:10
   23 │          rusteria::assert(!!$cond, concat!(stringify!($($arg)+)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering memory operation
      │           2: Call trace
      ┌─ $TESTCASE_ROOT/assert.rs:20:2
   20 │  fn override_asserteq_macro() {
      │   ---------------------------- 1: Entry point
  PC 1: (V|1| != V|2|)
  
  [1]

Test kani::slice::any_slice_of_array
  $ echo "Skipped; can't read symbolic slice" # soteria-rust rustc any_slice.rs --clean --no-timing --kani
  Skipped; can't read symbolic slice

Test kani::vec::any_vec
  $ soteria-rust rustc any_vec.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: len_capacity_invariant: done in <time>, ran 17 branches
  PC 1: (extract[0-1](V|18|) == 0b00) /\ (0x0000000000000000 == V|1|) /\
        (0x0000000000000000 == V|1|) /\ (0x0000000000000001 <=u V|18|) /\
        (V|18| <=u 0x7fffffffffffffbe)
  PC 2: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x000000000000000f) /\
        (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x000000000000000f) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffc2)
  PC 3: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x000000000000000e) /\
        (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x000000000000000e) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffc6)
  PC 4: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x000000000000000d) /\
        (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x000000000000000d) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffca)
  PC 5: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x000000000000000c) /\
        (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x000000000000000c) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffce)
  PC 6: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x000000000000000b) /\
        (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x000000000000000b) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffd2)
  PC 7: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x000000000000000a) /\
        (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x000000000000000a) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffd6)
  PC 8: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x0000000000000009) /\
        (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x0000000000000009) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffda)
  PC 9: (extract[0-1](V|18|) == 0b00) /\ (0x0000000000000008 == V|1|) /\
        (0b00 == extract[0-1](V|19|)) /\ (0x0000000000000008 == V|1|) /\
        (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
        (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffde)
  PC 10: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x0000000000000007) /\
         (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x0000000000000007) /\
         (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
         (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffe2)
  PC 11: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x0000000000000006) /\
         (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x0000000000000006) /\
         (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
         (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffe6)
  PC 12: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x0000000000000005) /\
         (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x0000000000000005) /\
         (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
         (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffea)
  PC 13: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x0000000000000004) /\
         (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x0000000000000004) /\
         (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
         (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7fffffffffffffee)
  PC 14: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x0000000000000003) /\
         (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x0000000000000003) /\
         (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
         (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7ffffffffffffff2)
  PC 15: (extract[0-1](V|18|) == 0b00) /\ (V|1| == 0x0000000000000002) /\
         (0b00 == extract[0-1](V|19|)) /\ (V|1| == 0x0000000000000002) /\
         (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
         (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7ffffffffffffff6)
  PC 16: (extract[0-1](V|18|) == 0b00) /\ (0x0000000000000001 == V|1|) /\
         (0b00 == extract[0-1](V|19|)) /\ (0x0000000000000001 == V|1|) /\
         (0x0000000000000001 <=u V|18|) /\ (V|18| <=u 0x7fffffffffffffbe) /\
         (0x0000000000000001 <=u V|19|) /\ (V|19| <=u 0x7ffffffffffffffa)
  PC 17: (extract[0-1](V|18|) == 0b00) /\ (0x0000000000000010 == V|1|) /\
         (0x0000000000000010 == V|1|) /\ (0x0000000000000001 <=u V|18|) /\
         (V|18| <=u 0x7fffffffffffffbe)
  
Test our simple Kani demo works
  $ soteria-rust rustc demo.rs --clean --no-timing --kani
  Compiling... done in <time>
  error: saturating_add_overflow: found issues in <time>, errors in 1 branch (out of 3)
  error: Overflow in saturating_add_overflow
      ┌─ $TESTCASE_ROOT/demo.rs:11:9
    8 │  fn saturating_add_overflow() -> u32 {
      │   ----------------------------------- 1: Entry point
    9 │      let a: u32 = kani::any();
   10 │      let b: u32 = kani::any();
   11 │      if a + b < u32::MAX {
      │          ^^^^^ Triggering memory operation
  PC 1: (((0xffffffff -ck V|1|) <u V|2|) || ((0xffffffff -ck V|2|) <u V|1|))
  
  note: saturating_add: done in <time>, ran 2 branches
  PC 1: (V|1| <u (0xffffffff -ck V|2|)) /\ (V|1| <=u (0xffffffff -ck V|2|)) /\
        (V|2| <=u (0xffffffff -ck V|1|))
  PC 2: ((0xffffffff -ck V|2|) <=u V|1|)
  
  error: memory_leak: found issues in <time>, errors in 1 branch (out of 1)
  warning: Memory leak at ../alloc/src/alloc.rs:251:9-39 in memory_leak
      ┌─ $TESTCASE_ROOT/demo.rs:32:2
   32 │  fn memory_leak() {
      │   ^^^^^^^^^^^^^^^^
      │   │
      │   Leaking function
      │   1: Entry point
  PC 1: (extract[0-1](V|1|) == 0b00) /\ (0x0000000000000001 <=u V|1|) /\
        (V|1| <=u 0x7ffffffffffffffa)
  
  error: uninit_access: found issues in <time>, errors in 1 branch (out of 2)
  bug: Uninitialized memory access in uninit_access
      ┌─ $TESTCASE_ROOT/demo.rs:63:27
   58 │  fn uninit_access() {
      │   ------------------ 1: Entry point
   59 │      let any_option: MyOption<u32> = kani::any();
   60 │      let addr: *const u32 = &any_option as *const MyOption<u32> as *const u32;
   61 │      unsafe {
   62 │          let addr_value = addr.offset(1);
   63 │          let value: u32 = *addr_value;
      │                            ^^^^^^^^^^^ Triggering memory operation
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  [1]
