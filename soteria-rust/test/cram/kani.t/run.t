Test kani::any
  $ soteria-rust rustc any.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: any_bool: done in <time>, ran 2 branches
  PC 1: (V|1| <=u 0x01) /\ (V|1| != 0x00)
  PC 2: (V|1| <=u 0x01) /\ (V|1| == 0x00)
  
  note: any_i8: done in <time>, ran 3 branches
  PC 1: (V|1| == 0x00)
  PC 2: (V|1| != 0x00) /\ (extract[7-7](V|1|) == 0b1)
  PC 3: (V|1| != 0x00) /\ (extract[7-7](V|1|) == 0b0) /\ (0x00 <s V|1|)
  


Test kani::assume
  $ soteria-rust rustc assume.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: assume_bool: done in <time>, ran 1 branch
  PC 1: (V|1| <=u 0x01) /\ (V|1| != 0x00)
  
  note: assume_i32: done in <time>, ran 1 branch
  PC 1: (V|1| != 0x00000000)
  


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
  
  error: override_assert_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert!" in override_assert_macro
      ┌─ $RUSTERIA/std/src/lib.rs:23:10
   23 │          rusteria::assert(!!$cond, concat!(stringify!($($arg)+)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering memory operation
      │           2: Call trace
      ┌─ $TESTCASE_ROOT/assert.rs:14:2
   14 │  fn override_assert_macro() {
      │   -------------------------- 1: Entry point
  
  error: override_asserteq_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert_eq!" in override_asserteq_macro
      ┌─ $RUSTERIA/std/src/lib.rs:23:10
   23 │          rusteria::assert(!!$cond, concat!(stringify!($($arg)+)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering memory operation
      │           2: Call trace
      ┌─ $TESTCASE_ROOT/assert.rs:20:2
   20 │  fn override_asserteq_macro() {
      │   ---------------------------- 1: Entry point
  
  [1]

Test kani::slice::any_slice_of_array
  $ echo "Skipped; can't read symbolic slice" # soteria-rust rustc any_slice.rs --clean --no-timing --kani
  Skipped; can't read symbolic slice
