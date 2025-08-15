Test kani::any
  $ soteria-rust rustc any.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: any::any_bool: done in <time>, ran 2 branches
  PC 1: (0 == V|1|) /\ (0 == V|1|)
  PC 2: (1 == V|1|) /\ (1 == V|1|)
  
  note: any::any_i8: done in <time>, ran 3 branches
  PC 1: (0 != V|1|) /\ (1 <= V|1|) /\ (V|1| <= 127)
  PC 2: (0 != V|1|) /\ (-128 <= V|1|) /\ (V|1| <= -1)
  PC 3: (0 == V|1|) /\ (0 == V|1|)
  


Test kani::assume
  $ soteria-rust rustc assume.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: assume::assume_bool: done in <time>, ran 1 branch
  PC 1: (1 == V|1|) /\ (1 == V|1|)
  
  note: assume::assume_i32: done in <time>, ran 1 branch
  PC 1: (0 != V|1|) /\ ((11 / V|1|) <= 0x7fffffff) /\
        (-0x80000000 <= (11 / V|1|)) /\ (-0x80000000 <= V|1|) /\
        (V|1| <= 0x7fffffff)
  


Test #[kani::should_panic]
  $ soteria-rust rustc should_panic.rs --clean --no-timing --kani
  Compiling... done in <time>
  note: should_panic::when_at_the_disco: done in <time>, ran 1 branch
  PC 1: empty
  


Test kani::assert
  $ soteria-rust rustc assert.rs --clean --no-timing --kani
  Compiling... done in <time>
  error: assert::assert_false: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: Expected true! in assert::assert_false
      ┌─ $TESTCASE_ROOT/assert.rs:4:5
    1 │    #[kani::proof]
    2 │ ╭  fn assert_false() {
    3 │ │      let b: bool = kani::any();
    4 │ │      kani::assert(b, "Expected true!");
      │ │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │ │      │
      │ │      Triggering memory operation
      │ │      2: Call trace
    5 │ │  }
      │ ╰──' 1: Entry point
    6 │    
  
  error: assert::fancy_assert_false: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: 👻 unicode is 𝒮𝒞𝒜ℛ𝒴 in assert::fancy_assert_false
      ┌─ $TESTCASE_ROOT/assert.rs:10:5
    7 │    #[kani::proof]
    8 │ ╭  fn fancy_assert_false() {
    9 │ │      let b: bool = kani::any();
   10 │ │      kani::assert(b, "👻 unicode is 𝒮𝒞𝒜ℛ𝒴");
      │ │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │ │      │
      │ │      Triggering memory operation
      │ │      2: Call trace
   11 │ │  }
      │ ╰──' 1: Entry point
   12 │    
  
  error: assert::override_assert_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert!" in assert::override_assert_macro
      ┌─ $RUSTERIA/std/src/lib.rs:23:9
   23 │            rusteria::assert(!!$cond, concat!(stringify!($($arg)+)));
      │            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │            │
      │            Triggering memory operation
      │            2: Call trace
      ┌─ $TESTCASE_ROOT/assert.rs:14:1
   13 │    #[kani::proof]
   14 │ ╭  fn override_assert_macro() {
   15 │ │      let b: bool = kani::any();
   16 │ │      assert!(b, "I used \"assert!\"");
   17 │ │  }
      │ ╰──' 1: Entry point
   18 │    
  
  error: assert::override_asserteq_macro: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: I used "assert_eq!" in assert::override_asserteq_macro
      ┌─ $RUSTERIA/std/src/lib.rs:23:9
   23 │            rusteria::assert(!!$cond, concat!(stringify!($($arg)+)));
      │            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │            │
      │            Triggering memory operation
      │            2: Call trace
      ┌─ $TESTCASE_ROOT/assert.rs:20:1
   19 │    #[kani::proof]
   20 │ ╭  fn override_asserteq_macro() {
   21 │ │      let a: u32 = kani::any();
   22 │ │      let b: u32 = kani::any();
   23 │ │      assert_eq!(a, b, "I used \"assert_eq!\"");
   24 │ │  }
      │ ╰──' 1: Entry point
   25 │    
  
  [1]

Test kani::slice::any_slice_of_array
  $ echo "Skipped; can't read symbolic slice" # soteria-rust rustc any_slice.rs --clean --no-timing --kani
  Skipped; can't read symbolic slice
