Test kani::any
  $ soteria-rust rustc any.rs --clean --kani
  note: Done, no errors found
  
  any::any_bool: ran 2 branches
  PC 1: (0 == V|1|) /\ (V|1| <= 1) /\ (0 <= V|1|)
  PC 2: (0 != V|1|) /\ (V|1| <= 1) /\ (0 <= V|1|)
  
  any::any_i8: ran 3 branches
  PC 1: (0 < V|1|) /\ (0 <= V|1|) /\ (0 != V|1|) /\ (V|1| <= 127) /\
        (-128 <= V|1|)
  PC 2: (V|1| < 0) /\ (0 != V|1|) /\ (V|1| <= 127) /\ (-128 <= V|1|)
  PC 3: (0 == V|1|) /\ (V|1| <= 127) /\ (-128 <= V|1|)

Test kani::assume
  $ soteria-rust rustc assume.rs --clean --kani
  note: Done, no errors found
  
  assume::assume_bool: ran 1 branch
  PC 1: (0 != V|1|) /\ (V|1| <= 1) /\ (0 <= V|1|)
  
  assume::assume_i32: ran 1 branch
  PC 1: (-0x80000000 <= (11 / V|1|)) /\ ((11 / V|1|) <= 0x7fffffff) /\
        (0 != V|1|) /\ (V|1| <= 0x7fffffff) /\ (-0x80000000 <= V|1|)

Test #[kani::should_panic]
  $ soteria-rust rustc should_panic.rs --clean --kani
  note: Done, no errors found
  
  should_panic::when_at_the_disco: ran 1 branch
  PC 1: empty

Test kani::assert
  $ soteria-rust rustc assert.rs --clean --kani
  error: Found issues
  
  assert::assert_false: error in 4 branches (out of 2):
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
  
  assert::fancy_assert_false: error in 4 branches (out of 2):
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
  
  assert::override_assert_macro: error in 4 branches (out of 2):
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
  
  assert::override_asserteq_macro: error in 4 branches (out of 2):
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
  $ soteria-rust rustc any_slice.rs --clean --kani
  note: Done, no errors found
  
  any_slice::main: ran 7 branches
  PC 1: ((V|2| - V|1|) <= 0) /\ ((V|2| - V|1|) <= 3) /\ (0 <= (V|2| - V|1|)) /\
        (((V|1| <= 0x1fffffffffffffff) && ((-0x2000000000000000 <= V|1|) && (V|1| <= 3))) || (0 == V|1|)) /\
        (V|1| <= V|2|) /\ (V|2| <= 3) /\ (V|2| <= 0xffffffffffffffff) /\
        (0 <= V|2|) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|)
  PC 2: ((V|2| - V|1|) <= 1) /\ (0 == V|1|) /\ (V|1| <= 0) /\
        !(((0 < V|1|) && (V|1| < 1))) /\ !(((0 == V|1|) && (2 == V|1|))) /\
        (V|1| <= 2) /\ (0 < (V|2| - V|1|)) /\ ((V|2| - V|1|) <= 3) /\
        (0 <= (V|2| - V|1|)) /\
        (((V|1| <= 0x1fffffffffffffff) && ((-0x2000000000000000 <= V|1|) && (V|1| <= 3))) || (0 == V|1|)) /\
        (V|1| <= V|2|) /\ (V|2| <= 3) /\ (V|2| <= 0xffffffffffffffff) /\
        (0 <= V|2|) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|)
  PC 3: ((V|2| - V|1|) <= 2) /\ (1 != V|1|) /\ (-1 < V|1|) /\
        !(((1 == V|1|) && (V|1| == -1))) /\ (-1 <= V|1|) /\ (V|1| <= 1) /\
        (-0x2000000000000001 <= V|1|) /\ (V|1| <= 0x1ffffffffffffffe) /\
        (1 < (V|2| - V|1|)) /\ (0 == V|1|) /\ (V|1| <= 0) /\
        !(((0 < V|1|) && (V|1| < 1))) /\ !(((0 == V|1|) && (2 == V|1|))) /\
        (V|1| <= 2) /\ (0 < (V|2| - V|1|)) /\ ((V|2| - V|1|) <= 3) /\
        (0 <= (V|2| - V|1|)) /\
        (((V|1| <= 0x1fffffffffffffff) && ((-0x2000000000000000 <= V|1|) && (V|1| <= 3))) || (0 == V|1|)) /\
        (V|1| <= V|2|) /\ (V|2| <= 3) /\ (V|2| <= 0xffffffffffffffff) /\
        (0 <= V|2|) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|)
  PC 4: (V|1| != -1) /\ (-2 < V|1|) /\ (V|1| != -2) /\ (-2 <= V|1|) /\
        (-0x2000000000000002 <= V|1|) /\ (V|1| <= 0x1ffffffffffffffd) /\
        (2 < (V|2| - V|1|)) /\ (1 != V|1|) /\ (-1 < V|1|) /\
        !(((1 == V|1|) && (V|1| == -1))) /\ (-1 <= V|1|) /\ (V|1| <= 1) /\
        (-0x2000000000000001 <= V|1|) /\ (V|1| <= 0x1ffffffffffffffe) /\
        (1 < (V|2| - V|1|)) /\ (0 == V|1|) /\ (V|1| <= 0) /\
        !(((0 < V|1|) && (V|1| < 1))) /\ !(((0 == V|1|) && (2 == V|1|))) /\
        (V|1| <= 2) /\ (0 < (V|2| - V|1|)) /\ ((V|2| - V|1|) <= 3) /\
        (0 <= (V|2| - V|1|)) /\
        (((V|1| <= 0x1fffffffffffffff) && ((-0x2000000000000000 <= V|1|) && (V|1| <= 3))) || (0 == V|1|)) /\
        (V|1| <= V|2|) /\ (V|2| <= 3) /\ (V|2| <= 0xffffffffffffffff) /\
        (0 <= V|2|) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|)
  PC 5: ((V|2| - V|1|) <= 1) /\ (1 == V|1|) /\ (V|1| <= 1) /\ (1 <= V|1|) /\
        !(((1 < V|1|) && (V|1| < 2))) /\ !(((2 == V|1|) && (1 == V|1|))) /\
        (0 < V|1|) /\ !(((0 < V|1|) && (V|1| < 1))) /\
        !(((0 == V|1|) && (2 == V|1|))) /\ (V|1| <= 2) /\
        (0 < (V|2| - V|1|)) /\ ((V|2| - V|1|) <= 3) /\ (0 <= (V|2| - V|1|)) /\
        (((V|1| <= 0x1fffffffffffffff) && ((-0x2000000000000000 <= V|1|) && (V|1| <= 3))) || (0 == V|1|)) /\
        (V|1| <= V|2|) /\ (V|2| <= 3) /\ (V|2| <= 0xffffffffffffffff) /\
        (0 <= V|2|) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|)
  PC 6: ((V|2| - V|1|) <= 2) /\ (0 != V|1|) /\ (-1 < V|1|) /\ (V|1| != -1) /\
        (-1 <= V|1|) /\ (-0x2000000000000001 <= V|1|) /\
        (V|1| <= 0x1ffffffffffffffe) /\ (1 < (V|2| - V|1|)) /\ (1 == V|1|) /\
        (V|1| <= 1) /\ (1 <= V|1|) /\ !(((1 < V|1|) && (V|1| < 2))) /\
        !(((2 == V|1|) && (1 == V|1|))) /\ (0 < V|1|) /\
        !(((0 < V|1|) && (V|1| < 1))) /\ !(((0 == V|1|) && (2 == V|1|))) /\
        (V|1| <= 2) /\ (0 < (V|2| - V|1|)) /\ ((V|2| - V|1|) <= 3) /\
        (0 <= (V|2| - V|1|)) /\
        (((V|1| <= 0x1fffffffffffffff) && ((-0x2000000000000000 <= V|1|) && (V|1| <= 3))) || (0 == V|1|)) /\
        (V|1| <= V|2|) /\ (V|2| <= 3) /\ (V|2| <= 0xffffffffffffffff) /\
        (0 <= V|2|) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|)
  PC 7: ((V|2| - V|1|) <= 1) /\ (2 == V|1|) /\
        !(((1 <= V|1|) && (V|1| <= 1))) /\ !(((1 < V|1|) && (V|1| < 2))) /\
        !(((2 == V|1|) && (1 == V|1|))) /\ (0 < V|1|) /\
        !(((0 < V|1|) && (V|1| < 1))) /\ !(((0 == V|1|) && (2 == V|1|))) /\
        (V|1| <= 2) /\ (0 < (V|2| - V|1|)) /\ ((V|2| - V|1|) <= 3) /\
        (0 <= (V|2| - V|1|)) /\
        (((V|1| <= 0x1fffffffffffffff) && ((-0x2000000000000000 <= V|1|) && (V|1| <= 3))) || (0 == V|1|)) /\
        (V|1| <= V|2|) /\ (V|2| <= 3) /\ (V|2| <= 0xffffffffffffffff) /\
        (0 <= V|2|) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|)
