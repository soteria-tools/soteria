Ensure we fail when in polymorphic mode with Obol
  $ soteria-rust rustc trivial.rs --frontend obol --poly
  error: Fatal (Config): Obol does not support polymorphic analyses; use --frontend charon
  [124]

Simple polymorphic function with no polymorphic values
  $ soteria-rust rustc trivial.rs --frontend charon --poly
  Compiling... done in <time>
  note: trivial::trivial: done in <time>, ran 2 branches
  PC 1: (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000000 != ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=s ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) ? (0x0000000000000000 == V|1|) : (0x0000000000000008 == ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (V|1| <=u 0x00000000000003ff)
  PC 2: (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000000 != ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x0000000000000008) && (0b000 != extract[0-2](V|1|))) || ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=s ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=u ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        !(((0b000 == extract[0-2](V|1|)) ? (0x0000000000000000 == V|1|) : (0x0000000000000008 == ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((0b000 == extract[0-2](V|1|)) || (0x0000000000000008 <=s ((0x0000000000000010 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        (V|1| <=u 0x00000000000003ff)
  
Ensure generic args are passed through function calls and not lost
  $ soteria-rust rustc subst_generics.rs --frontend charon --poly
  Compiling... done in <time>
  note: subst_generics::wrap_stuff: done in <time>, ran 1 branch
  PC 1: empty
  
Try creating a generic vec
  $ soteria-rust rustc vec.rs --frontend charon --poly
  Compiling... done in <time>
  note: vec::with_vec: done in <time>, ran 2 branches
  PC 1: (0x0000000000000000 == V|1|) /\ (V|2| <=u 0x00000000000003ff) /\
        (0x0000000000000000 == V|1|)
  PC 2: (V|3| <u (0x7fffffffffffffff - (0x000000000000000a *ck V|1|))) /\
        (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x00000000000003ff) /\
        (V|2| <=u 0x00000000000003ff) /\ (0x0000000000000001 <=u V|3|)
  
  error: vec::with_vec_wrong: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: assertion failed: my_vec.capacity() == 10 in vec::with_vec_wrong
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           2: Call trace
      ┌─ $TESTCASE_ROOT/vec.rs:14:1
   13 │    #[rusteria::test]
   14 │ ╭  fn with_vec_wrong<T>() {
   15 │ │      let my_vec: Vec<T> = Vec::with_capacity(10);
   16 │ │      assert!(my_vec.capacity() == 10);
      │ ╰───────────────────────────────────────' 1: Entry point
   17 │    }
  PC 1: (0x0000000000000000 == V|1|) /\ (V|2| <=u 0x00000000000003ff) /\
        (0x0000000000000000 == V|1|)
  
  [1]

Try generics when moving them between argumen ts
  $ soteria-rust rustc moving_generics.rs --frontend charon --poly
  Compiling... done in <time>
  error: moving_generics::two_generics: found issues in <time>, errors in 8 branches (out of 11)
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:16:4
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
      │ │     ----------------------------------- 2: Call trace
   17 │ │      check_size::<U, _>(|s| s == size_u);
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
   21 │ │          check_size::<U, _>(|s| s > size_t);
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
   24 │ │          check_size::<U, _>(|s| s < size_t);
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
   27 │ │          check_size::<U, _>(|s| s == size_t);
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|1| != V|3|) /\ (V|1| <=u 0x00000000000003ff) /\
        (V|2| <=u 0x00000000000003ff) /\ (V|3| <=u 0x00000000000003ff)
  
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:17:4
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
   17 │ │      check_size::<U, _>(|s| s == size_u);
      │ │     ----------------------------------- 2: Call trace
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
   21 │ │          check_size::<U, _>(|s| s > size_t);
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
   24 │ │          check_size::<U, _>(|s| s < size_t);
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
   27 │ │          check_size::<U, _>(|s| s == size_t);
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|2| != V|4|) /\ (V|1| <=u 0x00000000000003ff) /\
        (V|2| <=u 0x00000000000003ff) /\ (V|3| <=u 0x00000000000003ff) /\
        (V|4| <=u 0x00000000000003ff) /\ (V|1| == V|3|)
  
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:20:8
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
   17 │ │      check_size::<U, _>(|s| s == size_u);
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
      │ │         ---------------------------------- 2: Call trace
   21 │ │          check_size::<U, _>(|s| s > size_t);
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
   24 │ │          check_size::<U, _>(|s| s < size_t);
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
   27 │ │          check_size::<U, _>(|s| s == size_t);
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|1| <u V|2|) /\ (V|2| <=u V|5|) /\ (V|1| <=u 0x00000000000003ff) /\
        (V|2| <=u 0x00000000000003ff) /\ (V|3| <=u 0x00000000000003ff) /\
        (V|4| <=u 0x00000000000003ff) /\ (V|5| <=u 0x00000000000003ff) /\
        (V|1| == V|3|) /\ (V|2| == V|4|)
  
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:21:8
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
   17 │ │      check_size::<U, _>(|s| s == size_u);
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
   21 │ │          check_size::<U, _>(|s| s > size_t);
      │ │         ---------------------------------- 2: Call trace
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
   24 │ │          check_size::<U, _>(|s| s < size_t);
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
   27 │ │          check_size::<U, _>(|s| s == size_t);
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|1| <u V|2|) /\ (V|5| <u V|2|) /\ (V|6| <=u V|1|) /\
        (V|1| <=u 0x00000000000003ff) /\ (V|2| <=u 0x00000000000003ff) /\
        (V|3| <=u 0x00000000000003ff) /\ (V|4| <=u 0x00000000000003ff) /\
        (V|5| <=u 0x00000000000003ff) /\ (V|6| <=u 0x00000000000003ff) /\
        (V|1| == V|3|) /\ (V|2| == V|4|)
  
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:23:8
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
   17 │ │      check_size::<U, _>(|s| s == size_u);
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
   21 │ │          check_size::<U, _>(|s| s > size_t);
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
      │ │         ---------------------------------- 2: Call trace
   24 │ │          check_size::<U, _>(|s| s < size_t);
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
   27 │ │          check_size::<U, _>(|s| s == size_t);
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|2| <=u V|1|) /\ (V|2| <u V|1|) /\ (V|5| <=u V|2|) /\
        (V|1| <=u 0x00000000000003ff) /\ (V|2| <=u 0x00000000000003ff) /\
        (V|3| <=u 0x00000000000003ff) /\ (V|4| <=u 0x00000000000003ff) /\
        (V|5| <=u 0x00000000000003ff) /\ (V|1| == V|3|) /\ (V|2| == V|4|)
  
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:24:8
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
   17 │ │      check_size::<U, _>(|s| s == size_u);
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
   21 │ │          check_size::<U, _>(|s| s > size_t);
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
   24 │ │          check_size::<U, _>(|s| s < size_t);
      │ │         ---------------------------------- 2: Call trace
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
   27 │ │          check_size::<U, _>(|s| s == size_t);
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|2| <=u V|1|) /\ (V|2| <u V|1|) /\ (V|2| <u V|5|) /\
        (V|1| <=u V|6|) /\ (V|1| <=u 0x00000000000003ff) /\
        (V|2| <=u 0x00000000000003ff) /\ (V|3| <=u 0x00000000000003ff) /\
        (V|4| <=u 0x00000000000003ff) /\ (V|5| <=u 0x00000000000003ff) /\
        (V|6| <=u 0x00000000000003ff) /\ (V|1| == V|3|) /\ (V|2| == V|4|)
  
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:26:8
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
   17 │ │      check_size::<U, _>(|s| s == size_u);
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
   21 │ │          check_size::<U, _>(|s| s > size_t);
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
   24 │ │          check_size::<U, _>(|s| s < size_t);
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
      │ │         ----------------------------------- 2: Call trace
   27 │ │          check_size::<U, _>(|s| s == size_t);
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|2| <=u V|1|) /\ (V|1| <=u V|2|) /\ (V|2| != V|5|) /\
        (V|1| <=u 0x00000000000003ff) /\ (V|2| <=u 0x00000000000003ff) /\
        (V|3| <=u 0x00000000000003ff) /\ (V|4| <=u 0x00000000000003ff) /\
        (V|5| <=u 0x00000000000003ff) /\ (V|1| == V|3|) /\ (V|2| == V|4|)
  
  error: Failed assertion: assertion failed: predicate(size) in moving_generics::two_generics
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:8
   20 │            rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering operation
      │           3: Call trace
      ┌─ $TESTCASE_ROOT/moving_generics.rs:27:8
   11 │    #[rusteria::test]
   12 │ ╭  fn two_generics<T, U>() {
   13 │ │      let size_t = std::intrinsics::size_of::<T>();
   14 │ │      let size_u = std::intrinsics::size_of::<U>();
   15 │ │  
   16 │ │      check_size::<T, _>(|s| s == size_t);
   17 │ │      check_size::<U, _>(|s| s == size_u);
   18 │ │  
   19 │ │      if size_t < size_u {
   20 │ │          check_size::<T, _>(|s| s < size_u);
   21 │ │          check_size::<U, _>(|s| s > size_t);
   22 │ │      } else if size_t > size_u {
   23 │ │          check_size::<T, _>(|s| s > size_u);
   24 │ │          check_size::<U, _>(|s| s < size_t);
   25 │ │      } else {
   26 │ │          check_size::<T, _>(|s| s == size_u);
   27 │ │          check_size::<U, _>(|s| s == size_t);
      │ │         ----------------------------------- 2: Call trace
   28 │ │      }
      │ ╰───────' 1: Entry point
   29 │    }
  PC 1: (V|2| <=u V|1|) /\ (V|1| <=u V|2|) /\ (V|1| != V|6|) /\
        (V|1| <=u 0x00000000000003ff) /\ (V|2| <=u 0x00000000000003ff) /\
        (V|3| <=u 0x00000000000003ff) /\ (V|4| <=u 0x00000000000003ff) /\
        (V|5| <=u 0x00000000000003ff) /\ (V|6| <=u 0x00000000000003ff) /\
        (V|1| == V|3|) /\ (V|2| == V|4|) /\ (V|2| == V|5|)
  
  [1]
Try const generics
  $ soteria-rust rustc const_generics.rs --frontend charon --poly
  Compiling... done in <time>
  note: const_generics::test_concrete_const_generic: done in <time>, ran 1 branch
  PC 1: empty
  
  note: const_generics::test_concrete_const_generic_vanish: done in <time>, ran 0 branches
  
  
  note: const_generics::test_poly_const_generic: done in <time>, ran 1 branch
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  
  note: const_generics::test_poly_const_generic2: done in <time>, ran 6 branches
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  PC 2: (0x0000000000000001 == V|1|) /\ (0x0000000000000001 == V|1|)
  PC 3: (V|1| == 0x0000000000000008) /\ (V|1| == 0x0000000000000008)
  PC 4: (V|1| == 0x000000000000000f) /\ (V|1| == 0x000000000000000f)
  PC 5: (V|1| == 0x00000000000000ff) /\ (V|1| == 0x00000000000000ff)
  PC 6: (0x0000000000000002 <=u V|1|) /\ (V|1| != 0x0000000000000008) /\
        (V|1| != 0x000000000000000f) /\ (V|1| != 0x00000000000000ff)
  
Test generating nondeterministic values of type T
  $ soteria-rust rustc nondet_t.rs --frontend charon --poly
  Compiling... done in <time>
  note: nondet_t::nondet_t: done in <time>, ran 2 branches
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  PC 2: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x00000000000003ff)
  
Test linked lists, to ensure dropping the list works despite the generic type
  $ soteria-rust rustc linked_list.rs --frontend charon --poly
  Compiling... done in <time>
  note: linked_list::test_linked_list: done in <time>, ran 3 branches
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000008 <=u V|2|) /\
        (V|2| <=u 0x7fffffffffffffee) /\ (0x0000000000000000 == V|1|) /\
        (0b000 == extract[0-2](V|2|))
  PC 2: ((extract[0-2](V|1|) == 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <=s 0x7fffffffffffffef)) /\
        !((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) == 0xfffffffffffffff0)) : (0x0000000000000000 == (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))))) /\
        !((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|)) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s V|1|))) /\
        (V|1| != (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))))) /\
        !((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000008 +ck V|1|) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : ((extract[0-2](V|1|) == 0b000) && ((0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|))) : ((extract[0-2](V|1|) == 0b000) ? ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s (0x0000000000000008 +ck V|1|)) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) /\
        ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 <=s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        (((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) != ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000008 +ck V|1|) : (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) || ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 != ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) == 0b000) || (V|1| <=s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) || ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <=s V|1|))) /\
        ((extract[0-2](V|1|) == 0b000) || (V|1| <s (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) != ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000008 +ck V|1|) : (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) || (V|1| != ((extract[0-2](V|1|) == 0b000) ? V|1| : ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) != ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000008 +ck V|1|) : (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x7fffffffffffffef)) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <u 0x7fffffffffffffff)) /\
        (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (V|2| <u (0x7fffffffffffffef - V|1|)) : (V|2| <u (0x7fffffffffffffef - ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) : (V|2| <u (0x7fffffffffffffff - (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))))) /\
        (!((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck V|1|) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : ((extract[0-2](V|1|) == 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|))) : ((extract[0-2](V|1|) == 0b000) ? ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s V|1|) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) || ((extract[0-2](V|1|) != 0b000) && (0x0000000000000000 == ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) != (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))))) || ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 != ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) == 0b000) || (V|1| <=s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) || ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <=s V|1|))) /\
        ((extract[0-2](V|1|) == 0b000) || (V|1| <s (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 != ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        !(((extract[0-2](V|1|) == 0b000) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck V|1|) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : ((extract[0-2](V|1|) == 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|)))) /\
        ((extract[0-2](V|1|) == 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) != 0xfffffffffffffff0)) /\
        (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x00000000000003ff) /\
        (0x0000000000000008 <=u V|2|) /\
        (V|1| == ((extract[0-2](V|1|) == 0b000) ? V|1| : ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) == (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))))) /\
        (0b000 == extract[0-2](V|2|))
  PC 3: ((extract[0-2](V|1|) == 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <=s 0x7fffffffffffffef)) /\
        !((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) == 0xfffffffffffffff0)) : (0x0000000000000000 == (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))))) /\
        !((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|)) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s V|1|))) /\
        (V|1| != (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))))) /\
        !((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000008 +ck V|1|) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : ((extract[0-2](V|1|) == 0b000) && ((0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|))) : ((extract[0-2](V|1|) == 0b000) ? ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s (0x0000000000000008 +ck V|1|)) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) /\
        ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 <=s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        (((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) != ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000008 +ck V|1|) : (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) || ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 != ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) == 0b000) || (V|1| <=s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) || ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <=s V|1|))) /\
        ((extract[0-2](V|1|) == 0b000) || (V|1| <s (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) != ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000008 +ck V|1|) : (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) || (V|1| != ((extract[0-2](V|1|) == 0b000) ? V|1| : ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (V|1| != ((extract[0-2](V|1|) == 0b000) ? V|1| : ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        ((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))) != ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000008 +ck V|1|) : (0x0000000000000008 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <u 0x7fffffffffffffef)) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <u 0x7fffffffffffffff)) /\
        (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (V|2| <u (0x7fffffffffffffef - V|1|)) : (V|2| <u (0x7fffffffffffffef - ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) : (V|2| <u (0x7fffffffffffffff - (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (!((((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck V|1|) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : ((extract[0-2](V|1|) == 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|))) : ((extract[0-2](V|1|) == 0b000) ? ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s V|1|) : ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))) || ((extract[0-2](V|1|) != 0b000) && (0x0000000000000000 == ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        ((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        ((((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) != (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))))) || ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 != ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) == 0b000) || (V|1| <=s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) || ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <=s V|1|))) /\
        ((extract[0-2](V|1|) == 0b000) || (V|1| <s (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000010 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        ((extract[0-2](V|1|) == 0b000) || (0x0000000000000000 != ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000010 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000010 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        !(((extract[0-2](V|1|) == 0b000) ? ((extract[0-2](V|1|) != 0b000) && ((0x0000000000000010 +ck V|1|) <s ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : ((extract[0-2](V|1|) == 0b000) && ((0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))) <s V|1|)))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        (((extract[0-2](V|1|) != 0b000) && (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s V|1|)) || ((extract[0-2](V|1|) == 0b000) || ((extract[0-2](V|1|) != 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) <s (0x0000000000000008 +ck V|1|))))) /\
        ((extract[0-2](V|1|) == 0b000) || (((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))) != 0xfffffffffffffff0)) /\
        (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x00000000000003ff) /\
        (0x0000000000000008 <=u V|2|) /\
        (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) == (((extract[0-2](V|1|) == 0b000) ? (extract[0-2](V|1|) == 0b000) : (0b000 == extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|)))))) ? ((extract[0-2](V|1|) == 0b000) ? (0x0000000000000010 +ck V|1|) : (0x0000000000000010 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) : (((extract[0-2](V|1|) == 0b000) ? (0x0000000000000018 +ck V|1|) : (0x0000000000000018 +ck ((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))) -ck ((extract[0-2](V|1|) == 0b000) ? extend[u61](extract[0-2](V|1|)) : extend[u61](extract[0-2](((0x0000000000000008 +ck V|1|) -ck extend[u61](extract[0-2](V|1|))))))))) /\
        (0b000 == extract[0-2](V|2|))
  
