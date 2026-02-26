  $ soteria-rust auto simple.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  error: Panic: aha! in half_manifest
      ┌─ $TESTCASE_ROOT/simple.rs:15:9
   11 │  fn half_manifest() -> i32 {
      │  ------------------------- 1: Entry point
      ·  
   15 │          panic!("aha!");
      │          ^^^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          2: Call trace
  
  error: Division by zero in manifest
      ┌─ $TESTCASE_ROOT/simple.rs:7:5
    5 │  fn manifest(x: i32) -> i32 {
      │  -------------------------- 1: Entry point
    6 │      let mut z = x;
    7 │      z /= z - x; // always a division by zero
      │      ^^^^^^^^^^ Triggering operation
  
  [1]

Test some bi-abduction with references.
  $ soteria-rust auto refs.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  error: Panic: tough luck in coinflip
      ┌─ $TESTCASE_ROOT/refs.rs:9:9
    1 │  fn coinflip(b: &bool) -> i32 {
      │  ---------------------------- 1: Entry point
      ·  
    9 │          panic!("tough luck");
      │          ^^^^^^^^^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          2: Call trace
  
  error: Panic: tough luck again! in coinflip_but_struct
      ┌─ $TESTCASE_ROOT/refs.rs:29:9
   19 │  fn coinflip_but_struct(f1: &Foo, f2: &Foo) -> i32 {
      │  ------------------------------------------------- 1: Entry point
      ·  
   29 │          panic!("tough luck again!");
      │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          2: Call trace
  
  [1]
