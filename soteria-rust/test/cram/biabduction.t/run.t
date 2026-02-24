  $ soteria-rust auto simple.rs
  Compiling... done in <time>
  
  error: found issues in <time>
  error: Division by zero in manifest
      ┌─ $TESTCASE_ROOT/simple.rs:7:5
    5 │  fn manifest(x: i32) -> i32 {
      │  -------------------------- 1: Entry point
    6 │      let mut z = x;
    7 │      z /= z - x; // always a division by zero
      │      ^^^^^^^^^^ Triggering operation
  
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
  
  [1]
