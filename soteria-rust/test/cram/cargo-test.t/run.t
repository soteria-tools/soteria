Test running Soteria Rust on a crate with tests
  $ soteria-rust exec . --test tests
  Compiling... done in <time>
  => Running test_ok...
  note: test_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running test_nok...
  error: test_nok: found issues in <time>, errors in 1 branch (out of 1)
  error: Failed assertion: assertion failed: get_answer() == 67 in test_nok
      ┌─ tests/tests.rs:10:5
    9 │  fn test_nok() {
      │  ------------- 1: Entry point
   10 │      assert_eq!(get_answer(), 67);
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │      │
      │      Triggering operation
      │      2: Call trace
  PC 1: empty
  
  => Running test_nok_expected...
  error: test_nok_expected: found issues in <time>, errors in 1 branch (out of 1)
  error: Failed assertion: assertion failed: get_answer() == 67 in test_nok_expected
      ┌─ tests/tests.rs:17:5
   16 │  fn test_nok_expected() {
      │  ---------------------- 1: Entry point
   17 │      assert_eq!(get_answer(), 67);
      │      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │      │
      │      Triggering operation
      │      2: Call trace
  PC 1: empty
  
  [1]
  $ soteria-rust exec . --test soteria
  Compiling... done in <time>
  => Running tests::test_ok...
  note: tests::test_ok: done in <time>, ran 1 branch
  PC 1: (V|1| == 0x0000002a) /\ (V|1| == 0x0000002a)
  
  => Running tests::test_nok...
  error: tests::test_nok: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: assertion failed: x == get_answer() in tests::test_nok
      ┌─ tests/soteria.rs:15:9
   13 │      fn test_nok() {
      │      ------------- 1: Entry point
   14 │          let x: i32 = soteria::nondet_bytes();
   15 │          assert_eq!(x, get_answer());
      │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          2: Call trace
  PC 1: (V|1| != 0x0000002a)
  
  => Running tests::test_nok_ok...
  note: tests::test_nok_ok: done in <time>, ran 2 branches
  PC 1: (V|1| != 0x0000002a)
  
  [1]
