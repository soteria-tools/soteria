Test running Soteria Rust on a crate with tests

A target's #[soteria::test] harnesses run by default, without --libtest.
  $ soteria-rust exec . --test soteria
  Compiling... done in <time>
  => Running soteria::tests::harness_ok...
  note: soteria::tests::harness_ok: done in <time>, ran 1 branch
  PC 1: (V|1| == 0x0000002a) /\ (V|1| == 0x0000002a)
  
  => Running soteria::tests::harness_nok...
  error: soteria::tests::harness_nok: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion in soteria::tests::harness_nok
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> tests/soteria.rs:19:9
   17 |      fn harness_nok() {
      |      ---------------- 1: Entry point
   18 |          let x: i32 = soteria::nondet_bytes();
   19 |          assert_eq!(x, get_answer());
      |          --------------------------- 2: Call trace
  PC 1: (V|1| != 0x0000002a)
  
  => Running soteria::tests::harness_nok_ok...
  note: soteria::tests::harness_nok_ok: done in <time>, ran 2 branches
  PC 1: (V|1| != 0x0000002a)
  
  [1]




With --libtest, the target's ordinary #[test] functions run alongside them.
  $ soteria-rust exec . --test soteria --libtest
  Compiling... done in <time>
  => Running soteria::tests::harness_ok...
  note: soteria::tests::harness_ok: done in <time>, ran 1 branch
  PC 1: (V|1| == 0x0000002a) /\ (V|1| == 0x0000002a)
  
  => Running soteria::tests::harness_nok...
  error: soteria::tests::harness_nok: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion in soteria::tests::harness_nok
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> tests/soteria.rs:19:9
   17 |      fn harness_nok() {
      |      ---------------- 1: Entry point
   18 |          let x: i32 = soteria::nondet_bytes();
   19 |          assert_eq!(x, get_answer());
      |          --------------------------- 2: Call trace
  PC 1: (V|1| != 0x0000002a)
  
  => Running soteria::tests::harness_nok_ok...
  note: soteria::tests::harness_nok_ok: done in <time>, ran 2 branches
  PC 1: (V|1| != 0x0000002a)
  
  => Running soteria::tests::unit_in_test_target...
  note: soteria::tests::unit_in_test_target: done in <time>, ran 1 branch
  PC 1: empty
  
  [1]





A target with only ordinary #[test] functions and no harnesses is empty by
default (nothing is discovered without --libtest).
  $ soteria-rust exec . --test tests
  Compiling... done in <time>
  error: Fatal: No entry points found
  [2]

Under --libtest those ordinary tests run.
  $ soteria-rust exec . --test tests --libtest
  Compiling... done in <time>
  => Running tests::test_ok...
  note: tests::test_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running tests::test_nok...
  error: tests::test_nok: found issues in <time>, errors in 1 branch (out of 1)
  error: Failed assertion in tests::test_nok
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> tests/tests.rs:10:5
    9 |  fn test_nok() {
      |  ------------- 1: Entry point
   10 |      assert_eq!(get_answer(), 67);
      |      ---------------------------- 2: Call trace
  PC 1: empty
  
  => Running tests::test_nok_expected...
  error: tests::test_nok_expected: found issues in <time>, errors in 1 branch (out of 1)
  error: Failed assertion in tests::test_nok_expected
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> tests/tests.rs:17:5
   16 |  fn test_nok_expected() {
      |  ---------------------- 1: Entry point
   17 |      assert_eq!(get_answer(), 67);
      |      ---------------------------- 2: Call trace
  PC 1: empty
  
  [1]




--lib analyses the library's unit tests in src/.
  $ soteria-rust exec . --lib --libtest --cargo="--features my_feature"
  Compiling... done in <time>
  => Running my_crate::test::test_in_src...
  note: my_crate::test::test_in_src: done in <time>, ran 1 branch
  PC 1: empty
  


--test and --lib are mutually exclusive.
  $ soteria-rust exec . --test tests --lib
  error: Fatal (Config): --test and --lib are mutually exclusive: use --lib for the library's unit tests in src/, or --test <name> for an integration test
  [124]
