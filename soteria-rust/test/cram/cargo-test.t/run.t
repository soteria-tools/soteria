Test running Soteria Rust on a crate with tests
  $ soteria-rust exec . --test tests
  Compiling test "tests"... done in <time>
  => Running tests::test_ok in test "tests"...
  note: tests::test_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running tests::test_nok in test "tests"...
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
  
  => Running tests::test_nok_expected in test "tests"...
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
  $ soteria-rust exec . --test soteria
  Compiling test "soteria"... done in <time>
  => Running soteria::tests::test_ok in test "soteria"...
  note: soteria::tests::test_ok: done in <time>, ran 1 branch
  PC 1: (V|1| == 0x0000002a) /\ (V|1| == 0x0000002a)
  
  => Running soteria::tests::test_nok in test "soteria"...
  error: soteria::tests::test_nok: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion in soteria::tests::test_nok
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> tests/soteria.rs:15:9
   13 |      fn test_nok() {
      |      ------------- 1: Entry point
   14 |          let x: i32 = soteria::nondet_bytes();
   15 |          assert_eq!(x, get_answer());
      |          --------------------------- 2: Call trace
  PC 1: (V|1| != 0x0000002a)
  
  => Running soteria::tests::test_nok_ok in test "soteria"...
  note: soteria::tests::test_nok_ok: done in <time>, ran 2 branches
  PC 1: (V|1| != 0x0000002a)
  
  [1]

  $ soteria-rust exec . --test lib --cargo="--features my_feature"
  Compiling lib tests... done in <time>
  => Running my_crate::test::test_in_src in lib tests...
  note: my_crate::test::test_in_src: done in <time>, ran 1 branch
  PC 1: empty
  
Test running a single example target; like integration tests, the example's
[#[test]] functions are used as entry points.
  $ soteria-rust exec . --example example
  Compiling example "example"... done in <time>
  => Running example::example_ok in example "example"...
  note: example::example_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running example::example_nok in example "example"...
  error: example::example_nok: found issues in <time>, errors in 1 branch (out of 1)
  error: Failed assertion in example::example_nok
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> examples/example.rs:14:5
   13 |  fn example_nok() {
      |  ---------------- 1: Entry point
   14 |      assert_eq!(get_answer(), 67);
      |      ---------------------------- 2: Call trace
  PC 1: empty
  
  [1]

Test running every target at once with --all-targets; targets without entry
points (here the lib, whose unit test is behind a feature) are skipped.
  $ soteria-rust exec . --all-targets
  Compiling lib tests... done in <time>
  No entry points found in lib tests, skipping
  Compiling example "example"... done in <time>
  => Running example::example_ok in example "example"...
  note: example::example_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running example::example_nok in example "example"...
  error: example::example_nok: found issues in <time>, errors in 1 branch (out of 1)
  error: Failed assertion in example::example_nok
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> examples/example.rs:14:5
   13 |  fn example_nok() {
      |  ---------------- 1: Entry point
   14 |      assert_eq!(get_answer(), 67);
      |      ---------------------------- 2: Call trace
  PC 1: empty
  
  Compiling test "soteria"... done in <time>
  => Running soteria::tests::test_ok in test "soteria"...
  note: soteria::tests::test_ok: done in <time>, ran 1 branch
  PC 1: (0x0000002a == V|1|) /\ (0x0000002a == V|1|)
  
  => Running soteria::tests::test_nok in test "soteria"...
  error: soteria::tests::test_nok: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion in soteria::tests::test_nok
      --> $RUSTLIB/library/core/src/panicking.rs:394:5
  394 |      assert_failed_inner(kind, &left, &right, args)
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> tests/soteria.rs:15:9
   13 |      fn test_nok() {
      |      ------------- 1: Entry point
   14 |          let x: i32 = soteria::nondet_bytes();
   15 |          assert_eq!(x, get_answer());
      |          --------------------------- 2: Call trace
  PC 1: (0x0000002a != V|1|)
  
  => Running soteria::tests::test_nok_ok in test "soteria"...
  note: soteria::tests::test_nok_ok: done in <time>, ran 2 branches
  PC 1: (0x0000002a != V|1|)
  
  Compiling test "tests"... done in <time>
  => Running tests::test_ok in test "tests"...
  note: tests::test_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running tests::test_nok in test "tests"...
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
  
  => Running tests::test_nok_expected in test "tests"...
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

--test and --example can be repeated to compile and analyse several targets, one
after the other (shown with --list-tests for brevity).
  $ soteria-rust compile . --test tests --test soteria --list-tests 2>/dev/null
  [{"test":"tests::test_ok","target":{"kind":"test","name":"tests"}},{"test":"tests::test_nok","target":{"kind":"test","name":"tests"}},{"test":"tests::test_nok_expected","target":{"kind":"test","name":"tests"}},{"test":"soteria::tests::test_ok","target":{"kind":"test","name":"soteria"}},{"test":"soteria::tests::test_nok","target":{"kind":"test","name":"soteria"}},{"test":"soteria::tests::test_nok_ok","target":{"kind":"test","name":"soteria"}}]

--tests selects every test target; --example can be added on top to also analyse
an example (shown with --list-tests for brevity; the lib has no tests here).
  $ soteria-rust compile . --tests --example example --list-tests 2>/dev/null
  [{"test":"soteria::tests::test_ok","target":{"kind":"test","name":"soteria"}},{"test":"soteria::tests::test_nok","target":{"kind":"test","name":"soteria"}},{"test":"soteria::tests::test_nok_ok","target":{"kind":"test","name":"soteria"}},{"test":"tests::test_ok","target":{"kind":"test","name":"tests"}},{"test":"tests::test_nok","target":{"kind":"test","name":"tests"}},{"test":"tests::test_nok_expected","target":{"kind":"test","name":"tests"}},{"test":"example::example_ok","target":{"kind":"example","name":"example"}},{"test":"example::example_nok","target":{"kind":"example","name":"example"}}]

--all-targets cannot be combined with any target, and --tests is redundant with
--test.
  $ soteria-rust exec . --all-targets --test tests
  error: Fatal (Config): --all-targets cannot be combined with --test, --example or --tests
  [124]
  $ soteria-rust exec . --tests --test tests
  error: Fatal (Config): --tests cannot be combined with --test
  [124]
