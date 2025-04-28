Basic code, reference gets invalidated
  $ soteria-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: empty

Simple tree borrow violation
  $ soteria-rust exec-main simple-fail.rs --clean
  Error in 1 branch:
  - UBTreeBorrow with trace [($TESTCASE_ROOT/simple-fail.rs:8:4-11,
                                                 Triggering memory operation)]
  [1]

Raw pointers don't get new tags
  $ soteria-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: empty

Raw pointers can access outside the parent's range, with offsets
  $ soteria-rust exec-main offsets.rs --clean
  Done. - Ran 1 branches
  PC: empty
