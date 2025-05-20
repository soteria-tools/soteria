Basic code, reference gets invalidated
  $ soteria-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: empty

Simple tree borrow violation
  $ soteria-rust exec-main simple-fail.rs --clean
  Error in 1 branch:
  - UBTreeBorrow
    Trace:
    • Call trace: ../cram/treeborrow.t/simple-fail.rs:3:0-10:1
    • Triggering memory operation: ../cram/treeborrow.t/simple-fail.rs:8:4-11
  [1]

Raw pointers don't get new tags
  $ soteria-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: empty

Raw pointers can access outside the parent's range, with offsets
  $ soteria-rust exec-main offsets.rs --clean
  Done. - Ran 1 branches
  PC: empty

Can have two mutable protected refs to the same allocation, if they don't overlap
  $ soteria-rust exec-main two-mut-protected.rs --clean
  Error in 1 branch:
  - UBTreeBorrow
    Trace:
    • Call trace: ../cram/treeborrow.t/two-mut-protected.rs:6:0-16:1
    • Call trace: ../cram/treeborrow.t/two-mut-protected.rs:15:4-42
    • Triggering memory operation: ../cram/treeborrow.t/two-mut-protected.rs:11:8-14
  [1]

UnsafeCell allow foreign writes followed by local writes
  $ soteria-rust exec-main cell.rs --clean
  Done. - Ran 1 branches
  PC: empty
