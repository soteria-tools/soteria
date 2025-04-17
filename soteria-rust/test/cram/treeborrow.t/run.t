Basic code, reference gets invalidated
  $ soteria-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: 
    Distinct(V|0-3|) /\ (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\
    (V|0| != 0)

Simple tree borrow violation
  $ soteria-rust exec-main simple-fail.rs --clean
  soteria-rust: [ERROR] Error: Errors: [UBTreeBorrow with trace [($TESTCASE_ROOT/simple-fail.rs:8:4-11,
                                     Triggering memory operation)]]
  [1]

Raw pointers don't get new tags
  $ soteria-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: 
    Distinct(V|0-3|) /\ (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\
    (V|0| != 0)

Raw pointers can access outside the parent's range, with offsets
  $ soteria-rust exec-main offsets.rs --clean
  Done. - Ran 1 branches
  PC: 
    Distinct(V|0-7|) /\ (V|7| != 0) /\ (V|6| != 0) /\ (V|5| != 0) /\
    (V|4| != 0) /\ (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\ (V|0| != 0)
