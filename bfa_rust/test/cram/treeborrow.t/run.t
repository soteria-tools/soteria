Basic code, reference gets invalidated
  $ bfa-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: 
    Distinct(V|0-3|) /\ (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\
    (V|0| != 0)

Simple tree borrow violation
  $ bfa-rust exec-main simple-fail.rs --clean
  bfa-rust: [ERROR] Error: Errors: [UBTreeBorrow with trace [($TESTCASE_ROOT/simple-fail.rs:8:4-11,
                                     Triggering memory operation)]]
  [1]

Raw pointers don't get new tags
  $ bfa-rust exec-main raw-ptrs.rs --clean
  Done. - Ran 1 branches
  PC: 
    Distinct(V|0-3|) /\ (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\
    (V|0| != 0)

Raw pointers can access outside the parent's range, with offsets
  $ bfa-rust exec-main offsets.rs --clean
  warning: Unsupported binary operation: offset
   --> /rustc/library/core/src/ptr/mut_ptr.rs:1030:18
  
  note: the error occurred when translating `core::ptr::mut_ptr::{*mut T}::add`, which is (transitively) used at the following location(s):
   --> $TESTCASE_ROOT/offsets.rs:7:17
    |
  7 |         let y = (x as *mut u8).add(1);
    |                 ---------------------
    |
  warning: The extraction generated 1 warnings
  Done. - Ran 1 branches
  PC: 
    Distinct(V|0-7|) /\ (V|7| != 0) /\ (V|6| != 0) /\ (V|5| != 0) /\
    (V|4| != 0) /\ (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\ (V|0| != 0)
