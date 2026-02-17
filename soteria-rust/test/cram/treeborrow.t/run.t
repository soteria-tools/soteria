Basic code, reference gets invalidated
  $ soteria-rust rustc raw-ptrs.rs
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Simple tree borrow violation
  $ soteria-rust rustc simple-fail.rs
  Compiling... done in <time>
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  bug: Aliasing error in main
      ┌─ $TESTCASE_ROOT/simple-fail.rs:8:5
    3 │  fn main() {
      │  --------- 1: Entry point
      ·  
    8 │      *y = 20; // UB: y is disabled
      │      ^^^^^^^ Memory store
  PC 1: empty
  
  [1]

Raw pointers don't get new tags
  $ soteria-rust rustc raw-ptrs.rs
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Raw pointers can access outside the parent's range, with offsets
  $ soteria-rust rustc offsets.rs
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Can have two mutable protected refs to the same allocation, if they don't overlap
  $ soteria-rust rustc two-mut-protected.rs
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

UnsafeCell allow foreign writes followed by local writes
  $ soteria-rust rustc cell.rs
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Nested UnsafeCells work too -- skipped for now, due to Charon changing the translation of IS_ZST
  $ soteria-rust rustc nested.rs
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Test --ignore-aliasing flag
  $ soteria-rust rustc simple-fail.rs --ignore-aliasing
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  
