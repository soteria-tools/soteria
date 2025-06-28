Basic code, reference gets invalidated
  $ soteria-rust exec-main raw-ptrs.rs --clean
  note: Done, no errors found
  
  raw_ptrs::main: ran 1 branch
  PC 1: true

Simple tree borrow violation
  $ soteria-rust exec-main simple-fail.rs --clean
  error: Found issues
  
  simple_fail::main: error in 1 branch (out of 1):
  bug: Aliasing error in simple_fail::main
      ┌─ $TESTCASE_ROOT/simple-fail.rs:8:5
    2 │    // https://perso.crans.org/vanille/treebor/aux/preprint.pdf
    3 │ ╭  fn main() {
    4 │ │      let mut root = 42;
    5 │ │      let ptr = &mut root as *mut i32;
    6 │ │      let (x, y) = unsafe { (&mut *ptr, &mut *ptr) };
    7 │ │      *x = 13;
    8 │ │      *y = 20; // UB: y is disabled
      │ │      ^^^^^^^ Triggering memory operation
    9 │ │      let val = *x;
   10 │ │  }
      │ ╰──' 1: Entry point
   11 │    
  [1]

Raw pointers don't get new tags
  $ soteria-rust exec-main raw-ptrs.rs --clean
  note: Done, no errors found
  
  raw_ptrs::main: ran 1 branch
  PC 1: true

Raw pointers can access outside the parent's range, with offsets
  $ soteria-rust exec-main offsets.rs --clean
  note: Done, no errors found
  
  offsets::main: ran 1 branch
  PC 1: true

Can have two mutable protected refs to the same allocation, if they don't overlap
  $ soteria-rust exec-main two-mut-protected.rs --clean
  note: Done, no errors found
  
  two_mut_protected::main: ran 1 branch
  PC 1: true

UnsafeCell allow foreign writes followed by local writes
  $ soteria-rust exec-main cell.rs --clean
  note: Done, no errors found
  
  cell::main: ran 1 branch
  PC 1: true

Nested UnsafeCells work too -- skipped for now, due to Charon changing the translation of IS_ZST
  $ true || soteria-rust exec-main nested.rs --clean

Test --ignore-aliasing flag
  $ soteria-rust exec-main simple-fail.rs --clean --ignore-aliasing
  note: Done, no errors found
  
  simple_fail::main: ran 1 branch
  PC 1: true
