Test memory leaks
  $ soteria-rust rustc leak.rs --clean
  error: Found issues
  
  leak::main: error in 1 branch (out of 1):
  warning: Memory leak in leak::main
      ┌─ $TESTCASE_ROOT/leak.rs:1:1
    1 │      fn main() {
      │ ╭────'
      │ │ ╭──^
    2 │ │ │      std::mem::forget(Box::new(11));
    3 │ │ │  }
      │ ╰─│  ' 1: Entry point
      │   ╰──^ Leaking function
    4 │      
  [1]

Test reading the max and min chars (used to crash Charon-ML)
  $ soteria-rust rustc char_min_max.rs --clean
  note: Done, no errors found
  
  char_min_max::main: ran 1 branch
  PC 1: true

Test casting between integer types
  $ soteria-rust rustc int_casting.rs --clean
  note: Done, no errors found
  
  int_casting::main: ran 1 branch
  PC 1: true

Splitting and merging, via a union
  $ soteria-rust rustc split_merges.rs --clean
  note: Done, no errors found
  
  split_merges::main: ran 1 branch
  PC 1: true

Test unwinding, and catching that unwind; we need to ignore leaks as this uses a Box.
  $ soteria-rust rustc unwind.rs --clean --ignore-leaks
  note: Done, no errors found
  
  unwind::main: ran 2 branches
  PC 1: (0 == V|1|)
  PC 2: (1 == V|1|) /\ (V|2| <= 0x7ffffffffffffffe) /\ (1 <= V|2|)
