Test memory leaks
  $ soteria-rust rustc leak.rs --clean --no-timing
  Compiling... done in <time>
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  warning: Memory leak in main
      ┌─ $TESTCASE_ROOT/leak.rs:1:2
    1 │  fn main() {
      │   ^^^^^^^^^
      │   │
      │   Leaking function
      │   1: Entry point
  
  [1]

Test reading the max and min chars (used to crash Charon-ML)
  $ soteria-rust rustc char_min_max.rs --clean --no-timing
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Test casting between integer types
  $ soteria-rust rustc int_casting.rs --clean --no-timing
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Splitting and merging, via a union
  $ soteria-rust rustc split_merges.rs --clean --no-timing
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  

Test unwinding, and catching that unwind; we need to ignore leaks as this uses a Box.
  $ soteria-rust rustc unwind.rs --clean --no-timing --ignore-leaks
  Compiling... done in <time>
  warning: main (<time>): unsupported feature, Cannot compute layout: error type
  Error(&dyn not supported in Obol)
  Occurred when computing:
  Error(&dyn not supported in Obol)
  [2]
Test that we properly handle the niche optimisation
  $ soteria-rust rustc niche_optim.rs --clean --no-timing --ignore-leaks
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: (V|1| <u 0x7ffffffffffffffb) /\ (0x0000000000000000 != V|1|) /\
        (extract[0-1](V|1|) == 0b00) /\ (V|2| <u 0x7ffffffffffffff7) /\
        (0x0000000000000000 != V|2|) /\ (0b00 == extract[0-1](V|2|)) /\
        (V|2| != 0xfffffffffffffffc) /\ (V|3| <u 0x7ffffffffffffffb) /\
        (0x0000000000000000 != V|3|) /\ (0b00 == extract[0-1](V|3|))
  
