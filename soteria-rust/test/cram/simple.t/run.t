Test memory leaks
  $ soteria-rust rustc leak.rs --clean --no-timing
  Compiling... done in <time>
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  warning: Memory leak at ../alloc/src/alloc.rs:250:9-39 in main
      ┌─ $TESTCASE_ROOT/leak.rs:1:2
    1 │  fn main() {
      │   ^^^^^^^^^
      │   │
      │   Leaking function
      │   1: Entry point
  PC 1: (extract[0-1](V|1|) == 0b00) /\ (0x0000000000000001 <=u V|1|) /\
        (V|1| <=u 0x7ffffffffffffffa)
  
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
  error: main: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: assertion failed: result.is_err() in main
      ┌─ $SOTERIA-RUST/std/src/lib.rs:20:10
   20 │          rusteria::assert(!!$cond, concat!("assertion failed: ", stringify!($cond)));
      │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │           │
      │           Triggering memory operation
      │           2: Call trace
      ┌─ $TESTCASE_ROOT/unwind.rs:1:2
    1 │  fn main() {
      │   --------- 1: Entry point
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  [1]
Test that we properly handle the niche optimisation
  $ soteria-rust rustc niche_optim.rs --clean --no-timing --ignore-leaks
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: (extract[0-1](V|1|) == 0b00) /\ (0b00 == extract[0-1](V|2|)) /\
        (0b00 == extract[0-1](V|3|)) /\ (0x0000000000000001 <=u V|1|) /\
        (V|1| <=u 0x7ffffffffffffffa) /\ (0x0000000000000001 <=u V|2|) /\
        (V|2| <=u 0x7ffffffffffffff6) /\ (0x0000000000000001 <=u V|3|) /\
        (V|3| <=u 0x7ffffffffffffffa)
  
Test function calls on function pointers
  $ soteria-rust rustc fn_ptr.rs --clean --no-timing
  Compiling... done in <time>
  note: fn_ptr_call: done in <time>, ran 1 branch
  PC 1: empty
  
  error: fn_ptr_read: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr_read
      ┌─ $TESTCASE_ROOT/fn_ptr.rs:25:19
   21 │  fn fn_ptr_read() {
      │   ---------------- 1: Entry point
   22 │      let add: fn(u8, u8) -> u8 = add;
   23 │      let ptr = add as *const u8;
   24 │      unsafe {
   25 │          let _b = *ptr;
      │                    ^^^^ Triggering memory operation
  PC 1: empty
  
  error: fn_ptr_write: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr_write
      ┌─ $TESTCASE_ROOT/fn_ptr.rs:34:10
   30 │  fn fn_ptr_write() {
      │   ----------------- 1: Entry point
   31 │      let add: fn(u8, u8) -> u8 = add;
   32 │      let ptr = add as *mut u8;
   33 │      unsafe {
   34 │          *ptr = 0;
      │           ^^^^^^^^ Triggering memory operation
  PC 1: empty
  
  [1]

Check strict provenance disables int to ptr casts
  $ soteria-rust rustc provenance.rs --clean --no-timing --provenance strict
  Compiling... done in <time>
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  bug: Attempted ot cast integer to pointer with strict provenance in main
      ┌─ $RUSTLIB/src/rust/library/core/src/ptr/mod.rs:975:6
  975 │      addr as *const T
      │       ^^^^^^^^^^^^^^^ Triggering memory operation
      ┌─ $TESTCASE_ROOT/provenance.rs:5:19
    1 │  fn main() {
      │   --------- 1: Entry point
    2 │      let mut x: u8 = 0;
    3 │      let p = &mut x as *mut u8;
    4 │      let p_int = p.expose_provenance();
    5 │      let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
      │                    ---------------------------------------------- 2: Call trace
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Check permissive provenance allows int to ptr casts
  $ soteria-rust rustc provenance.rs --clean --no-timing --provenance permissive
  Compiling... done in <time>
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  bug: UB: dangling pointer in main
      ┌─ $TESTCASE_ROOT/provenance.rs:7:10
    1 │  fn main() {
      │   --------- 1: Entry point
    2 │      let mut x: u8 = 0;
    3 │      let p = &mut x as *mut u8;
    4 │      let p_int = p.expose_provenance();
    5 │      let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
    6 │      unsafe {
    7 │          *p_back = 1;
      │           ^^^^^^^^^^^ Triggering memory operation
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Check corner cases with permissive provenance, around transmutes
  $ soteria-rust rustc provenance_transmute.rs --clean --no-timing --provenance permissive
  Compiling... done in <time>
  error: addr_doesnt_expose: found issues in <time>, errors in 1 branch (out of 1)
  bug: UB: dangling pointer in addr_doesnt_expose
      ┌─ $TESTCASE_ROOT/provenance_transmute.rs:9:10
    2 │  fn addr_doesnt_expose() {
      │   ----------------------- 1: Entry point
    3 │      let mut x: u8 = 0;
    4 │      let p = &mut x as *mut u8;
    5 │      let p_int = p.addr();
    6 │      // this will not return the provenance information, since it was never exposed!
    7 │      let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
    8 │      unsafe {
    9 │          *p_back = 1;
      │           ^^^^^^^^^^^ Triggering memory operation
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  error: transmute_doesnt_restore_provenance: found issues in <time>, errors in 1 branch (out of 1)
  bug: UB: dangling pointer in transmute_doesnt_restore_provenance
      ┌─ $TESTCASE_ROOT/provenance_transmute.rs:22:10
   15 │  fn transmute_doesnt_restore_provenance() {
      │   ---------------------------------------- 1: Entry point
   16 │      let mut x: u8 = 0;
   17 │      let p = &mut x as *mut u8;
   18 │      let p_int = p.expose_provenance();
   19 │      // this will not return the provenance information, because it's a transmute!
   20 │      let p_back = unsafe { std::mem::transmute::<usize, *mut u8>(p_int) };
   21 │      unsafe {
   22 │          *p_back = 1;
      │           ^^^^^^^^^^^ Triggering memory operation
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]
