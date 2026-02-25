Test memory leaks
  $ soteria-rust rustc leak.rs
  Compiling... done in <time>
  => Running main...
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  warning: Memory leak in main
      ┌─ $RUSTLIB/src/rust/library/alloc/src/alloc.rs:251:9
  251 │          self.alloc_impl(layout, false)
      │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          4: Allocation
      ┌─ $RUSTLIB/src/rust/library/alloc/src/boxed.rs:266:16
  266 │          return box_new(x);
      │                 ---------- 3: Call trace
      ┌─ $TESTCASE_ROOT/leak.rs:2:22
    1 │  fn main() {
      │  --------- 1: Leaking function
    2 │      std::mem::forget(Box::new(11));
      │                       ------------ 2: Call trace
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  [1]

Test reading the max and min chars (used to crash Charon-ML)
  $ soteria-rust rustc char_min_max.rs
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  
Test casting between integer types
  $ soteria-rust rustc int_casting.rs
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  
Splitting and merging, via a union
  $ soteria-rust rustc split_merges.rs
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  
Test unwinding, and catching that unwind; we need to ignore leaks as this uses a Box.
  $ soteria-rust rustc unwind.rs --ignore-leaks
  Compiling... done in <time>
  => Running main...
  error: main: found issues in <time>, errors in 1 branch (out of 2)
  error: Failed assertion: assertion failed: result.is_err() in main
      ┌─ $TESTCASE_ROOT/unwind.rs:9:9
    1 │  fn main() {
      │  --------- 1: Entry point
      ·  
    9 │          assert!(result.is_err());
      │          ^^^^^^^^^^^^^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          2: Call trace
  PC 1: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  [1]
Test that we properly handle the niche optimisation
  $ soteria-rust rustc niche_optim.rs --ignore-leaks
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|2|) /\ (V|2| <=u 0x7ffffffffffffff6) /\
        (0x0000000000000004 <=u V|3|) /\ (V|3| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00) /\ (0b00 == extract[0-1](V|2|)) /\
        (0b00 == extract[0-1](V|3|))
  
Test function calls on function pointers
  $ soteria-rust rustc fn_ptr.rs
  Compiling... done in <time>
  => Running fn_ptr_call...
  note: fn_ptr_call: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running fn_ptr_read...
  error: fn_ptr_read: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr_read
      ┌─ $TESTCASE_ROOT/fn_ptr.rs:25:18
   21 │  fn fn_ptr_read() {
      │  ---------------- 1: Entry point
      ·  
   25 │          let _b = *ptr;
      │                   ^^^^ Memory load
  PC 1: empty
  
  => Running fn_ptr_write...
  error: fn_ptr_write: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr_write
      ┌─ $TESTCASE_ROOT/fn_ptr.rs:34:9
   30 │  fn fn_ptr_write() {
      │  ----------------- 1: Entry point
      ·  
   34 │          *ptr = 0;
      │          ^^^^^^^^ Memory store
  PC 1: empty
  
  [1]

Check strict provenance disables int to ptr casts
  $ soteria-rust rustc provenance.rs --provenance strict
  Compiling... done in <time>
  => Running main...
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  bug: Attempted to cast an integer to an pointer with strict provenance in main
      ┌─ $RUSTLIB/src/rust/library/core/src/ptr/mod.rs:986:5
  986 │      addr as *const T
      │      ^^^^^^^^^^^^^^^^ Casting integer to pointer
      ┌─ $TESTCASE_ROOT/provenance.rs:5:18
    1 │  fn main() {
      │  --------- 1: Entry point
      ·  
    5 │      let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
      │                   ---------------------------------------------- 2: Call trace
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Check permissive provenance allows int to ptr casts
  $ soteria-rust rustc provenance.rs --provenance permissive
  Compiling... done in <time>
  => Running main...
  error: main: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in main
      ┌─ $TESTCASE_ROOT/provenance.rs:7:9
    1 │  fn main() {
      │  --------- 1: Entry point
      ·  
    7 │          *p_back = 1;
      │          ^^^^^^^^^^^ Memory store
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Check corner cases with permissive provenance, around transmutes
  $ soteria-rust rustc provenance_transmute.rs --provenance permissive
  Compiling... done in <time>
  => Running addr_doesnt_expose...
  error: addr_doesnt_expose: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in addr_doesnt_expose
      ┌─ $TESTCASE_ROOT/provenance_transmute.rs:9:9
    2 │  fn addr_doesnt_expose() {
      │  ----------------------- 1: Entry point
      ·  
    9 │          *p_back = 1;
      │          ^^^^^^^^^^^ Memory store
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  => Running transmute_doesnt_restore_provenance...
  error: transmute_doesnt_restore_provenance: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in transmute_doesnt_restore_provenance
      ┌─ $TESTCASE_ROOT/provenance_transmute.rs:22:9
   15 │  fn transmute_doesnt_restore_provenance() {
      │  ---------------------------------------- 1: Entry point
      ·  
   22 │          *p_back = 1;
      │          ^^^^^^^^^^^ Memory store
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Test transmutations keeping the bit-patterns the same
  $ soteria-rust rustc transmute_roundtrip.rs
  Compiling... done in <time>
  => Running one_way_u32_f32...
  note: one_way_u32_f32: done in <time>, ran 1 branch
  PC 1: !(fis(NaN)(bv2f[F32](V|1|))) /\ (V|1| == V|2|) /\
        (bv2f[F32](V|1|) == bv2f[F32](V|2|))
  
  => Running one_way_f32_u32...
  note: one_way_f32_u32: done in <time>, ran 2 branches
  PC 1: fis(NaN)(V|1|) /\ (bv2f[F32](V|2|) == V|1|)
  PC 2: !(fis(NaN)(V|1|)) /\ (bv2f[F32](V|2|) == V|1|)
  
  => Running two_way_u32_i32...
  note: two_way_u32_i32: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running two_way_u8x4_u16x2...
  note: two_way_u8x4_u16x2: done in <time>, ran 1 branch
  PC 1: empty
  
Test null and dangling pointers
  $ soteria-rust rustc dangling_ptrs.rs
  Compiling... done in <time>
  => Running null_ptr_zst...
  note: null_ptr_zst: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running null_ptr_not_zst...
  error: null_ptr_not_zst: found issues in <time>, errors in 1 branch (out of 1)
  error: Null dereference in null_ptr_not_zst
      ┌─ $TESTCASE_ROOT/dangling_ptrs.rs:11:30
    9 │  fn null_ptr_not_zst() {
      │  --------------------- 1: Entry point
   10 │      let ptr: *const u32 = std::ptr::null();
   11 │      let _val: u32 = unsafe { *ptr };
      │                               ^^^^ Memory load
  PC 1: empty
  
  => Running dangling_ptr_not_zst...
  error: dangling_ptr_not_zst: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in dangling_ptr_not_zst
      ┌─ $TESTCASE_ROOT/dangling_ptrs.rs:17:29
   15 │  fn dangling_ptr_not_zst() {
      │  ------------------------- 1: Entry point
   16 │      let ptr: *const u8 = 0xdeadbeef as *const u8;
   17 │      let _val: u8 = unsafe { *ptr };
      │                              ^^^^ Memory load
  PC 1: empty
  
  [1]

Test exposing function pointers
  $ soteria-rust rustc expose_fn_ptr.rs
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 1 branch
  PC 1: (0x0000000000000010 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffe) /\
        (extract[0-3](V|1|) == 0x0)
  
Test thread local statics; the two warnings due to opaque functions are to be expected, as we do not run the test suite with a sysroot.
  $ soteria-rust rustc thread_local.rs
  Compiling... done in <time>
  => Running pub_static_cell...
  note: pub_static_cell: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running static_ref_cell...
  warning: static_ref_cell (<time>): unsupported feature, Function std::sys::thread_local::destructors::list::register is opaque
  
  => Running pub_static_from_const_expr...
  warning: pub_static_from_const_expr (<time>): unsupported feature, Function std::sys::thread_local::destructors::list::register is opaque
  
  [2]

Test cloning ZSTs works; in particular, this generates a function with an empty body that just returns, so if we don't handle the ZST case we get an uninit access.
  $ soteria-rust rustc clone_zst.rs
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 1 branch
  PC 1: empty
  
--fail-fast should stop symbolic execution upon the first error encountered
  $ soteria-rust rustc fail_fast.rs --fail-fast
  Compiling... done in <time>
  => Running main...
  error: main: found an issue in <time> after exploring 1 branch -- stopped immediately (fail-fast)
  error: Panic: ok in main
      ┌─ $TESTCASE_ROOT/fail_fast.rs:4:9
    1 │  fn main() {
      │  --------- 1: Entry point
      ·  
    4 │          panic!("ok");
      │          ^^^^^^^^^^^^
      │          │
      │          Triggering operation
      │          2: Call trace
  PC 1: (V|1| == 0x01) /\ (V|1| == 0x01)
  
  [1]

Test recursive validity check for references; disabled
  $ soteria-rust rustc ref_validity.rs --recursive-validity=allow
  Compiling... done in <time>
  => Running test_uninit_ref...
  note: test_uninit_ref: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running test_dangling_ref...
  error: test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in test_dangling_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:17:38
   14 │  fn test_dangling_ref() {
      │  ---------------------- 1: Entry point
      ·  
   17 │      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      │                                       ^^^^^^^^ Dangling check
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running test_unaligned_ref...
  error: test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in test_unaligned_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:25:33
   22 │  fn test_unaligned_ref() {
      │  ----------------------- 1: Entry point
      ·  
   25 │      let as_ref: &u64 = unsafe { &*as_ptr };
      │                                  ^^^^^^^^ Requires well-aligned pointer
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffff6) /\
        (extract[0-1](V|1|) == 0b00)
  
  [1]

Test recursive validity check for references; enabled
  $ soteria-rust rustc ref_validity.rs --recursive-validity=deny
  Compiling... done in <time>
  => Running test_uninit_ref...
  error: test_uninit_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Invalid reference: Uninitialized memory access in test_uninit_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:7:33
    4 │  fn test_uninit_ref() {
      │  -------------------- 1: Entry point
      ·  
    7 │      let as_ref: &u32 = unsafe { &*as_ptr };
      │                                  ^^^^^^^^ Fake read
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running test_dangling_ref...
  error: test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in test_dangling_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:17:38
   14 │  fn test_dangling_ref() {
      │  ---------------------- 1: Entry point
      ·  
   17 │      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      │                                       ^^^^^^^^ Dangling check
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running test_unaligned_ref...
  error: test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in test_unaligned_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:25:33
   22 │  fn test_unaligned_ref() {
      │  ----------------------- 1: Entry point
      ·  
   25 │      let as_ref: &u64 = unsafe { &*as_ptr };
      │                                  ^^^^^^^^ Requires well-aligned pointer
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffff6) /\
        (extract[0-1](V|1|) == 0b00)
  
  [1]

Test recursive validity check for references; warn
  $ soteria-rust rustc ref_validity.rs --recursive-validity=warn
  Compiling... done in <time>
  => Running test_uninit_ref...
  warning: Invalid reference: Uninitialized memory access
      ┌─ $TESTCASE_ROOT/ref_validity.rs:7:33
    4 │  fn test_uninit_ref() {
      │  -------------------- 1: Entry point
      ·  
    7 │      let as_ref: &u32 = unsafe { &*as_ptr };
      │                                  ^^^^^^^^ Triggering operation
  note: test_uninit_ref: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running test_dangling_ref...
  error: test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in test_dangling_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:17:38
   14 │  fn test_dangling_ref() {
      │  ---------------------- 1: Entry point
      ·  
   17 │      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      │                                       ^^^^^^^^ Dangling check
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running test_unaligned_ref...
  error: test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in test_unaligned_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:25:33
   22 │  fn test_unaligned_ref() {
      │  ----------------------- 1: Entry point
      ·  
   25 │      let as_ref: &u64 = unsafe { &*as_ptr };
      │                                  ^^^^^^^^ Requires well-aligned pointer
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffff6) /\
        (extract[0-1](V|1|) == 0b00)
  
  [1]

Test approximation of complex float operations -- warn (default)
  $ soteria-rust rustc approx_float.rs
  Compiling... done in <time>
  => Running main...
  warning: A complex floating point intrinsic was encountered; it will be executed with a significant over-approximation.
  note: main: done in <time>, ran 1 branch
  PC 1: !(fis(Infinite)(V|1|)) /\ !(fis(NaN)(V|1|)) /\
        ((V|2| ==. 1.0f) || !((V|1| ==. 0.0f))) /\ (-1.0f <=. V|2|) /\
        (V|2| <=. 1.0f) /\ (-1f <=. V|2|) /\ (V|2| <=. 1f)
  

Test approximation of complex float operations -- denied
  $ soteria-rust rustc approx_float.rs --approx-floating-ops deny
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 0 branches
  
  

Test approximation of complex float operations -- allowed
  $ soteria-rust rustc approx_float.rs --approx-floating-ops allow
  Compiling... done in <time>
  => Running main...
  note: main: done in <time>, ran 1 branch
  PC 1: !(fis(Infinite)(V|1|)) /\ !(fis(NaN)(V|1|)) /\
        ((V|2| ==. 1.0f) || !((V|1| ==. 0.0f))) /\ (-1.0f <=. V|2|) /\
        (V|2| <=. 1.0f) /\ (-1f <=. V|2|) /\ (V|2| <=. 1f)
  
