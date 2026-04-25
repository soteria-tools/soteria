Test memory leaks
  $ soteria-rust exec leak.rs
  Compiling... done in <time>
  => Running leak::main...
  error: leak::main: found issues in <time>, errors in 1 branch (out of 1)
  warning: Memory leak in leak::main
      ┌─ $RUSTLIB/library/alloc/src/alloc.rs:95:9
   95 │            __rust_alloc(layout.size(), layout.align())
      │            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      │            │
      │            Triggering operation
      │            5: Allocation
      ·    
  311 │        const fn alloc_impl(&self, layout: Layout, zeroed: bool) -> Result<NonNull<[u8]>, AllocError> {
  312 │ ╭          core::intrinsics::const_eval_select(
  313 │ │              (layout, zeroed),
  314 │ │              Global::alloc_impl_const,
  315 │ │              Global::alloc_impl_runtime,
  316 │ │          )
      │ ╰──────────' 4: Call trace
  317 │        }
      ┌─ $RUSTLIB/library/alloc/src/boxed.rs:265:16
  265 │            return box_new(x);
      │                   ---------- 3: Call trace
      ┌─ $TESTCASE_ROOT/leak.rs:2:22
    1 │    fn main() {
      │    --------- 1: Leaking function
    2 │        std::mem::forget(Box::new(11));
      │                         ------------ 2: Call trace
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  [1]

Test reading the max and min chars (used to crash Charon-ML)
  $ soteria-rust exec char_min_max.rs
  Compiling... done in <time>
  => Running char_min_max::main...
  note: char_min_max::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test casting between integer types
  $ soteria-rust exec int_casting.rs
  Compiling... done in <time>
  => Running int_casting::main...
  note: int_casting::main: done in <time>, ran 1 branch
  PC 1: empty
  
Splitting and merging, via a union
  $ soteria-rust exec split_merges.rs
  Compiling... done in <time>
  => Running split_merges::endianness...
  note: split_merges::endianness: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running split_merges::uninit_gap...
  warning: Invalid reference: Uninitialized memory access
      ┌─ $TESTCASE_ROOT/split_merges.rs:64:9
   52 │  fn uninit_gap() {
      │  --------------- 1: Entry point
      ·  
   64 │          assert_eq!(x.as_u32, 0x1234_5678);
      │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Triggering operation
  error: split_merges::uninit_gap: found issues in <time>, errors in 1 branch (out of 1)
  bug: Uninitialized memory access in split_merges::uninit_gap
      ┌─ $TESTCASE_ROOT/split_merges.rs:64:9
   52 │  fn uninit_gap() {
      │  --------------- 1: Entry point
      ·  
   64 │          assert_eq!(x.as_u32, 0x1234_5678);
      │          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Memory load
  PC 1: empty
  
  [1]
Test unwinding, and catching that unwind; we need to ignore leaks as this uses a Box.
  $ soteria-rust exec unwind.rs --ignore-leaks
  Compiling... done in <time>
  => Running unwind::main...
  note: unwind::main: done in <time>, ran 2 branches
  PC 1: (V|1| == 0x01) /\ (0x0000000000000001 <=u V|2|) /\
        (V|2| <=u 0x7ffffffffffffffd) /\ (V|1| == 0x01)
  PC 2: (0x00 == V|1|) /\ (0x00 == V|1|)
  
Test that we properly handle the niche optimisation
  $ soteria-rust exec niche_optim.rs --ignore-leaks
  Compiling... done in <time>
  => Running niche_optim::main...
  note: niche_optim::main: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|2|) /\ (V|2| <=u 0x7ffffffffffffff6) /\
        (0x0000000000000004 <=u V|3|) /\ (V|3| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00) /\ (0b00 == extract[0-1](V|2|)) /\
        (0b00 == extract[0-1](V|3|))
  
Test function calls on function pointers
  $ soteria-rust exec fn_ptr.rs
  Compiling... done in <time>
  => Running fn_ptr::fn_ptr_call...
  note: fn_ptr::fn_ptr_call: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running fn_ptr::fn_ptr_read...
  error: fn_ptr::fn_ptr_read: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr::fn_ptr_read
      ┌─ $TESTCASE_ROOT/fn_ptr.rs:25:18
   21 │  fn fn_ptr_read() {
      │  ---------------- 1: Entry point
      ·  
   25 │          let _b = *ptr;
      │                   ^^^^ Memory load
  PC 1: empty
  
  => Running fn_ptr::fn_ptr_write...
  error: fn_ptr::fn_ptr_write: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr::fn_ptr_write
      ┌─ $TESTCASE_ROOT/fn_ptr.rs:34:9
   30 │  fn fn_ptr_write() {
      │  ----------------- 1: Entry point
      ·  
   34 │          *ptr = 0;
      │          ^^^^^^^^ Memory store
  PC 1: empty
  
  [1]

Check strict provenance disables int to ptr casts
  $ soteria-rust exec provenance.rs --provenance strict
  Compiling... done in <time>
  => Running provenance::with_exposed...
  error: provenance::with_exposed: found issues in <time>, errors in 1 branch (out of 1)
  bug: Attempted to cast an integer to an pointer with strict provenance in provenance::with_exposed
      ┌─ $RUSTLIB/library/core/src/ptr/mod.rs:988:5
  988 │      addr as *const T
      │      ^^^^^^^^^^^^^^^^ Casting integer to pointer
      ┌─ $TESTCASE_ROOT/provenance.rs:6:18
    2 │  fn with_exposed() {
      │  ----------------- 1: Entry point
      ·  
    6 │      let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
      │                   ---------------------------------------------- 2: Call trace
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Check permissive provenance allows int to ptr casts
  $ soteria-rust exec provenance.rs --provenance permissive
  Compiling... done in <time>
  => Running provenance::with_exposed...
  note: provenance::with_exposed: done in <time>, ran 1 branch
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  

Check corner cases with permissive provenance, around transmutes
  $ soteria-rust exec provenance_transmute.rs --provenance permissive
  Compiling... done in <time>
  => Running provenance_transmute::addr_doesnt_expose...
  error: provenance_transmute::addr_doesnt_expose: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in provenance_transmute::addr_doesnt_expose
      ┌─ $TESTCASE_ROOT/provenance_transmute.rs:9:9
    2 │  fn addr_doesnt_expose() {
      │  ----------------------- 1: Entry point
      ·  
    9 │          *p_back = 1;
      │          ^^^^^^^^^^^ Memory store
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  => Running provenance_transmute::transmute_doesnt_restore_provenance...
  error: provenance_transmute::transmute_doesnt_restore_provenance: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in provenance_transmute::transmute_doesnt_restore_provenance
      ┌─ $TESTCASE_ROOT/provenance_transmute.rs:22:9
   15 │  fn transmute_doesnt_restore_provenance() {
      │  ---------------------------------------- 1: Entry point
      ·  
   22 │          *p_back = 1;
      │          ^^^^^^^^^^^ Memory store
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Test transmutations keeping the bit-patterns the same
  $ soteria-rust exec transmute_roundtrip.rs
  Compiling... done in <time>
  => Running transmute_roundtrip::one_way_u32_f32...
  note: transmute_roundtrip::one_way_u32_f32: done in <time>, ran 1 branch
  PC 1: !(fis(NaN)(bv2f[F32](V|1|))) /\ (V|1| == V|2|) /\
        (bv2f[F32](V|1|) == bv2f[F32](V|2|))
  
  => Running transmute_roundtrip::one_way_f32_u32...
  note: transmute_roundtrip::one_way_f32_u32: done in <time>, ran 2 branches
  PC 1: fis(NaN)(V|1|) /\ fis(NaN)(V|1|) /\ (bv2f[F32](V|2|) == V|1|)
  PC 2: !(fis(NaN)(V|1|)) /\ !(fis(NaN)(V|1|)) /\ (bv2f[F32](V|2|) == V|1|)
  
  => Running transmute_roundtrip::two_way_u32_i32...
  note: transmute_roundtrip::two_way_u32_i32: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running transmute_roundtrip::two_way_u8x4_u16x2...
  note: transmute_roundtrip::two_way_u8x4_u16x2: done in <time>, ran 1 branch
  PC 1: empty
  
Test null and dangling pointers
  $ soteria-rust exec dangling_ptrs.rs
  Compiling... done in <time>
  => Running dangling_ptrs::null_ptr_zst...
  note: dangling_ptrs::null_ptr_zst: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running dangling_ptrs::null_ptr_not_zst...
  error: dangling_ptrs::null_ptr_not_zst: found issues in <time>, errors in 1 branch (out of 1)
  error: Null dereference in dangling_ptrs::null_ptr_not_zst
      ┌─ $TESTCASE_ROOT/dangling_ptrs.rs:11:30
    9 │  fn null_ptr_not_zst() {
      │  --------------------- 1: Entry point
   10 │      let ptr: *const u32 = std::ptr::null();
   11 │      let _val: u32 = unsafe { *ptr };
      │                               ^^^^ Memory load
  PC 1: empty
  
  => Running dangling_ptrs::dangling_ptr_not_zst...
  error: dangling_ptrs::dangling_ptr_not_zst: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in dangling_ptrs::dangling_ptr_not_zst
      ┌─ $TESTCASE_ROOT/dangling_ptrs.rs:17:29
   15 │  fn dangling_ptr_not_zst() {
      │  ------------------------- 1: Entry point
   16 │      let ptr: *const u8 = 0xdeadbeef as *const u8;
   17 │      let _val: u8 = unsafe { *ptr };
      │                              ^^^^ Memory load
  PC 1: empty
  
  [1]

Test exposing function pointers
  $ soteria-rust exec expose_fn_ptr.rs
  Compiling... done in <time>
  => Running expose_fn_ptr::main...
  note: expose_fn_ptr::main: done in <time>, ran 1 branch
  PC 1: (0x0000000000000010 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffe) /\
        (extract[0-3](V|1|) == 0x0)
  
Test thread local statics; the two warnings due to opaque functions are to be expected, as we do not run the test suite with a sysroot.
  $ soteria-rust exec thread_local.rs
  Compiling... done in <time>
  => Running thread_local::pub_static_cell...
  note: thread_local::pub_static_cell: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running thread_local::static_ref_cell...
  warning: thread_local::static_ref_cell (<time>): unsupported feature, Can't execute function std::sys::thread_local::destructors::list::register: GAst.Missing
  
  => Running thread_local::pub_static_from_const_expr...
  warning: thread_local::pub_static_from_const_expr (<time>): unsupported feature, Can't execute function std::sys::thread_local::destructors::list::register: GAst.Missing
  
  [2]

Test cloning ZSTs works; in particular, this generates a function with an empty body that just returns, so if we don't handle the ZST case we get an uninit access.
  $ soteria-rust exec clone_zst.rs
  Compiling... done in <time>
  => Running clone_zst::main...
  note: clone_zst::main: done in <time>, ran 1 branch
  PC 1: empty
  
--fail-fast should stop symbolic execution upon the first error encountered
  $ soteria-rust exec fail_fast.rs --fail-fast
  Compiling... done in <time>
  => Running fail_fast::main...
  error: fail_fast::main: found an issue in <time> after exploring 1 branch -- stopped immediately (fail-fast)
  error: Panic: ok in fail_fast::main
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
  $ soteria-rust exec ref_validity.rs --recursive-validity=allow
  Compiling... done in <time>
  => Running ref_validity::test_uninit_ref...
  note: ref_validity::test_uninit_ref: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running ref_validity::test_dangling_ref...
  error: ref_validity::test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in ref_validity::test_dangling_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:17:38
   14 │  fn test_dangling_ref() {
      │  ---------------------- 1: Entry point
      ·  
   17 │      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      │                                       ^^^^^^^^ Dangling check
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running ref_validity::test_unaligned_ref...
  error: ref_validity::test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in ref_validity::test_unaligned_ref
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
  $ soteria-rust exec ref_validity.rs --recursive-validity=deny
  Compiling... done in <time>
  => Running ref_validity::test_uninit_ref...
  error: ref_validity::test_uninit_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Invalid reference: Uninitialized memory access in ref_validity::test_uninit_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:7:33
    4 │  fn test_uninit_ref() {
      │  -------------------- 1: Entry point
      ·  
    7 │      let as_ref: &u32 = unsafe { &*as_ptr };
      │                                  ^^^^^^^^ Fake read
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running ref_validity::test_dangling_ref...
  error: ref_validity::test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in ref_validity::test_dangling_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:17:38
   14 │  fn test_dangling_ref() {
      │  ---------------------- 1: Entry point
      ·  
   17 │      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      │                                       ^^^^^^^^ Dangling check
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running ref_validity::test_unaligned_ref...
  error: ref_validity::test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in ref_validity::test_unaligned_ref
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
  $ soteria-rust exec ref_validity.rs --recursive-validity=warn
  Compiling... done in <time>
  => Running ref_validity::test_uninit_ref...
  warning: Invalid reference: Uninitialized memory access
      ┌─ $TESTCASE_ROOT/ref_validity.rs:7:33
    4 │  fn test_uninit_ref() {
      │  -------------------- 1: Entry point
      ·  
    7 │      let as_ref: &u32 = unsafe { &*as_ptr };
      │                                  ^^^^^^^^ Triggering operation
  note: ref_validity::test_uninit_ref: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running ref_validity::test_dangling_ref...
  error: ref_validity::test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in ref_validity::test_dangling_ref
      ┌─ $TESTCASE_ROOT/ref_validity.rs:17:38
   14 │  fn test_dangling_ref() {
      │  ---------------------- 1: Entry point
      ·  
   17 │      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      │                                       ^^^^^^^^ Dangling check
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  => Running ref_validity::test_unaligned_ref...
  error: ref_validity::test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in ref_validity::test_unaligned_ref
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
  $ soteria-rust exec approx_float.rs
  Compiling... done in <time>
  => Running approx_float::main...
  warning: A complex floating point intrinsic was encountered; it will be executed with a significant over-approximation.
  note: approx_float::main: done in <time>, ran 1 branch
  PC 1: !(fis(Infinite)(V|1|)) /\ !(fis(NaN)(V|1|)) /\
        ((V|2| ==. 1.0f) || !((V|1| ==. 0.0f))) /\ (-1.0f <=. V|2|) /\
        (V|2| <=. 1.0f) /\ (-1f <=. V|2|) /\ (V|2| <=. 1f)
  

Test approximation of complex float operations -- denied
  $ soteria-rust exec approx_float.rs --approx-floating-ops deny
  Compiling... done in <time>
  => Running approx_float::main...
  note: approx_float::main: done in <time>, ran 0 branches
  
  

Test approximation of complex float operations -- allowed
  $ soteria-rust exec approx_float.rs --approx-floating-ops allow
  Compiling... done in <time>
  => Running approx_float::main...
  note: approx_float::main: done in <time>, ran 1 branch
  PC 1: !(fis(Infinite)(V|1|)) /\ !(fis(NaN)(V|1|)) /\
        ((V|2| ==. 1.0f) || !((V|1| ==. 0.0f))) /\ (-1.0f <=. V|2|) /\
        (V|2| <=. 1.0f) /\ (-1f <=. V|2|) /\ (V|2| <=. 1f)
  
Test enum constructors as functions; this broke with a rust toolchain update
  $ soteria-rust exec enum_constructor.rs
  Compiling... done in <time>
  => Running enum_constructor::main...
  note: enum_constructor::main: done in <time>, ran 1 branch
  PC 1: empty
  
Print the callgraph
  $ soteria-rust exec callgraph.rs --dump-callgraph callgraph.dot && cat callgraph.dot
  Compiling... done in <time>
  => Running callgraph::main...
  note: callgraph::main: done in <time>, ran 1 branch
  PC 1: (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffa) /\
        (extract[0-1](V|1|) == 0b00)
  
  digraph callgraph {
    node [shape=box fontname="monospace"];
    n0 [label="Range<A>>::next" tooltip="std::iter::range::<impl std::iter::Iterator for std::ops::Range<A>>::next::<i32>"];
    n13 [label="io::_print" tooltip="std::io::_print"];
    n7 [label="callgraph::limit" tooltip="callgraph::limit"];
    n2 [label="callgraph::choose" tooltip="callgraph::choose"];
    n12 [label="callgraph::main" tooltip="callgraph::main"];
    n6 [label="callgraph::run" tooltip="callgraph::run"];
    n16 [label="Step>::forward_unchecked" tooltip="<i32 as std::iter::Step>::forward_unchecked"];
    n3 [label="callgraph::twice" tooltip="callgraph::twice"];
    n14 [label="Argument::<'_>::new_display" tooltip="core::fmt::rt::Argument::<'_>::new_display::<'_, i32>"];
    n4 [label="callgraph::dec" tooltip="callgraph::dec"];
    n8 [label="callgraph::score" tooltip="callgraph::score"];
    n17 [label="PartialOrd for i32>::lt" tooltip="std::cmp::impls::<impl std::cmp::PartialOrd for i32>::lt"];
    n10 [label="callgraph::ping" tooltip="callgraph::ping"];
    n15 [label="Arguments::<'a>::new" tooltip="std::fmt::Arguments::<'a>::new::<'_, 12usize, 1usize>"];
    n11 [label="callgraph::pong" tooltip="callgraph::pong"];
    n9 [label="IntoIterator>::into_iter" tooltip="<I as std::iter::IntoIterator>::into_iter::<std::ops::Range::<i32>>"];
    n5 [label="callgraph::inc" tooltip="callgraph::inc"];
    n1 [label="RangeIteratorImpl>::spec_next" tooltip="<std::ops::Range<T> as std::iter::range::RangeIteratorImpl>::spec_next::<i32>"];
    n0 -> n1;
    n2 -> n3;
    n2 -> n4;
    n2 -> n5;
    n6 -> n0;
    n6 -> n7;
    n6 -> n8;
    n6 -> n9;
    n10 -> n11;
    n12 -> n13;
    n12 -> n6;
    n12 -> n14;
    n12 -> n15;
    n8 -> n2;
    n8 -> n10;
    n11 -> n10;
    n1 -> n16;
    n1 -> n17;
  }
