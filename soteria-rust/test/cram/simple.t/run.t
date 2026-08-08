Test memory leaks
  $ soteria-rust exec leak.rs
  Compiling... done in <time>
  => Running leak::main...
  error: leak::main: found issues in <time>, errors in 1 branch (out of 1)
  warning: Memory leak in leak::main
      --> $RUSTLIB/library/alloc/src/alloc.rs:130:9
  130 |            __rust_alloc(layout.size(), layout.alignment())
      |            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |            |
      |            Triggering operation
      |            5: Allocation
      .    
  423 |        const fn alloc_impl(&self, layout: Layout, zeroed: bool) -> Result<NonNull<[u8]>, AllocError> {
  424 | /          core::intrinsics::const_eval_select(
  425 | |              (layout, zeroed),
  426 | |              Global::alloc_impl_const,
  427 | |              Global::alloc_impl_runtime,
  428 | |          )
      | \----------' 4: Call trace
  429 |        }
      --> $RUSTLIB/library/alloc/src/boxed.rs:290:19
  290 |            let ptr = box_new_uninit(<T as SizedTypeProperties>::LAYOUT) as *mut T;
      |                      -------------------------------------------------- 3: Call trace
      --> $TESTCASE_ROOT/leak.rs:2:22
    1 |    fn main() {
      |    --------- 1: Leaking function
    2 |        std::mem::forget(Box::new(11));
      |                         ------------ 2: Call trace
  PC 1: empty
  
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
  error: split_merges::uninit_gap: found issues in <time>, errors in 1 branch (out of 1)
  bug: Uninitialized memory access in split_merges::uninit_gap
      --> $TESTCASE_ROOT/split_merges.rs:64:9
   52 |  fn uninit_gap() {
      |  --------------- 1: Entry point
      .  
   64 |          assert_eq!(x.as_u32, 0x1234_5678);
      |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Memory load
  PC 1: empty
  
  [1]
Test unwinding, and catching that unwind; we need to ignore leaks as this uses a Box.
  $ soteria-rust exec unwind.rs --ignore-leaks
  Compiling... done in <time>
  => Running unwind::main...
  note: unwind::main: done in <time>, ran 2 branches
  PC 1: (0x01 == V|1|) /\ (0x0000000000000001 <=u V|2|) /\
        (V|2| <=u 0x7ffffffffffffffd) /\ (0x01 == V|1|)
  PC 2: (0x00 == V|1|) /\ (0x00 == V|1|)
  
Test that we properly handle the niche optimisation
  $ soteria-rust exec niche_optim.rs --ignore-leaks
  Compiling... done in <time>
  => Running niche_optim::main...
  note: niche_optim::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test function calls on function pointers
  $ soteria-rust exec fn_ptr.rs
  Compiling... done in <time>
  => Running fn_ptr::fn_ptr_call...
  note: fn_ptr::fn_ptr_call: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running fn_ptr::fn_ptr_read...
  error: fn_ptr::fn_ptr_read: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr::fn_ptr_read
      --> $TESTCASE_ROOT/fn_ptr.rs:25:18
   21 |  fn fn_ptr_read() {
      |  ---------------- 1: Entry point
      .  
   25 |          let _b = *ptr;
      |                   ^^^^ Memory load
  PC 1: empty
  
  => Running fn_ptr::fn_ptr_write...
  error: fn_ptr::fn_ptr_write: found issues in <time>, errors in 1 branch (out of 1)
  bug: Accessed function pointer's pointee in fn_ptr::fn_ptr_write
      --> $TESTCASE_ROOT/fn_ptr.rs:34:9
   30 |  fn fn_ptr_write() {
      |  ----------------- 1: Entry point
      .  
   34 |          *ptr = 0;
      |          ^^^^^^^^ Memory store
  PC 1: empty
  
  [1]

Check strict provenance disables int to ptr casts
  $ soteria-rust exec provenance.rs --provenance strict
  Compiling... done in <time>
  => Running provenance::with_exposed...
  error: provenance::with_exposed: found issues in <time>, errors in 1 branch (out of 1)
  bug: Attempted to cast an integer to an pointer with strict provenance in provenance::with_exposed
       --> $RUSTLIB/library/core/src/ptr/mod.rs:1027:5
  1027 |      addr as *const T
       |      ^^^^^^^^^^^^^^^^ Casting integer to pointer
       --> $TESTCASE_ROOT/provenance.rs:6:18
     2 |  fn with_exposed() {
       |  ----------------- 1: Entry point
       .  
     6 |      let p_back = std::ptr::with_exposed_provenance::<u8>(p_int) as *mut u8;
       |                   ---------------------------------------------- 2: Call trace
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  [1]

Check permissive provenance allows int to ptr casts
  $ soteria-rust exec provenance.rs --provenance permissive
  Compiling... done in <time>
  => Running provenance::with_exposed...
  note: provenance::with_exposed: done in <time>, ran 1 branch
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
Distinct allocations get distinct base addresses, so they can never alias
  $ soteria-rust exec distinct_allocs.rs --stats stats.json && check_stat stats.json decayed_pointers 0
  Compiling... done in <time>
  => Running distinct_allocs::distinct_allocs_dont_alias...
  note: distinct_allocs::distinct_allocs_dont_alias: done in <time>, ran 1 branch
  PC 1: empty
  
Check corner cases with permissive provenance, around transmutes
  $ soteria-rust exec provenance_transmute.rs --provenance permissive
  Compiling... done in <time>
  => Running provenance_transmute::addr_doesnt_expose...
  error: provenance_transmute::addr_doesnt_expose: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in provenance_transmute::addr_doesnt_expose
      --> $TESTCASE_ROOT/provenance_transmute.rs:9:9
    2 |  fn addr_doesnt_expose() {
      |  ----------------------- 1: Entry point
      .  
    9 |          *p_back = 1;
      |          ^^^^^^^^^^^ Memory store
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffd)
  
  => Running provenance_transmute::transmute_doesnt_restore_provenance...
  error: provenance_transmute::transmute_doesnt_restore_provenance: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in provenance_transmute::transmute_doesnt_restore_provenance
      --> $TESTCASE_ROOT/provenance_transmute.rs:22:9
   15 |  fn transmute_doesnt_restore_provenance() {
      |  ---------------------------------------- 1: Entry point
      .  
   22 |          *p_back = 1;
      |          ^^^^^^^^^^^ Memory store
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
  => Running dangling_ptrs::access_zst...
  note: dangling_ptrs::access_zst: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running dangling_ptrs::get_discriminant_zst...
  note: dangling_ptrs::get_discriminant_zst: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running dangling_ptrs::null_ptr_not_zst...
  error: dangling_ptrs::null_ptr_not_zst: found issues in <time>, errors in 1 branch (out of 1)
  error: Null dereference in dangling_ptrs::null_ptr_not_zst
      --> $TESTCASE_ROOT/dangling_ptrs.rs:63:30
   61 |  fn null_ptr_not_zst() {
      |  --------------------- 1: Entry point
   62 |      let ptr: *const u32 = std::ptr::null();
   63 |      let _val: u32 = unsafe { *ptr };
      |                               ^^^^ Memory load
  PC 1: empty
  
  => Running dangling_ptrs::dangling_ptr_not_zst...
  error: dangling_ptrs::dangling_ptr_not_zst: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in dangling_ptrs::dangling_ptr_not_zst
      --> $TESTCASE_ROOT/dangling_ptrs.rs:69:29
   67 |  fn dangling_ptr_not_zst() {
      |  ------------------------- 1: Entry point
   68 |      let ptr: *const u8 = 0xdeadbeef as *const u8;
   69 |      let _val: u8 = unsafe { *ptr };
      |                              ^^^^ Memory load
  PC 1: empty
  
  [1]

Test exposing function pointers
  $ soteria-rust exec expose_fn_ptr.rs
  Compiling... done in <time>
  => Running expose_fn_ptr::main...
  note: expose_fn_ptr::main: done in <time>, ran 1 branch
  PC 1: ((V|1| ++ 0x0) <u 0x7ffffffffffffffe) /\
        (0x0000000000000010 <=u (V|1| ++ 0x0))
  
Test thread local statics; the two warnings due to opaque functions are to be expected, as we do not run the test suite with a sysroot.
  $ soteria-rust exec thread_local.rs --target aarch64-apple-darwin
  Compiling... done in <time>
  => Running thread_local::pub_static_cell...
  note: thread_local::pub_static_cell: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running thread_local::static_ref_cell...
  warning: thread_local::static_ref_cell (<time>): an unsupported feature was reached
  Can't execute function std::sys::thread_local::destructors::list::register, try using a sysroot (--sysroot)
  
  tip: to get a sysroot, run
       cargo +nightly-2026-08-18 miri setup --print-sysroot
  
  This is tracked at https://github.com/soteria-tools/soteria/issues/322
  
  => Running thread_local::pub_static_from_const_expr...
  warning: thread_local::pub_static_from_const_expr (<time>): an unsupported feature was reached
  Can't execute function std::sys::thread_local::destructors::list::register, try using a sysroot (--sysroot)
  
  tip: to get a sysroot, run
       cargo +nightly-2026-08-18 miri setup --print-sysroot
  
  This is tracked at https://github.com/soteria-tools/soteria/issues/322
  
  [2]

This test must be run separtely on linux and macos as it yields different error messages.
  $ soteria-rust exec thread_local.rs --target x86_64-unknown-linux-gnu
  Compiling... done in <time>
  => Running thread_local::pub_static_cell...
  note: thread_local::pub_static_cell: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running thread_local::static_ref_cell...
  warning: thread_local::static_ref_cell (<time>): an unsupported feature was reached
  Can't execute function std::sys::thread_local::destructors::linux_like::register, try using a sysroot (--sysroot)
  
  tip: to get a sysroot, run
       cargo +nightly-2026-08-18 miri setup --print-sysroot
  
  This is tracked at https://github.com/soteria-tools/soteria/issues/322
  
  => Running thread_local::pub_static_from_const_expr...
  warning: thread_local::pub_static_from_const_expr (<time>): an unsupported feature was reached
  Can't execute function std::sys::thread_local::destructors::linux_like::register, try using a sysroot (--sysroot)
  
  tip: to get a sysroot, run
       cargo +nightly-2026-08-18 miri setup --print-sysroot
  
  This is tracked at https://github.com/soteria-tools/soteria/issues/322
  
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
      --> $TESTCASE_ROOT/fail_fast.rs:4:9
    1 |  fn main() {
      |  --------- 1: Entry point
      .  
    4 |          panic!("ok");
      |          ^^^^^^^^^^^^
      |          |
      |          Triggering operation
      |          2: Call trace
  PC 1: (0x01 == V|1|) /\ (0x01 == V|1|)
  
  [1]

Test recursive validity check for references; disabled
  $ soteria-rust exec ref_validity.rs --reference-to-invalid-memory=allow
  Compiling... done in <time>
  => Running ref_validity::test_uninit_ref...
  note: ref_validity::test_uninit_ref: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running ref_validity::test_dangling_ref...
  error: ref_validity::test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in ref_validity::test_dangling_ref
      --> $TESTCASE_ROOT/ref_validity.rs:17:38
   14 |  fn test_dangling_ref() {
      |  ---------------------- 1: Entry point
      .  
   17 |      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      |                                       ^^^^^^^^ Dangling check
  PC 1: empty
  
  => Running ref_validity::test_unaligned_ref...
  error: ref_validity::test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in ref_validity::test_unaligned_ref
      --> $TESTCASE_ROOT/ref_validity.rs:25:33
   22 |  fn test_unaligned_ref() {
      |  ----------------------- 1: Entry point
      .  
   25 |      let as_ref: &u64 = unsafe { &*as_ptr };
      |                                  ^^^^^^^^ Requires well-aligned pointer
  PC 1: ((V|1| ++ 0b00) <u 0x7ffffffffffffff7) /\
        (0x0000000000000004 <=u (V|1| ++ 0b00))
  
  [1]

Test recursive validity check for references; enabled
  $ soteria-rust exec ref_validity.rs --reference-to-invalid-memory=deny
  Compiling... done in <time>
  => Running ref_validity::test_uninit_ref...
  error: ref_validity::test_uninit_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Invalid reference: Uninitialized memory access in ref_validity::test_uninit_ref
      --> $TESTCASE_ROOT/ref_validity.rs:7:33
    4 |  fn test_uninit_ref() {
      |  -------------------- 1: Entry point
      .  
    7 |      let as_ref: &u32 = unsafe { &*as_ptr };
      |                                  ^^^^^^^^ Fake read
  PC 1: empty
  
  => Running ref_validity::test_dangling_ref...
  error: ref_validity::test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in ref_validity::test_dangling_ref
      --> $TESTCASE_ROOT/ref_validity.rs:17:38
   14 |  fn test_dangling_ref() {
      |  ---------------------- 1: Entry point
      .  
   17 |      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      |                                       ^^^^^^^^ Dangling check
  PC 1: empty
  
  => Running ref_validity::test_unaligned_ref...
  error: ref_validity::test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in ref_validity::test_unaligned_ref
      --> $TESTCASE_ROOT/ref_validity.rs:25:33
   22 |  fn test_unaligned_ref() {
      |  ----------------------- 1: Entry point
      .  
   25 |      let as_ref: &u64 = unsafe { &*as_ptr };
      |                                  ^^^^^^^^ Requires well-aligned pointer
  PC 1: ((V|1| ++ 0b00) <u 0x7ffffffffffffff7) /\
        (0x0000000000000004 <=u (V|1| ++ 0b00))
  
  [1]

Test recursive validity check for references; warn
  $ soteria-rust exec ref_validity.rs --reference-to-invalid-memory=warn
  Compiling... done in <time>
  => Running ref_validity::test_uninit_ref...
  warning: Invalid reference: Uninitialized memory access
      --> $TESTCASE_ROOT/ref_validity.rs:7:33
    4 |  fn test_uninit_ref() {
      |  -------------------- 1: Entry point
      .  
    7 |      let as_ref: &u32 = unsafe { &*as_ptr };
      |                                  ^^^^^^^^ Triggering operation
  note: ref_validity::test_uninit_ref: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running ref_validity::test_dangling_ref...
  error: ref_validity::test_dangling_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Dangling pointer in ref_validity::test_dangling_ref
      --> $TESTCASE_ROOT/ref_validity.rs:17:38
   14 |  fn test_dangling_ref() {
      |  ---------------------- 1: Entry point
      .  
   17 |      let as_ref: &[u32; 2] = unsafe { &*as_ptr };
      |                                       ^^^^^^^^ Dangling check
  PC 1: empty
  
  => Running ref_validity::test_unaligned_ref...
  error: ref_validity::test_unaligned_ref: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000008, received 0x0000000000000004 with offset 0x0000000000000000 in ref_validity::test_unaligned_ref
      --> $TESTCASE_ROOT/ref_validity.rs:25:33
   22 |  fn test_unaligned_ref() {
      |  ----------------------- 1: Entry point
      .  
   25 |      let as_ref: &u64 = unsafe { &*as_ptr };
      |                                  ^^^^^^^^ Requires well-aligned pointer
  PC 1: ((V|1| ++ 0b00) <u 0x7ffffffffffffff7) /\
        (0x0000000000000004 <=u (V|1| ++ 0b00))
  
  [1]

Test exactly-evaluated float operations, at every precision
  $ soteria-rust exec float_ops.rs --stats stdout
  Compiling... done in <time>
  => Running float_ops::arithmetic...
  note: float_ops::arithmetic: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::precision...
  note: float_ops::precision: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::classification...
  note: float_ops::classification: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::min_max...
  note: float_ops::min_max: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::signed_zeros...
  note: float_ops::signed_zeros: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::comparisons...
  note: float_ops::comparisons: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::rounding...
  note: float_ops::rounding: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::square_root...
  note: float_ops::square_root: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::bit_patterns...
  note: float_ops::bit_patterns: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::int_conversions...
  note: float_ops::int_conversions: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::saturating_int_casts...
  note: float_ops::saturating_int_casts: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::float_casts...
  note: float_ops::float_casts: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running float_ops::symbolic...
  note: float_ops::symbolic: done in <time>, ran 1 branch
  PC 1: (1f <. V|1|) /\ (V|1| <. 2f) /\ (2f <. (V|1| +. V|1|)) /\
        ((V|1| +. V|1|) <. 4f) /\ (0f <. V|2|) /\ fis(NaN)(V|3|) /\
        !((V|3| <. 0f)) /\ (V|4| <. 0f) /\ fis(NaN)(sqrt.(V|4|)) /\
        (0f <. V|5|) /\ (V|5| <. 3f) /\
        ((((fround(NearestTiesToEven)((V|5| /. 3f)) /. 2f) ==. fround(NearestTiesToEven)((fround(NearestTiesToEven)((V|5| /. 3f)) /. 2f))) && (3f == (2f *. abs.(Fma(-3f,
       fround(NearestTiesToEven)((V|5| /. 3f)),
       V|5|))))) || ((2f *. abs.(Fma(-3f,
       fround(NearestTiesToEven)((V|5| /. 3f)), V|5|))) <. 3f)) /\
        (V|5| ==. ((fisneg(V|5|) == fisneg(Fma(-3f,
       fround(NearestTiesToEven)((V|5| /. 3f)), V|5|))) ? Fma(-3f,
       fround(NearestTiesToEven)((V|5| /. 3f)), V|5|) : (Fma(-3f,
       fround(NearestTiesToEven)((V|5| /. 3f)),
       V|5|) +. (fisneg(V|5|) ? -3f : 3f)))) /\ (1f <. V|6|) /\
        !(fis(Subnormal)(V|6|)) /\ !(fis(NaN)(V|6|)) /\ (2f == V|2|) /\
        (4f == (V|2| *. V|2|))
  
  => Running float_ops::symbolic_casts...
  note: float_ops::symbolic_casts: done in <time>, ran 1 branch
  PC 1: fis(NaN)(V|1|) /\ fis(Infinite)(V|2|) /\ (0f <. V|2|) /\
        ((2.14748365E+9f <=. V|2|) || (0x7fffffff == f2sbv[Truncate,32](V|2|))) /\
        !((V|2| <=. -2.14748365E+9f)) /\ !(fis(NaN)(V|2|)) /\
        ((256f <=. V|2|) || (0xff == f2ubv[Truncate,8](V|2|))) /\
        !((V|2| <=. 0f)) /\ (V|3| <. -1.0E+30f) /\ !(fis(NaN)(V|3|)) /\
        ((V|3| <=. -32768f) || ((0x8000 == f2sbv[Truncate,16](V|3|)) && !((32768f <=. V|3|)))) /\
        ((V|3| <=. 0f) || ((0x00000000 == f2ubv[Truncate,32](V|3|)) && !((4294967296f <=. V|3|)))) /\
        !(fis(Infinite)(V|4|)) /\ !(fis(NaN)(V|4|)) /\
        (V|4| ==. f2f[NearestTiesToEven,F32](f2f[NearestTiesToEven,F64](V|4|))) /\
        (fis(NaN)(V|5|) || fisneg(V|5|)) /\ fis(Zero)(V|5|) /\
        (0x8000000000000000 == V|6|) /\ (0x8000000000000000 == V|6|) /\
        (V|5| == bv2f[F64](V|6|))
  
  Statistics:
  • Z3 check-sat calls: 26
  • branch_on: branches 1.03% of calls (16 of 1551)
  • Execution time: <time>
  • Steps: 529
  • Function calls: 238
  • Load accesses: 717 (91.35% through store)
  • Allocations: 50
  • SAT checks: 50 (0 unknowns)
  • SAT solving time: <time> (<%>)
  • Branches: 16 (0 unexplored)
  

Test approximation of complex float operations -- warn (default)
  $ soteria-rust exec approx_float.rs
  Compiling... done in <time>
  => Running approx_float::main...
  warning: A complex floating point intrinsic was encountered; it will be executed with a significant over-approximation.
  note: approx_float::main: done in <time>, ran 1 branch
  PC 1: !(fis(Infinite)(V|1|)) /\ !(fis(NaN)(V|1|)) /\
        ((V|2| == 1f) || !(fis(Zero)(V|1|))) /\ (-1f <=. V|2|) /\ (V|2| <=. 1f)
  

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
        ((V|2| == 1f) || !(fis(Zero)(V|1|))) /\ (-1f <=. V|2|) /\ (V|2| <=. 1f)
  
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
  PC 1: empty
  
  digraph callgraph {
    node [shape=box fontname="monospace"];
    n0 [label="range::next" tooltip="core::iter::range::{impl Iterator for Range::<_>}::next::<i32>"];
    n12 [label="impls::lt" tooltip="core::cmp::impls::impl_PartialOrd_i32_for_i32::lt"];
    n15 [label="stdio::_print" tooltip="std::io::stdio::_print"];
    n13 [label="range::forward_unchecked" tooltip="core::iter::range::impl_Step_for_i32::forward_unchecked"];
    n7 [label="callgraph::limit" tooltip="callgraph::limit"];
    n2 [label="callgraph::choose" tooltip="callgraph::choose"];
    n16 [label="rt::new_display" tooltip="core::fmt::rt::{Argument::<'_>}::new_display::<'_, i32>"];
    n14 [label="callgraph::main" tooltip="callgraph::main"];
    n6 [label="callgraph::run" tooltip="callgraph::run"];
    n8 [label="collect::into_iter" tooltip="core::iter::traits::collect::impl_IntoIterator_for_T::into_iter::<Range::<i32>>"];
    n3 [label="callgraph::twice" tooltip="callgraph::twice"];
    n4 [label="callgraph::dec" tooltip="callgraph::dec"];
    n9 [label="callgraph::score" tooltip="callgraph::score"];
    n10 [label="callgraph::ping" tooltip="callgraph::ping"];
    n17 [label="fmt::new" tooltip="core::fmt::{Arguments::<'_>}::new::<'_, 12usize, 1usize>"];
    n11 [label="callgraph::pong" tooltip="callgraph::pong"];
    n5 [label="callgraph::inc" tooltip="callgraph::inc"];
    n1 [label="range::spec_next" tooltip="core::iter::range::{impl RangeIteratorImpl for Range::<_>}::spec_next::<i32>"];
    n0 -> n1;
    n2 -> n3;
    n2 -> n4;
    n2 -> n5;
    n6 -> n0;
    n6 -> n7;
    n6 -> n8;
    n6 -> n9;
    n10 -> n11;
    n1 -> n12;
    n1 -> n13;
    n14 -> n15;
    n14 -> n16;
    n14 -> n6;
    n14 -> n17;
    n9 -> n2;
    n9 -> n10;
    n11 -> n10;
  }

Check we trust addresses for pointer alignment
  $ soteria-rust exec assumed_align.rs
  Compiling... done in <time>
  => Running assumed_align::main...
  note: assumed_align::main: done in <time>, ran 2 branches
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffb) /\
        (0b0 == extract[0-0](V|1|))
  PC 2: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffffb) /\
        (0b1 == extract[0-0](V|1|))
  
Check that nondet_raw for unions work
  $ soteria-rust exec union_nondet.rs
  Compiling... done in <time>
  => Running union_nondet::read_d0...
  note: union_nondet::read_d0: done in <time>, ran 1 branch
  PC 1: empty
  
Check we handle pattern types correctly
  $ soteria-rust exec pattern_types.rs
  Compiling... done in <time>
  => Running pattern_types::nonnull_ok...
  note: pattern_types::nonnull_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running pattern_types::nonnull...
  error: pattern_types::nonnull: found issues in <time>, errors in 1 branch (out of 2)
  bug: UB: Transmute: Value violates pattern type constraint in pattern_types::nonnull
      --> $RUSTLIB/library/core/src/ptr/non_null.rs:247:13
  247 |              transmute(ptr)
      |              ^^^^^^^^^^^^^^ Transmute
      --> $TESTCASE_ROOT/pattern_types.rs:12:25
   10 |  fn nonnull() {
      |  ------------ 1: Entry point
   11 |      let x: usize = soteria::nondet_bytes();
   12 |      let _ptr = unsafe { NonNull::new_unchecked(x as *mut i32) };
      |                          ------------------------------------- 2: Call trace
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  
  => Running pattern_types::nonzero_ok...
  note: pattern_types::nonzero_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running pattern_types::nonzero_isize...
  error: pattern_types::nonzero_isize: found issues in <time>, errors in 1 branch (out of 2)
  bug: UB: Transmute: Value violates pattern type constraint in pattern_types::nonzero_isize
      --> $TESTCASE_ROOT/pattern_types.rs:25:24
   21 |  fn nonzero_isize() {
      |  ------------------ 1: Entry point
      .  
   25 |      let _nz = unsafe { std::mem::transmute::<isize, NonZero<isize>>(x) };
      |                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Transmute
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  
  [1]

Test that it's UB to write to a const (regardless of aliasing checks), and that we don't detect const refs as leaks
  $ soteria-rust exec write_to_const.rs  --ignore-aliasing
  Compiling... done in <time>
  => Running write_to_const::write_to_const...
  error: write_to_const::write_to_const: found issues in <time>, errors in 1 branch (out of 1)
  bug: Write to read-only location in write_to_const::write_to_const
      --> $TESTCASE_ROOT/write_to_const.rs:6:14
    4 |  fn write_to_const() {
      |  ------------------- 1: Entry point
    5 |      let ptr = REF as *const u8 as *mut u8;
    6 |      unsafe { *ptr = 67 };
      |               ^^^^^^^^^ Memory store
  PC 1: empty
  
  => Running write_to_const::write_to_str...
  error: write_to_const::write_to_str: found issues in <time>, errors in 1 branch (out of 1)
  bug: Write to read-only location in write_to_const::write_to_str
      --> $TESTCASE_ROOT/write_to_const.rs:12:14
   10 |  fn write_to_str() {
      |  ----------------- 1: Entry point
   11 |      let ptr = "hello" as *const str as *mut u8;
   12 |      unsafe { *ptr = 67 };
      |               ^^^^^^^^^ Memory store
  PC 1: empty
  
  [1]

Ensure we implement the caller_location intrinsic correctly; this used to cause a null pointer deref, rather than a proper panic from the handler.
  $ soteria-rust exec unreachable.rs
  Compiling... done in <time>
  => Running unreachable::main...
  error: unreachable::main: found issues in <time>, errors in 1 branch (out of 1)
  error: Panic in unreachable::main
      --> $RUSTLIB/library/core/src/panicking.rs:240:5
  240 |      panic_fmt(format_args!("internal error: entered unreachable code: {}", *x));
      |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      3: Call trace
      --> $TESTCASE_ROOT/unreachable.rs:2:5
    1 |  fn main() {
      |  --------- 1: Entry point
    2 |      unreachable!("This should not be a null pointer deref!");
      |      -------------------------------------------------------- 2: Call trace
  PC 1: empty
  
  [1]

Boolean BitOr must not be assumed true; both operands can be false (issue #376).
  $ soteria-rust exec bool_or.rs
  Compiling... done in <time>
  => Running bool_or::main...
  error: bool_or::main: found issues in <time>, errors in 1 branch (out of 2)
  error: Panic: assertion failed: output in bool_or::main
      --> $TESTCASE_ROOT/bool_or.rs:5:5
    1 |  fn main() {
      |  --------- 1: Entry point
      .  
    5 |      assert!(output);
      |      ^^^^^^^^^^^^^^^
      |      |
      |      Triggering operation
      |      2: Call trace
  PC 1: (0x00 == V|1|) /\ (0x00 == V|2|) /\ (0x00 == V|1|) /\ (0x00 == V|2|)
  
  [1]

Test that allocating a box only requires two heap allocation (thanks to the store optimisation): one for the contents of the box, and one for the box that we pass to the drop glue.
FIXME: now that named consts are globals, there is in fact a third allocation: the one for <i32 as SizedTypeProperties>::LAYOUT. We should extend the store optimisation to handle globals; in particular we have a guarantee they can't be written to, so it's likely the optimisation will perform really well.
  $ soteria-rust exec box.rs --stats stats.json && check_stat stats.json allocs 2
  Compiling... done in <time>
  => Running box::main...
  note: box::main: done in <time>, ran 1 branch
  PC 1: empty
  
  check_stat: expected '2', got '3' for allocs
  [1]

Test that taking a reference to a ZST doesn't allocate it on the heap; the reference is a dangling pointer, so the value stays in the store.
  $ soteria-rust exec zst_ref.rs --stats stats.json && check_stat stats.json allocs 0
  Compiling... done in <time>
  => Running zst_ref::main...
  note: zst_ref::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test that indexing arrays with a constant index does not allocate; the value is updated in place in the store.
  $ soteria-rust exec store_struct.rs --stats stats.json && check_stat stats.json allocs 0
  Compiling... done in <time>
  => Running store_struct::main...
  note: store_struct::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test that reading the metadata of a store-hosted pointer does not allocate; the pointer stays in the store.
  $ soteria-rust exec ptr_metadata.rs --stats stats.json && check_stat stats.json allocs 0
  Compiling... done in <time>
  => Running ptr_metadata::main...
  note: ptr_metadata::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test we can use ptr::metadata to get the metadata of a trait object; this used to crash
  $ soteria-rust exec ptr_dyn_metadata.rs
  Compiling... done in <time>
  => Running ptr_dyn_metadata::main...
  note: ptr_dyn_metadata::main: done in <time>, ran 1 branch
  PC 1: empty
  
  $ soteria-rust exec nonnull.rs --stats stats.json && check_stat stats.json decayed_pointers 0
  Compiling... done in <time>
  => Running nonnull::match_niched_enums...
  note: nonnull::match_niched_enums: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running nonnull::null_is_none...
  note: nonnull::null_is_none: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running nonnull::niche_ok...
  note: nonnull::niche_ok: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running nonnull::niche_err...
  note: nonnull::niche_err: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running nonnull::transmuted_discriminant...
  note: nonnull::transmuted_discriminant: done in <time>, ran 2 branches
  PC 1: (0x0000000000000000 == V|1|) /\ (0x0000000000000000 == V|1|)
  PC 2: (0x0000000000000001 <=u V|1|)
  
  $ soteria-rust exec btreeset_small.rs --stats stats.json && check_stat stats.json decayed_pointers 0
  Compiling... done in <time>
  => Running btreeset_small::test_treeset_is_ordered...
  note: btreeset_small::test_treeset_is_ordered: done in <time>, ran 3 branches
  PC 1: (V|2| <u V|1|)
  PC 2: (V|1| <=u V|2|) /\ (V|1| != V|2|)
  PC 3: (V|1| <=u V|2|) /\ (V|1| == V|2|)
  

Pointers are compared by their addresses. Two pointer with equal addresses
but different provenance should be decayed, compared, and checked to be equal
successfuly.
  $ soteria-rust exec ptr_diff_prov.rs
  Compiling... done in <time>
  => Running ptr_diff_prov::diff_prov_same_address...
  note: ptr_diff_prov::diff_prov_same_address: done in <time>, ran 1 branch
  PC 1: Distinct(V|1-2|) /\ (0x0000000000000001 <=u V|1|) /\
        (V|1| <=u 0x7ffffffffffffffd) /\ (0x0000000000000001 <=u V|2|) /\
        (V|2| <=u 0x7ffffffffffffffd)
  
  => Running ptr_diff_prov::one_prov_one_no_prove_same_address...
  note: ptr_diff_prov::one_prov_one_no_prove_same_address: done in <time>, ran 1 branch
  PC 1: ((V|1| ++ 0b000) <u 0x7ffffffffffffff7) /\
        (0x0000000000000008 <=u (V|1| ++ 0b000))
  
Test calls to FnOnce trait objects.
  $ soteria-rust exec box_fnonce.rs
  Compiling... done in <time>
  => Running box_fnonce::main...
  note: box_fnonce::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test the atomic read-modify-write intrinsics (fetch_and/or/xor/nand/sub/min/max), on both integers and pointers.
  $ soteria-rust exec atomics.rs
  Compiling... done in <time>
  => Running atomics::bitwise...
  warning: An atomic intrinsic was encountered; it will be executed as sequential code
  note: atomics::bitwise: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running atomics::min_max_sub...
  note: atomics::min_max_sub: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running atomics::pointer...
  note: atomics::pointer: done in <time>, ran 1 branch
  PC 1: empty
  
Test a pointer constant into a larger, over-aligned allocation (à la hashbrown's
Group::static_empty); the allocation must keep its full size and alignment.
  $ soteria-rust exec oversized_const_alloc.rs
  Compiling... done in <time>
  => Running oversized_const_alloc::main...
  note: oversized_const_alloc::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test a field access through a pointer derived from ptr.sub with a symbolic index; used to error
  $ soteria-rust exec ptr_sub_field.rs
  Compiling... done in <time>
  => Running ptr_sub_field::main...
  note: ptr_sub_field::main: done in <time>, ran 4 branches
  PC 1: !((0x0000000000000000 -s_ovf (0x0000000000000001 +cku (V|1| & 0x0000000000000003)))) /\
        (0x0000000000000002 <=u ((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009)) /\
        (0x0000000000000004 <=u ((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009)) /\
        (0x0000000000000006 <=u ((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009)) /\
        (((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009) <u 0x0000000000000008) /\
        (0x0000000000000000 == (V|1| & 0x0000000000000003))
  PC 2: !((0x0000000000000000 -s_ovf (0x0000000000000001 +cku (V|1| & 0x0000000000000003)))) /\
        (0x0000000000000000 != (V|1| & 0x0000000000000003)) /\
        (((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009) <u 0x0000000000000002) /\
        (0x0000000000000003 <=u (V|1| & 0x0000000000000003)) /\
        (0b11 == extract[0-1](V|1|))
  PC 3: !((0x0000000000000000 -s_ovf (0x0000000000000001 +cku (V|1| & 0x0000000000000003)))) /\
        (0x0000000000000000 != (V|1| & 0x0000000000000003)) /\
        (0x0000000000000002 <=u ((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009)) /\
        (((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009) <u 0x0000000000000004) /\
        (0x0000000000000002 <=u (V|1| & 0x0000000000000003)) /\
        (extract[0-1](V|1|) == 0b10)
  PC 4: !((0x0000000000000000 -s_ovf (0x0000000000000001 +cku (V|1| & 0x0000000000000003)))) /\
        (0x0000000000000000 != (V|1| & 0x0000000000000003)) /\
        (0x0000000000000002 <=u ((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009)) /\
        (0x0000000000000004 <=u ((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009)) /\
        (((0xfffffffffffffffe *cks (0x0000000000000001 +cku (V|1| & 0x0000000000000003))) +cks 0x0000000000000009) <u 0x0000000000000006) /\
        (0x0000000000000001 <=u (V|1| & 0x0000000000000003)) /\
        (0b01 == extract[0-1](V|1|))
  
Test the SIMD intrinsics used by hashbrown's NEON control group.
  $ soteria-rust exec simd.rs --target aarch64-apple-darwin
  Compiling... done in <time>
  => Running simd::main...
  note: simd::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test a HashMap with concrete keys (insert/get/len) and a symbolic key. The
stubbed (constant) hasher keeps bucket indices concrete even for the symbolic
key, so the only branch is its equality with the concrete entry.
  $ soteria-rust exec hashmap.rs --target aarch64-apple-darwin
  Compiling... done in <time>
  => Running hashmap::main...
  warning: std::sys::random::hashmap_random_keys was stubbed to constant random keys, to avoid path explosion. This is an under-approximation, some paths may be missed.
  warning: std::hash::BuildHasher::hash_one was stubbed to always hash to 0, to avoid path explosion. This is an under-approximation, some paths may be missed.
  note: hashmap::main: done in <time>, ran 2 branches
  PC 1: (0x00000007 == V|1|) /\ (0x00000007 == V|1|)
  PC 2: (0x00000007 != V|1|)
  
Test destructors run when reached through `ptr::drop_in_place`.
  $ soteria-rust exec drop_in_place.rs
  Compiling... done in <time>
  => Running drop_in_place::main...
  note: drop_in_place::main: done in <time>, ran 1 branch
  PC 1: empty
  
Test that a place must be aligned for the type of the pointer it was created
from, rather than for the type of the field being accessed.
  $ soteria-rust exec place_align.rs
  Compiling... done in <time>
  => Running place_align::access_packed_field...
  note: place_align::access_packed_field: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running place_align::deref_ptr_to_packed_field...
  error: place_align::deref_ptr_to_packed_field: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000004, received 0x0000000000000001 with offset 0x0000000000000001 in place_align::deref_ptr_to_packed_field
      --> $TESTCASE_ROOT/place_align.rs:30:22
   27 |  fn deref_ptr_to_packed_field() {
      |  ------------------------------ 1: Entry point
      .  
   30 |      let _ = unsafe { *q };
      |                       ^^ Requires well-aligned pointer
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7ffffffffffffff9)
  
  => Running place_align::access_field_of_misaligned_struct...
  error: place_align::access_field_of_misaligned_struct: found issues in <time>, errors in 1 branch (out of 1)
  bug: Misaligned pointer; expected 0x0000000000000004, received 0x0000000000000008 with offset 0x0000000000000001 in place_align::access_field_of_misaligned_struct
      --> $TESTCASE_ROOT/place_align.rs:39:22
   36 |  fn access_field_of_misaligned_struct() {
      |  -------------------------------------- 1: Entry point
      .  
   39 |      let _ = unsafe { (*p).fill };
      |                       ^^^^^^^^^ Requires well-aligned pointer
  PC 1: ((V|1| ++ 0b000) <u 0x7fffffffffffffdf) /\
        (0x0000000000000008 <=u (V|1| ++ 0b000))
  
  => Running place_align::addr_of_field_of_misaligned_struct...
  note: place_align::addr_of_field_of_misaligned_struct: done in <time>, ran 1 branch
  PC 1: empty
  
  [1]

Test unsizing a Box with a non-ZST allocator
  $ soteria-rust exec unsize_custom_alloc.rs
  Compiling... done in <time>
  => Running unsize_custom_alloc::unsize_array_with_a_non_zst_allocator...
  note: unsize_custom_alloc::unsize_array_with_a_non_zst_allocator: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running unsize_custom_alloc::upcast_with_a_pointer_allocator...
  note: unsize_custom_alloc::upcast_with_a_pointer_allocator: done in <time>, ran 1 branch
  PC 1: empty
  
Test TypeId equality and downcasting a `dyn Any`
  $ soteria-rust exec type_id.rs
  Compiling... done in <time>
  => Running type_id::main...
  note: type_id::main: done in <time>, ran 1 branch
  PC 1: Distinct(V|1-2|) /\ (V|1| != V|2|) /\ Distinct(V|1-3|) /\
        (V|1| != V|3|) /\ Distinct(V|1-4|) /\ Distinct(V|1-5|) /\
        (V|4| != V|5|)
  
Test that `align_offset` is answered from the allocation's alignment
  $ soteria-rust exec align_offset.rs
  Compiling... done in <time>
  => Running align_offset::offset_within_the_allocation_alignment...
  note: align_offset::offset_within_the_allocation_alignment: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running align_offset::offset_beyond_the_allocation_alignment...
  warning: std::ptr::align_offset was stubbed to avoid path explosion. This is an under-approximation, some paths may be missed.
  note: align_offset::offset_beyond_the_allocation_alignment: done in <time>, ran 1 branch
  PC 1: ((V|1| ++ 0b00) <u 0x7fffffffffffffef) /\
        (0x0000000000000004 <=u (V|1| ++ 0b00)) /\ (0b0 == extract[0-0](V|1|))
  
  => Running align_offset::offset_asked_twice_for_one_allocation...
  note: align_offset::offset_asked_twice_for_one_allocation: done in <time>, ran 1 branch
  PC 1: (0x0000000000000001 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffbe) /\
        (0b000 == extract[0-2](V|1|))
  
  => Running align_offset::offset_for_a_known_address...
  note: align_offset::offset_for_a_known_address: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running align_offset::offset_wrapping_around_the_alignment...
  note: align_offset::offset_wrapping_around_the_alignment: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running align_offset::offset_that_cannot_be_given...
  note: align_offset::offset_that_cannot_be_given: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running align_offset::alignment_is_not_a_power_of_two...
  error: align_offset::alignment_is_not_a_power_of_two: found issues in <time>, errors in 1 branch (out of 1)
  error: Panic in align_offset::alignment_is_not_a_power_of_two
       --> $RUSTLIB/library/core/src/ptr/const_ptr.rs:1282:13
  1282 |              panic!("align_offset: align is not a power-of-two");
       |              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       |              |
       |              Triggering operation
       |              3: Call trace
       --> $TESTCASE_ROOT/align_offset.rs:82:13
    80 |  fn alignment_is_not_a_power_of_two() {
       |  ------------------------------------ 1: Entry point
    81 |      let arr = [0u8; 8];
    82 |      let _ = arr.as_ptr().align_offset(std::hint::black_box(3));
       |              -------------------------------------------------- 2: Call trace
  PC 1: empty
  
  [1]
