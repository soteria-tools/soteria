Basic code, reference gets invalidated
  $ soteria-rust exec raw-ptrs.rs
  Compiling... done in <time>
  => Running raw_ptrs::main...
  note: raw_ptrs::main: done in <time>, ran 1 branch
  PC 1: empty
  

Simple tree borrow violation
  $ soteria-rust exec simple-fail.rs
  Compiling... done in <time>
  => Running simple_fail::main...
  error: simple_fail::main: found issues in <time>, errors in 1 branch (out of 1)
  bug: Aliasing error in simple_fail::main
      --> $TESTCASE_ROOT/simple-fail.rs:8:5
    3 |  fn main() {
      |  --------- 1: Entry point
      .  
    8 |      *y = 20; // UB: y is disabled
      |      ^^^^^^^ Memory store
  PC 1: empty
  
  [1]

Raw pointers don't get new tags
  $ soteria-rust exec raw-ptrs.rs
  Compiling... done in <time>
  => Running raw_ptrs::main...
  note: raw_ptrs::main: done in <time>, ran 1 branch
  PC 1: empty
  

Raw pointers can access outside the parent's range, with offsets
  $ soteria-rust exec offsets.rs
  Compiling... done in <time>
  => Running offsets::main...
  note: offsets::main: done in <time>, ran 1 branch
  PC 1: empty
  

Can have two mutable protected refs to the same allocation, if they don't overlap
  $ soteria-rust exec two-mut-protected.rs
  Compiling... done in <time>
  => Running two_mut_protected::main...
  note: two_mut_protected::main: done in <time>, ran 1 branch
  PC 1: empty
  

UnsafeCell allow foreign writes followed by local writes
  $ soteria-rust exec cell.rs
  Compiling... done in <time>
  => Running cell::main...
  note: cell::main: done in <time>, ran 1 branch
  PC 1: empty
  

Deallocating memory that is strongly protected by an interior mutable reference is allowed
  $ soteria-rust exec cell-dealloc.rs
  Compiling... done in <time>
  => Running cell_dealloc::main...
  note: cell_dealloc::main: done in <time>, ran 1 branch
  PC 1: empty
  

Deallocating memory that is strongly protected by a mutable reference is not
  $ soteria-rust exec protected-dealloc.rs
  Compiling... done in <time>
  => Running protected_dealloc::main...
  error: protected_dealloc::main: found issues in <time>, errors in 1 branch (out of 1)
  bug: Tried freeing an allocation which was passed to a function by reference in protected_dealloc::main
       --> $RUSTLIB/library/alloc/src/alloc.rs:175:14
   175 |        unsafe { __rust_dealloc(ptr, layout.size(), layout.alignment()) }
       |                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       |                 |
       |                 Freeing memory
       |                 10: Call trace
       --> $RUSTLIB/library/alloc/src/boxed.rs:2000:17
  2000 |                    self.1.deallocate(From::from(ptr.cast()), layout);
       |                    ------------------------------------------------- 9: Call trace
       --> $RUSTLIB/library/core/src/mem/mod.rs:1049:1
  1049 |    }
       |    - 7: Call trace
       --> $RUSTLIB/library/core/src/ops/function.rs:250:5
   250 |        extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
       |        ------------------------------------------------------------------ 5: Call trace
       --> $RUSTLIB/library/core/src/ptr/mod.rs:847:1
   846 |    #[lang = "drop_glue"]
   847 | /  pub(crate) const unsafe fn drop_glue<T: PointeeSized>(_: &mut T)
   848 | |  where
   849 | |      T: [const] Destruct,
       | \-------------------------' 8: Call trace
   850 |    {
       --> $TESTCASE_ROOT/protected-dealloc.rs:9:9
     4 |        f(x)
       |        ---- 3: Call trace
       .    
     7 |    fn main() {
       |    --------- 1: Entry point
     8 | /      inner(Box::leak(Box::new(0)), |raw| {
       | |                                    ----- 4: Call trace
     9 | |          drop(unsafe { Box::from_raw(raw) });
       | |          ----------------------------------- 6: Call trace
    10 | |      });
       | \-------' 2: Call trace
    11 |    }
  PC 1: empty
  
  [1]

Nested UnsafeCells work too -- skipped for now, due to Charon changing the translation of IS_ZST
  $ soteria-rust exec nested.rs
  Compiling... done in <time>
  => Running nested::main...
  note: nested::main: done in <time>, ran 1 branch
  PC 1: empty
  

Test --ignore-aliasing flag
  $ soteria-rust exec simple-fail.rs --ignore-aliasing
  Compiling... done in <time>
  => Running simple_fail::main...
  note: simple_fail::main: done in <time>, ran 1 branch
  PC 1: empty
  
