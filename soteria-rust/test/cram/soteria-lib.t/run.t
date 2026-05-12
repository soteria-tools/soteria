Test base functions
  $ soteria-rust exec lib-fns.rs
  Compiling... done in <time>
  => Running lib_fns::main...
  note: lib_fns::main: done in <time>, ran 1 branch
  PC 1: (V|1| == 0x01)
  Variables:
    |1| — nondet bool, created at .../cram/soteria-lib.t/lib-fns.rs:<range>
  

Test #[soteria::*] annotations
  $ soteria-rust exec annots.rs
  Compiling... done in <time>
  => Running annots::test1...
  note: annots::test1: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running annots::test2...
  note: annots::test2: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running annots::test3...
  note: annots::test3: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running annots::test_branch_fuel...
  note: annots::test_branch_fuel: done in <time>, ran 10 branches
  PC 1: (V|1| == 0x09)
  PC 2: (V|1| == 0x08)
  PC 3: (V|1| == 0x07)
  PC 4: (V|1| == 0x06)
  PC 5: (V|1| == 0x05)
  PC 6: (V|1| == 0x04)
  PC 7: (V|1| == 0x03)
  PC 8: (V|1| == 0x02)
  PC 9: (V|1| == 0x01)
  PC 10: (V|1| == 0x00)
  Variables:
    |1| — nondet u8, created at .../cram/soteria-lib.t/annots.rs:<range>
  
  => Running annots::test_step_fuel...
  note: annots::test_step_fuel: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running annots::test_expect_fail...
  note: annots::test_expect_fail: done in <time>, ran 1 branch
  PC 1: empty
  
