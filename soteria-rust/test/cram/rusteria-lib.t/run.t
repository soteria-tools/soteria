Test base functions
  $ soteria-rust rustc lib-fns.rs --clean --no-timing
  Compiling... done in <time>
  note: lib_fns::main: done in <time>, ran 1 branch
  PC 1: (1 == V|1|)
  


Test #[soteria::*] annotations
  $ soteria-rust rustc annots.rs --clean --no-timing
  Compiling... done in <time>
  note: annots::test1: done in <time>, ran 1 branch
  PC 1: true
  
  note: annots::test2: done in <time>, ran 1 branch
  PC 1: true
  
  note: annots::test3: done in <time>, ran 1 branch
  PC 1: true
  
  note: annots::test_branch_fuel: done in <time>, ran 10 branches
  PC 1: (0 == V|1|)
  PC 2: (1 == V|1|)
  PC 3: (V|1| == 2)
  PC 4: (V|1| == 3)
  PC 5: (V|1| == 4)
  PC 6: (V|1| == 5)
  PC 7: (V|1| == 6)
  PC 8: (V|1| == 7)
  PC 9: (V|1| == 8)
  PC 10: (V|1| == 9)
  
  note: annots::test_step_fuel: done in <time>, ran 1 branch
  PC 1: true
  
  note: annots::test_expect_fail: done in <time>, ran 1 branch
  PC 1: true
  
