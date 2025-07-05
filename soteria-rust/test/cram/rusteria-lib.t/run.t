Test base functions
  $ soteria-rust rustc lib-fns.rs --clean
  note: Done, no errors found
  
  lib_fns::main: ran 1 branch
  PC 1: (0 != V|1|) /\ (1 == V|1|)

Test #[soteria::*] annotations
  $ soteria-rust rustc annots.rs --clean
  note: Done, no errors found
  
  annots::test1: ran 1 branch
  PC 1: true
  
  annots::test2: ran 1 branch
  PC 1: true
  
  annots::test3: ran 1 branch
  PC 1: true
  
  annots::test_branch_fuel: ran 10 branches
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
  
  annots::test_step_fuel: ran 1 branch
  PC 1: true
