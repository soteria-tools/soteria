Test base functions
  $ soteria-rust exec lib-fns.rs
  Compiling... done in <time>
  note: main: done in <time>, ran 1 branch
  PC 1: (V|1| == 0x01) /\ (V|1| == 0x01)
  

Test #[soteria::*] annotations
  $ soteria-rust exec annots.rs
  Compiling... done in <time>
  note: test1: done in <time>, ran 1 branch
  PC 1: empty
  
  note: test2: done in <time>, ran 1 branch
  PC 1: empty
  
  note: test3: done in <time>, ran 1 branch
  PC 1: empty
  
  note: test_branch_fuel: done in <time>, ran 10 branches
  PC 1: (V|1| == 0x09) /\ (V|1| == 0x09)
  PC 2: (V|1| == 0x08) /\ (V|1| == 0x08)
  PC 3: (V|1| == 0x07) /\ (V|1| == 0x07)
  PC 4: (V|1| == 0x06) /\ (V|1| == 0x06)
  PC 5: (V|1| == 0x05) /\ (V|1| == 0x05)
  PC 6: (V|1| == 0x04) /\ (V|1| == 0x04)
  PC 7: (V|1| == 0x03) /\ (V|1| == 0x03)
  PC 8: (V|1| == 0x02) /\ (V|1| == 0x02)
  PC 9: (V|1| == 0x01) /\ (V|1| == 0x01)
  PC 10: (V|1| == 0x00) /\ (V|1| == 0x00)
  
  note: test_step_fuel: done in <time>, ran 1 branch
  PC 1: empty
  
  note: test_expect_fail: done in <time>, ran 1 branch
  PC 1: empty
  
