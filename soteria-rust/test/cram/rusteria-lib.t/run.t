Test base functions
  $ soteria-rust exec-main lib-fns.rs --clean
  note: Done, no errors found
  
  lib_fns::main: ran 1 branch
  PC 1: (0 != V|1|) /\ (V|1| <= 1) /\ (0 <= V|1|)

Test #[soteria::*] annotations
  $ soteria-rust exec-main annots.rs --clean
  note: Done, no errors found
  
  annots::test1: ran 1 branch
  PC 1: empty
  
  annots::test2: ran 1 branch
  PC 1: empty
  
  annots::test3: ran 1 branch
  PC 1: empty
  
  annots::test_branch_fuel: ran 10 branches
  PC 1: (V|1| <= 0) /\ (V|1| < 10) /\ (V|1| <= 255) /\ (0 <= V|1|)
  PC 2: (V|1| <= 1) /\ (0 < V|1|) /\ (V|1| < 10) /\ (V|1| <= 255) /\
        (0 <= V|1|)
  PC 3: (V|1| <= 2) /\ (1 < V|1|) /\ (0 < V|1|) /\ (V|1| < 10) /\
        (V|1| <= 255) /\ (0 <= V|1|)
  PC 4: (V|1| <= 3) /\ (2 < V|1|) /\ (1 < V|1|) /\ (0 < V|1|) /\ (V|1| < 10) /\
        (V|1| <= 255) /\ (0 <= V|1|)
  PC 5: (V|1| <= 4) /\ (3 < V|1|) /\ (2 < V|1|) /\ (1 < V|1|) /\ (0 < V|1|) /\
        (V|1| < 10) /\ (V|1| <= 255) /\ (0 <= V|1|)
  PC 6: (V|1| <= 5) /\ (4 < V|1|) /\ (3 < V|1|) /\ (2 < V|1|) /\ (1 < V|1|) /\
        (0 < V|1|) /\ (V|1| < 10) /\ (V|1| <= 255) /\ (0 <= V|1|)
  PC 7: (V|1| <= 6) /\ (5 < V|1|) /\ (4 < V|1|) /\ (3 < V|1|) /\ (2 < V|1|) /\
        (1 < V|1|) /\ (0 < V|1|) /\ (V|1| < 10) /\ (V|1| <= 255) /\ (0 <= V|1|)
  PC 8: (V|1| <= 7) /\ (6 < V|1|) /\ (5 < V|1|) /\ (4 < V|1|) /\ (3 < V|1|) /\
        (2 < V|1|) /\ (1 < V|1|) /\ (0 < V|1|) /\ (V|1| < 10) /\
        (V|1| <= 255) /\ (0 <= V|1|)
  PC 9: (V|1| <= 8) /\ (7 < V|1|) /\ (6 < V|1|) /\ (5 < V|1|) /\ (4 < V|1|) /\
        (3 < V|1|) /\ (2 < V|1|) /\ (1 < V|1|) /\ (0 < V|1|) /\ (V|1| < 10) /\
        (V|1| <= 255) /\ (0 <= V|1|)
  PC 10: (V|1| <= 9) /\ (8 < V|1|) /\ (7 < V|1|) /\ (6 < V|1|) /\ (5 < V|1|) /\
         (4 < V|1|) /\ (3 < V|1|) /\ (2 < V|1|) /\ (1 < V|1|) /\ (0 < V|1|) /\
         (V|1| < 10) /\ (V|1| <= 255) /\ (0 <= V|1|)
  
  annots::test_step_fuel: ran 1 branch
  PC 1: empty
