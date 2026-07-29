Test base functions
  $ soteria-rust exec lib-fns.rs
  Compiling... done in <time>
  => Running lib_fns::main...
  note: lib_fns::main: done in <time>, ran 1 branch
  PC 1: (0x01 == V|1|) /\ (0x01 == V|1|)
  

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
  PC 1: (V|1| == 0x09) /\ (V|1| == 0x09)
  PC 2: (V|1| == 0x08) /\ (V|1| == 0x08)
  PC 3: (V|1| == 0x07) /\ (V|1| == 0x07)
  PC 4: (V|1| == 0x06) /\ (V|1| == 0x06)
  PC 5: (V|1| == 0x05) /\ (V|1| == 0x05)
  PC 6: (V|1| == 0x04) /\ (V|1| == 0x04)
  PC 7: (V|1| == 0x03) /\ (V|1| == 0x03)
  PC 8: (V|1| == 0x02) /\ (V|1| == 0x02)
  PC 9: (0x01 == V|1|) /\ (0x01 == V|1|)
  PC 10: (0x00 == V|1|) /\ (0x00 == V|1|)
  
  => Running annots::test_step_fuel...
  note: annots::test_step_fuel: done in <time>, ran 1 branch
  PC 1: empty
  
  => Running annots::test_expect_fail...
  note: annots::test_expect_fail: done in <time>, ran 1 branch
  PC 1: empty
  
Test branching on nondet enums: we want to avoid branching unless necessary (when comparing the discriminant, or accessing variant fields)
  $ soteria-rust exec nondet_branching.rs
  Compiling... done in <time>
  => Running nondet_branching::scalar_enum...
  note: nondet_branching::scalar_enum: done in <time>, ran 1 branch
  PC 1: ((((V|3|.as<0>.0 <u 0x0000d800) || (0x0000dfff <u V|3|.as<0>.0)) && (V|3|.as<0>.0 <=u 0x0010ffff)) || !(V|3|.is<0>)) /\
        ((V|3|.as<1>.0 <=u 0x01) || !(V|3|.is<1>)) /\ (V|2| <=u 0x01)
  
  => Running nondet_branching::enum_large...
  note: nondet_branching::enum_large: done in <time>, ran 10 branches
  PC 1: V|1|.is<0>
  PC 2: !(V|1|.is<0>) /\ V|1|.is<1>
  PC 3: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ V|1|.is<2>
  PC 4: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ !(V|1|.is<2>) /\ V|1|.is<3>
  PC 5: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ !(V|1|.is<2>) /\ !(V|1|.is<3>) /\
        V|1|.is<4>
  PC 6: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ !(V|1|.is<2>) /\ !(V|1|.is<3>) /\
        !(V|1|.is<4>) /\ V|1|.is<5>
  PC 7: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ !(V|1|.is<2>) /\ !(V|1|.is<3>) /\
        !(V|1|.is<4>) /\ !(V|1|.is<5>) /\ V|1|.is<6>
  PC 8: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ !(V|1|.is<2>) /\ !(V|1|.is<3>) /\
        !(V|1|.is<4>) /\ !(V|1|.is<5>) /\ !(V|1|.is<6>) /\ V|1|.is<7>
  PC 9: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ !(V|1|.is<2>) /\ !(V|1|.is<3>) /\
        !(V|1|.is<4>) /\ !(V|1|.is<5>) /\ !(V|1|.is<6>) /\ !(V|1|.is<7>) /\
        V|1|.is<8>
  PC 10: !(V|1|.is<0>) /\ !(V|1|.is<1>) /\ !(V|1|.is<2>) /\ !(V|1|.is<3>) /\
         !(V|1|.is<4>) /\ !(V|1|.is<5>) /\ !(V|1|.is<6>) /\ !(V|1|.is<7>) /\
         !(V|1|.is<8>) /\ V|1|.is<9>
  
