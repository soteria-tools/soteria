Test kani::any
  $ soteria-rust exec-main any.rs --clean
  Done. - Ran 5 branches
  PC: 
    (V|0| == 0) /\ (V|0| <= 1) /\ (0 <= V|0|)
  
  PC: 
    (V|0| != 0) /\ (V|0| <= 1) /\ (0 <= V|0|)
  
  PC: 
    (0 < V|0|) /\ (0 <= V|0|) /\ (V|0| != 0) /\ (V|0| <= 127) /\ (-128 <= V|0|)
  
  PC: 
    (V|0| < 0) /\ (V|0| != 0) /\ (V|0| <= 127) /\ (-128 <= V|0|)
  
  PC: 
    (V|0| == 0) /\ (V|0| <= 127) /\ (-128 <= V|0|)

Test kani::assume
  $ soteria-rust exec-main assume.rs --clean
  Done. - Ran 2 branches
  PC: 
    (V|0| != 0) /\ (V|0| <= 1) /\ (0 <= V|0|)
  
  PC: 
    ((11 / V|0|) <= 2147483647) /\ (-2147483648 <= (11 / V|0|)) /\
    (V|0| != 0) /\ (V|0| <= 2147483647) /\ (-2147483648 <= V|0|)

Test valid values
  $ soteria-rust exec-main should_panic.rs --clean
  Done. - Ran 1 branches
  PC: empty
