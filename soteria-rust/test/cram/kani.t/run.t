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

Test #[kani::should_panic]
  $ soteria-rust exec-main should_panic.rs --clean
  Done. - Ran 1 branches
  PC: empty

Test kani::assert
  $ soteria-rust exec-main assert.rs --clean
  Error in 4 branchs:
  - Failed assertion: Expected true! with trace [($TESTCASE_ROOT/assert.rs:4:4-37,
                                                                      Call trace);
                                                                     ($TESTCASE_ROOT/assert.rs:4:4-37,
                                                                      Triggering memory operation)]
  
  - Failed assertion: 👻 unicode is 𝒮𝒞𝒜ℛ𝒴 with trace [($TESTCASE_ROOT/assert.rs:10:4-42,
                                                                      Call trace);
                                                                      ($TESTCASE_ROOT/assert.rs:10:4-42,
                                                                      Triggering memory operation)]
  
  - Failed assertion: "I used \"assert!\"" with trace [(/Users/opale/Documents/GitHub/soteria/_build/install/default/share/soteria-rust/kani_lib/std/src/lib.rs:45:8-60,
                                                        Call trace);
                                                       (/Users/opale/Documents/GitHub/soteria/_build/install/default/share/soteria-rust/kani_lib/std/src/lib.rs:45:8-60,
                                                        Triggering memory operation)]
  
  - Failed assertion: "I used \"assert_eq!\"" with trace [(/Users/opale/Documents/GitHub/soteria/_build/install/default/share/soteria-rust/kani_lib/std/src/lib.rs:45:8-60,
                                                           Call trace);
                                                          (/Users/opale/Documents/GitHub/soteria/_build/install/default/share/soteria-rust/kani_lib/std/src/lib.rs:45:8-60,
                                                           Triggering memory operation)]
  [1]
