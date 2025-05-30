Test kani::any
  $ soteria-rust exec-main any.rs --clean --kani
  Done. - Ran 5 branches
  PC: 
    (0 == V|0|) /\ (V|0| <= 1) /\ (0 <= V|0|)
  
  PC: 
    (0 != V|0|) /\ (V|0| <= 1) /\ (0 <= V|0|)
  
  PC: 
    (0 < V|0|) /\ (0 <= V|0|) /\ (0 != V|0|) /\ (V|0| <= 127) /\ (-128 <= V|0|)
  
  PC: 
    (V|0| < 0) /\ (0 != V|0|) /\ (V|0| <= 127) /\ (-128 <= V|0|)
  
  PC: 
    (0 == V|0|) /\ (V|0| <= 127) /\ (-128 <= V|0|)

Test kani::assume
  $ soteria-rust exec-main assume.rs --clean --kani
  Done. - Ran 2 branches
  PC: 
    (0 != V|0|) /\ (V|0| <= 1) /\ (0 <= V|0|)
  
  PC: 
    (-0x80000000 <= (11 / V|0|)) /\ ((11 / V|0|) <= 0x7fffffff) /\
    (0 != V|0|) /\ (V|0| <= 0x7fffffff) /\ (-0x80000000 <= V|0|)

Test #[kani::should_panic]
  $ soteria-rust exec-main should_panic.rs --clean --kani
  Done. - Ran 1 branches
  PC: empty

Test kani::assert
  $ soteria-rust exec-main assert.rs --clean --kani
  Error in 4 branches:
  - Failed assertion: Expected true!
    Trace:
    • Call trace: ../cram/kani.t/assert.rs:2:0-5:1
    • Call trace: ../cram/kani.t/assert.rs:4:4-37
    • Triggering memory operation: ../cram/kani.t/assert.rs:4:4-37
  
  - Failed assertion: 👻 unicode is 𝒮𝒞𝒜ℛ𝒴
    Trace:
    • Call trace: ../cram/kani.t/assert.rs:8:0-11:1
    • Call trace: ../cram/kani.t/assert.rs:10:4-42
    • Triggering memory operation: ../cram/kani.t/assert.rs:10:4-42
  
  - Failed assertion: I used "assert!"
    Trace:
    • Call trace: ../cram/kani.t/assert.rs:14:0-17:1
    • Call trace: ../std/src/lib.rs:23:8-64
    • Triggering memory operation: ../std/src/lib.rs:23:8-64
  
  - Failed assertion: I used "assert_eq!"
    Trace:
    • Call trace: ../cram/kani.t/assert.rs:20:0-24:1
    • Call trace: ../std/src/lib.rs:23:8-64
    • Triggering memory operation: ../std/src/lib.rs:23:8-64
  [1]

Test kani::slice::any_slice_of_array
  $ soteria-rust exec-main any_slice.rs --clean --kani
  Done. - Ran 7 branches
  PC: 
    ((V|1| - V|0|) <= 0) /\ ((V|1| - V|0|) <= 3) /\ (0 <= (V|1| - V|0|)) /\
    (V|0| <= V|1|) /\ (V|1| <= 3) /\ (V|1| <= 0xffffffffffffffff) /\
    (0 <= V|1|) /\ (V|0| <= 0xffffffffffffffff) /\ (0 <= V|0|)
  
  PC: 
    ((V|1| - V|0|) <= 1) /\ (0 == V|0|) /\ (V|0| <= 0) /\
    !(((0 < V|0|) && (V|0| < 1))) /\ !(((2 == V|0|) && (0 == V|0|))) /\
    (V|0| <= 2) /\ (0 < (V|1| - V|0|)) /\ ((V|1| - V|0|) <= 3) /\
    (0 <= (V|1| - V|0|)) /\ (V|0| <= V|1|) /\ (V|1| <= 3) /\
    (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|) /\
    (V|0| <= 0xffffffffffffffff) /\ (0 <= V|0|)
  
  PC: 
    ((V|1| - V|0|) <= 2) /\ (1 != V|0|) /\ (-1 < V|0|) /\
    !(((1 == V|0|) && (V|0| == -1))) /\ (-1 <= V|0|) /\ (V|0| <= 1) /\
    (1 < (V|1| - V|0|)) /\ (0 == V|0|) /\ (V|0| <= 0) /\
    !(((0 < V|0|) && (V|0| < 1))) /\ !(((2 == V|0|) && (0 == V|0|))) /\
    (V|0| <= 2) /\ (0 < (V|1| - V|0|)) /\ ((V|1| - V|0|) <= 3) /\
    (0 <= (V|1| - V|0|)) /\ (V|0| <= V|1|) /\ (V|1| <= 3) /\
    (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|) /\
    (V|0| <= 0xffffffffffffffff) /\ (0 <= V|0|)
  
  PC: 
    (V|0| != -1) /\ (-2 < V|0|) /\ (V|0| != -2) /\ (-2 <= V|0|) /\
    (2 < (V|1| - V|0|)) /\ (1 != V|0|) /\ (-1 < V|0|) /\
    !(((1 == V|0|) && (V|0| == -1))) /\ (-1 <= V|0|) /\ (V|0| <= 1) /\
    (1 < (V|1| - V|0|)) /\ (0 == V|0|) /\ (V|0| <= 0) /\
    !(((0 < V|0|) && (V|0| < 1))) /\ !(((2 == V|0|) && (0 == V|0|))) /\
    (V|0| <= 2) /\ (0 < (V|1| - V|0|)) /\ ((V|1| - V|0|) <= 3) /\
    (0 <= (V|1| - V|0|)) /\ (V|0| <= V|1|) /\ (V|1| <= 3) /\
    (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|) /\
    (V|0| <= 0xffffffffffffffff) /\ (0 <= V|0|)
  
  PC: 
    ((V|1| - V|0|) <= 1) /\ (1 == V|0|) /\ (V|0| <= 1) /\ (1 <= V|0|) /\
    !(((1 < V|0|) && (V|0| < 2))) /\ !(((2 == V|0|) && (1 == V|0|))) /\
    (0 < V|0|) /\ !(((0 < V|0|) && (V|0| < 1))) /\
    !(((2 == V|0|) && (0 == V|0|))) /\ (V|0| <= 2) /\ (0 < (V|1| - V|0|)) /\
    ((V|1| - V|0|) <= 3) /\ (0 <= (V|1| - V|0|)) /\ (V|0| <= V|1|) /\
    (V|1| <= 3) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|) /\
    (V|0| <= 0xffffffffffffffff) /\ (0 <= V|0|)
  
  PC: 
    ((V|1| - V|0|) <= 2) /\ (0 != V|0|) /\ (-1 < V|0|) /\ (V|0| != -1) /\
    (-1 <= V|0|) /\ (1 < (V|1| - V|0|)) /\ (1 == V|0|) /\ (V|0| <= 1) /\
    (1 <= V|0|) /\ !(((1 < V|0|) && (V|0| < 2))) /\
    !(((2 == V|0|) && (1 == V|0|))) /\ (0 < V|0|) /\
    !(((0 < V|0|) && (V|0| < 1))) /\ !(((2 == V|0|) && (0 == V|0|))) /\
    (V|0| <= 2) /\ (0 < (V|1| - V|0|)) /\ ((V|1| - V|0|) <= 3) /\
    (0 <= (V|1| - V|0|)) /\ (V|0| <= V|1|) /\ (V|1| <= 3) /\
    (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|) /\
    (V|0| <= 0xffffffffffffffff) /\ (0 <= V|0|)
  
  PC: 
    ((V|1| - V|0|) <= 1) /\ (2 == V|0|) /\ !(((1 <= V|0|) && (V|0| <= 1))) /\
    !(((1 < V|0|) && (V|0| < 2))) /\ !(((2 == V|0|) && (1 == V|0|))) /\
    (0 < V|0|) /\ !(((0 < V|0|) && (V|0| < 1))) /\
    !(((2 == V|0|) && (0 == V|0|))) /\ (V|0| <= 2) /\ (0 < (V|1| - V|0|)) /\
    ((V|1| - V|0|) <= 3) /\ (0 <= (V|1| - V|0|)) /\ (V|0| <= V|1|) /\
    (V|1| <= 3) /\ (V|1| <= 0xffffffffffffffff) /\ (0 <= V|1|) /\
    (V|0| <= 0xffffffffffffffff) /\ (0 <= V|0|)
