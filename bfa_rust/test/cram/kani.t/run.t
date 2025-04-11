Test kani::any
  $ bfa-rust exec-main any.rs --clean
  Done. - Ran 5 branches
  PC: 
    (V|7| == 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ (V|6| != 0) /\
    Distinct(V|0-6|) /\ (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (V|7| != 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ (V|6| != 0) /\
    Distinct(V|0-6|) /\ (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (0 < V|12|) /\ (0 <= V|12|) /\ (V|12| != 0) /\ (V|12| <= 127) /\
    (-128 <= V|12|) /\ (V|11| != 0) /\ Distinct(V|0-11|) /\ (V|10| != 0) /\
    Distinct(V|0-10|) /\ (V|9| != 0) /\ Distinct(V|0-9|) /\ (V|8| != 0) /\
    Distinct(V|0-8|) /\ (V|7| != 0) /\ Distinct(V|0-7|) /\ (V|6| != 0) /\
    Distinct(V|0-6|) /\ (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (V|12| < 0) /\ (V|12| != 0) /\ (V|12| <= 127) /\ (-128 <= V|12|) /\
    (V|11| != 0) /\ Distinct(V|0-11|) /\ (V|10| != 0) /\ Distinct(V|0-10|) /\
    (V|9| != 0) /\ Distinct(V|0-9|) /\ (V|8| != 0) /\ Distinct(V|0-8|) /\
    (V|7| != 0) /\ Distinct(V|0-7|) /\ (V|6| != 0) /\ Distinct(V|0-6|) /\
    (V|5| != 0) /\ Distinct(V|0-5|) /\ (V|4| != 0) /\ Distinct(V|0-4|) /\
    (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\ Distinct(V|0-2|) /\
    (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    (V|12| == 0) /\ (V|12| <= 127) /\ (-128 <= V|12|) /\ (V|11| != 0) /\
    Distinct(V|0-11|) /\ (V|10| != 0) /\ Distinct(V|0-10|) /\ (V|9| != 0) /\
    Distinct(V|0-9|) /\ (V|8| != 0) /\ Distinct(V|0-8|) /\ (V|7| != 0) /\
    Distinct(V|0-7|) /\ (V|6| != 0) /\ Distinct(V|0-6|) /\ (V|5| != 0) /\
    Distinct(V|0-5|) /\ (V|4| != 0) /\ Distinct(V|0-4|) /\ (V|3| != 0) /\
    Distinct(V|0-3|) /\ (V|2| != 0) /\ Distinct(V|0-2|) /\ (V|1| != 0) /\
    Distinct(V|0-1|) /\ (V|0| != 0)

Test kani::assume
  $ bfa-rust exec-main assume.rs --clean
  Done. - Ran 2 branches
  PC: 
    (V|5| != 0) /\ (V|5| <= 1) /\ (0 <= V|5|) /\ (V|4| != 0) /\
    Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\ (V|2| != 0) /\
    Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\ (V|0| != 0)
  
  PC: 
    ((11 / V|7|) <= 2147483647) /\ (-2147483648 <= (11 / V|7|)) /\
    (V|7| != 0) /\ (V|7| <= 2147483647) /\ (-2147483648 <= V|7|) /\
    (V|6| != 0) /\ Distinct(V|0-6|) /\ (V|5| != 0) /\ Distinct(V|0-5|) /\
    (V|4| != 0) /\ Distinct(V|0-4|) /\ (V|3| != 0) /\ Distinct(V|0-3|) /\
    (V|2| != 0) /\ Distinct(V|0-2|) /\ (V|1| != 0) /\ Distinct(V|0-1|) /\
    (V|0| != 0)

Test valid values
  $ bfa-rust exec-main should_panic.rs --clean
  Done. - Ran 1 branches
  PC: 
    (V|0| != 0)
