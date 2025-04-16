Test kani::any
  $ bfa-rust exec-main any.rs --clean
  Done. - Ran 5 branches
  PC:
    (V|7| == 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ Distinct(V|0-6|) /\
    (V|6| != 0) /\ (V|5| != 0) /\ (V|4| != 0) /\ (V|3| != 0) /\ (V|2| != 0) /\
    (V|1| != 0) /\ (V|0| != 0)

  PC:
    (V|7| != 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ Distinct(V|0-6|) /\
    (V|6| != 0) /\ (V|5| != 0) /\ (V|4| != 0) /\ (V|3| != 0) /\ (V|2| != 0) /\
    (V|1| != 0) /\ (V|0| != 0)

  PC:
    (0 < V|20|) /\ (0 <= V|20|) /\ (V|20| != 0) /\ (V|20| <= 127) /\
    (-128 <= V|20|) /\ Distinct(V|8-19|) /\ (V|19| != 0) /\ (V|18| != 0) /\
    (V|17| != 0) /\ (V|16| != 0) /\ (V|15| != 0) /\ (V|14| != 0) /\
    (V|13| != 0) /\ (V|12| != 0) /\ (V|11| != 0) /\ (V|10| != 0) /\
    (V|9| != 0) /\ (V|8| != 0) /\ (V|7| != 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\
    Distinct(V|0-6|) /\ (V|6| != 0) /\ (V|5| != 0) /\ (V|4| != 0) /\
    (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\ (V|0| != 0)

  PC:
    (V|20| < 0) /\ (V|20| != 0) /\ (V|20| <= 127) /\ (-128 <= V|20|) /\
    Distinct(V|8-19|) /\ (V|19| != 0) /\ (V|18| != 0) /\ (V|17| != 0) /\
    (V|16| != 0) /\ (V|15| != 0) /\ (V|14| != 0) /\ (V|13| != 0) /\
    (V|12| != 0) /\ (V|11| != 0) /\ (V|10| != 0) /\ (V|9| != 0) /\
    (V|8| != 0) /\ (V|7| != 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\
    Distinct(V|0-6|) /\ (V|6| != 0) /\ (V|5| != 0) /\ (V|4| != 0) /\
    (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\ (V|0| != 0)

  PC:
    (V|20| == 0) /\ (V|20| <= 127) /\ (-128 <= V|20|) /\ Distinct(V|8-19|) /\
    (V|19| != 0) /\ (V|18| != 0) /\ (V|17| != 0) /\ (V|16| != 0) /\
    (V|15| != 0) /\ (V|14| != 0) /\ (V|13| != 0) /\ (V|12| != 0) /\
    (V|11| != 0) /\ (V|10| != 0) /\ (V|9| != 0) /\ (V|8| != 0) /\
    (V|7| != 0) /\ (V|7| <= 1) /\ (0 <= V|7|) /\ Distinct(V|0-6|) /\
    (V|6| != 0) /\ (V|5| != 0) /\ (V|4| != 0) /\ (V|3| != 0) /\ (V|2| != 0) /\
    (V|1| != 0) /\ (V|0| != 0)

Test kani::assume
  $ bfa-rust exec-main assume.rs --clean
  Done. - Ran 2 branches
  PC:
    (V|5| != 0) /\ (V|5| <= 1) /\ (0 <= V|5|) /\ Distinct(V|0-4|) /\
    (V|4| != 0) /\ (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\ (V|0| != 0)

  PC:
    ((11 / V|13|) <= 2147483647) /\ (-2147483648 <= (11 / V|13|)) /\
    (V|13| != 0) /\ (V|13| <= 2147483647) /\ (-2147483648 <= V|13|) /\
    Distinct(V|6-12|) /\ (V|12| != 0) /\ (V|11| != 0) /\ (V|10| != 0) /\
    (V|9| != 0) /\ (V|8| != 0) /\ (V|7| != 0) /\ (V|6| != 0) /\ (V|5| != 0) /\
    (V|5| <= 1) /\ (0 <= V|5|) /\ Distinct(V|0-4|) /\ (V|4| != 0) /\
    (V|3| != 0) /\ (V|2| != 0) /\ (V|1| != 0) /\ (V|0| != 0)

Test valid values
  $ bfa-rust exec-main should_panic.rs --clean
  Done. - Ran 1 branches
  PC:
    (V|0| != 0)
