  $ soteria-rust exec array_init.rs
  Compiling... done in <time>
  => Running array_init::main...
  note: array_init::main: done in <time>, ran 1 branch
  PC 1: empty
  
  $ soteria-rust exec btreeset_sort.rs
  Compiling... done in <time>
  => Running btreeset_sort::test_treeset_is_ordered...
  note: btreeset_sort::test_treeset_is_ordered: done in <time>, ran 75 branches
  PC 1: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|4| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
        (V|5| <u V|3|) /\ (V|5| <u V|4|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
       V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
       V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|, V|13|) /\
        (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
        (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffc6) /\
        (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffee) /\
        (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
        (0x0000000000000004 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffee) /\
        (0x0000000000000008 <=u V|13|) /\ (V|13| <=u 0x7fffffffffffffb6) /\
        (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
        (0b00 == extract[0-1](V|7|)) /\ (0b00 == extract[0-1](V|8|)) /\
        (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
        (0b000 == extract[0-2](V|11|)) /\ (0b00 == extract[0-1](V|12|)) /\
        (0b000 == extract[0-2](V|13|))
  PC 2: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|4| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
        (V|5| <u V|3|) /\ (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|,
       V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ (V|4| != V|5|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
       V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
       V|9|, V|10|, V|11|, V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
       V|10|, V|11|, V|12|, V|13|) /\ (0x0000000000000004 <=u V|1|) /\
        (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
        (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
        (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
        (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
        (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
        (V|10| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|11|) /\
        (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
        (V|12| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|13|) /\
        (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
        (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
        (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
        (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
        (0b00 == extract[0-1](V|12|)) /\ (0b000 == extract[0-2](V|13|))
  PC 3: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|4| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
        (V|5| <u V|3|) /\ (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|,
       V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|,
       V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
       V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
       V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|,
       V|13|) /\ (0x0000000000000004 <=u V|1|) /\
        (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
        (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
        (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
        (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
        (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
        (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
        (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
        (V|12| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|13|) /\
        (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
        (V|4| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
        (0b00 == extract[0-1](V|7|)) /\ (0b00 == extract[0-1](V|8|)) /\
        (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
        (0b000 == extract[0-2](V|11|)) /\ (0b00 == extract[0-1](V|12|)) /\
        (0b000 == extract[0-2](V|13|))
  PC 4: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|4| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
        (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
       V|6|, V|7|, V|8|, V|9|) /\ (V|3| != V|5|) /\ Distinct(V|1|, V|6|, V|7|,
       V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
       V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|, V|13|) /\
        (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
        (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffc6) /\
        (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffee) /\
        (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
        (0x0000000000000004 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffee) /\
        (0x0000000000000008 <=u V|13|) /\ (V|13| <=u 0x7fffffffffffffb6) /\
        (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
        (0b00 == extract[0-1](V|7|)) /\ (0b00 == extract[0-1](V|8|)) /\
        (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
        (0b000 == extract[0-2](V|11|)) /\ (0b00 == extract[0-1](V|12|)) /\
        (0b000 == extract[0-2](V|13|))
  PC 5: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|4| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
        (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
       V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
       V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|, V|13|) /\
        (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
        (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffc6) /\
        (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
        (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
        (0x0000000000000004 <=u V|12|) /\ (V|12| <=u 0x7ffffffffffffff2) /\
        (0x0000000000000008 <=u V|13|) /\ (V|13| <=u 0x7fffffffffffffb6) /\
        (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|5|) /\
        (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
        (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
        (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
        (0b00 == extract[0-1](V|12|)) /\ (0b000 == extract[0-2](V|13|))
  PC 6: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|4| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| <=u V|5|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|5|) /\ Distinct(V|1|,
       V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
       V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
        (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
        (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
        (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffee) /\
        (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
        (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffee) /\
        (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
        (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
        (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
        (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
        (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 7: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|4| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| <=u V|5|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
       V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|,
       V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|,
       V|8|, V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
        (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
        (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
        (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
        (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
        (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
        (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
        (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
        (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
        (V|2| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
        (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
        (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
        (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 8: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
        (V|5| <u V|4|) /\ (V|5| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ (V|3| != V|4|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
       V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
       V|9|, V|10|, V|11|, V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
       V|10|, V|11|, V|12|, V|13|) /\ (0x0000000000000004 <=u V|1|) /\
        (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
        (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
        (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
        (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
        (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
        (V|10| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|11|) /\
        (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
        (V|12| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|13|) /\
        (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
        (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
        (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
        (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
        (0b00 == extract[0-1](V|12|)) /\ (0b000 == extract[0-2](V|13|))
  PC 9: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
        (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
        (V|5| <u V|4|) /\ (V|5| <u V|3|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
       V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
       V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
        Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|, V|13|) /\
        (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
        (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffffa) /\
        (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffc6) /\
        (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
        (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
        (0x0000000000000004 <=u V|12|) /\ (V|12| <=u 0x7ffffffffffffff2) /\
        (0x0000000000000008 <=u V|13|) /\ (V|13| <=u 0x7fffffffffffffb6) /\
        (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|4|) /\
        (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
        (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
        (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
        (0b00 == extract[0-1](V|12|)) /\ (0b000 == extract[0-2](V|13|))
  PC 10: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
         (V|5| <u V|4|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ (V|3| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|, V|11|, V|12|, V|13|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|13|) /\
         (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b00 == extract[0-1](V|12|)) /\ (0b000 == extract[0-2](V|13|))
  PC 11: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
         (V|5| <u V|4|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|,
        V|13|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
         (V|12| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|13|) /\
         (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b00 == extract[0-1](V|12|)) /\
         (0b000 == extract[0-2](V|13|))
  PC 12: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ (V|3| != V|4|) /\ (V|4| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|, V|11|, V|12|, V|13|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|13|) /\
         (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b00 == extract[0-1](V|12|)) /\ (0b000 == extract[0-2](V|13|))
  PC 13: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ (V|3| != V|4|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|,
        V|13|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
         (V|12| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|13|) /\
         (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|4| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b00 == extract[0-1](V|12|)) /\
         (0b000 == extract[0-2](V|13|))
  PC 14: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ (V|3| != V|5|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|,
        V|13|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|12|) /\
         (V|12| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|13|) /\
         (V|13| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|4|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b00 == extract[0-1](V|12|)) /\
         (0b000 == extract[0-2](V|13|))
  PC 15: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|5| <u V|2|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|, V|13|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|12|) /\ (V|12| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|13|) /\ (V|13| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|4|) /\ (V|3| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b00 == extract[0-1](V|12|)) /\ (0b000 == extract[0-2](V|13|))
  PC 16: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|3| != V|4|) /\
         (V|2| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 17: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|3| != V|4|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 18: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|5|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|4|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 19: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|4| <u V|2|) /\
         (V|3| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|4|) /\ (V|2| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 20: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|5| <u V|2|) /\ (V|5| <u V|3|) /\ Distinct(V|1|,
        V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|4|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 21: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|5| <u V|2|) /\ (V|5| <u V|3|) /\ Distinct(V|1|,
        V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|4|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 22: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|5| <u V|2|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|,
        V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|3| != V|5|) /\
         (V|2| != V|4|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 23: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|5| <u V|2|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|,
        V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|3| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|4|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 24: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|5| <u V|2|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|,
        V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|4|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 25: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|5| <u V|2|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|,
        V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|4|) /\ (V|3| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 26: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|5|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 27: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|5| <u V|4|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 28: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|4|) /\
         (V|4| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 29: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|4|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|4| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 30: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|4|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 31: (V|3| <u V|2|) /\ Distinct(V|1|, V|6|) /\ (V|2| <=u V|4|) /\
         (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|4|) /\ (V|2| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 32: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ (V|5| <u V|4|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         (V|2| != V|3|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 33: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ (V|5| <u V|4|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 34: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         (V|4| != V|5|) /\ (V|2| != V|3|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 35: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         (V|4| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 36: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         (V|2| != V|3|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|4| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 37: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (V|4| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 38: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 39: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 40: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         (V|2| != V|3|) /\ (V|3| != V|5|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 41: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         (V|2| != V|3|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 42: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         (V|2| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|3|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 43: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|4| <u V|2|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|3|) /\ (V|2| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 44: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|4|) /\ (V|5| <u V|2|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         (V|2| != V|4|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 45: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|4|) /\ (V|5| <u V|2|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|4|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 46: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|4|) /\ (V|2| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         (V|2| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 47: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|5| <u V|4|) /\ (V|2| <=u V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 48: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|4|) /\
         (V|4| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|, V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 49: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|4|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|4| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 50: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ (V|2| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|,
        V|12|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|11|) /\
         (V|11| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|12|) /\
         (V|12| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|4|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b00 == extract[0-1](V|11|)) /\ (0b000 == extract[0-2](V|12|))
  PC 51: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|5| <u V|3|) /\ (V|4| <=u V|5|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|, V|11|, V|12|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|11|) /\ (V|11| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|12|) /\ (V|12| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|4|) /\ (V|2| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b00 == extract[0-1](V|11|)) /\
         (0b000 == extract[0-2](V|12|))
  PC 52: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         (V|2| != V|4|) /\ (V|3| != V|5|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 53: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         (V|2| != V|4|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 54: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         (V|3| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff2) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|4|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 55: (V|2| <=u V|3|) /\ (V|4| <u V|3|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|,
        V|6|) /\ (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|4|) /\ (V|3| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 56: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|3|) /\ (V|3| != V|4|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 57: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|3|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|4|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 58: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|4|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 59: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|5| <u V|2|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (V|2| == V|4|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 60: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|5|) /\ (V|3| != V|4|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 61: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ (V|2| != V|5|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|4|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 62: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ (V|3| != V|4|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 63: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|5| <u V|3|) /\ (V|2| <=u V|5|) /\ Distinct(V|1|, V|6|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|4|) /\ (V|2| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 64: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ (V|2| != V|3|) /\ (V|3| != V|5|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|,
        V|10|, V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 65: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ (V|2| != V|3|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|5|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 66: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ (V|2| != V|5|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|,
        V|11|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000004 <=u V|6|) /\
         (V|6| <=u 0x7ffffffffffffffa) /\ (0x0000000000000008 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|8|) /\
         (V|8| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|10|) /\
         (V|10| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|11|) /\
         (V|11| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (0b00 == extract[0-1](V|6|)) /\
         (0b00 == extract[0-1](V|8|)) /\ (0b000 == extract[0-2](V|9|)) /\
         (0b00 == extract[0-1](V|10|)) /\ (0b000 == extract[0-2](V|11|)) /\
         (0b000 == extract[0-2](V|7|))
  PC 67: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|5| <u V|4|) /\
         (V|3| <=u V|5|) /\ Distinct(V|1|, V|6|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|, V|11|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000004 <=u V|6|) /\ (V|6| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|7|) /\ (V|7| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|8|) /\ (V|8| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|9|) /\ (V|9| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|10|) /\ (V|10| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|11|) /\ (V|11| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|3|) /\ (V|2| == V|5|) /\
         (0b00 == extract[0-1](V|6|)) /\ (0b00 == extract[0-1](V|8|)) /\
         (0b000 == extract[0-2](V|9|)) /\ (0b00 == extract[0-1](V|10|)) /\
         (0b000 == extract[0-2](V|11|)) /\ (0b000 == extract[0-2](V|7|))
  PC 68: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ (V|2| != V|3|) /\ (V|3| != V|4|) /\
         (V|4| != V|5|) /\ Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|,
        V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|, V|9|, V|10|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|6|) /\
         (V|6| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b000 == extract[0-2](V|6|))
  PC 69: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ (V|2| != V|3|) /\ (V|3| != V|4|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|6|) /\
         (V|6| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|4| == V|5|) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b000 == extract[0-2](V|6|))
  PC 70: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ (V|2| != V|3|) /\ (V|3| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|6|) /\
         (V|6| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|3| == V|4|) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b000 == extract[0-2](V|6|))
  PC 71: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ (V|2| != V|3|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|6|) /\ (V|6| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|3| == V|4|) /\ (V|3| == V|5|) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b000 == extract[0-2](V|6|))
  PC 72: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ (V|2| != V|4|) /\ (V|4| != V|5|) /\
         Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|, V|10|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|6|) /\
         (V|6| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff2) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b000 == extract[0-2](V|6|))
  PC 73: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ (V|2| != V|4|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|) /\ (V|2| <=u V|4|) /\ Distinct(V|1|, V|6|, V|7|, V|8|,
        V|9|, V|10|) /\ (0x0000000000000004 <=u V|1|) /\
         (V|1| <=u 0x7fffffffffffffee) /\ (0x0000000000000008 <=u V|6|) /\
         (V|6| <=u 0x7fffffffffffffc6) /\ (0x0000000000000004 <=u V|7|) /\
         (V|7| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|8|) /\
         (V|8| <=u 0x7fffffffffffffb6) /\ (0x0000000000000004 <=u V|9|) /\
         (V|9| <=u 0x7ffffffffffffff6) /\ (0x0000000000000008 <=u V|10|) /\
         (V|10| <=u 0x7fffffffffffffb6) /\ (0b00 == extract[0-1](V|1|)) /\
         (V|2| == V|3|) /\ (V|4| == V|5|) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b000 == extract[0-2](V|6|))
  PC 74: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ (V|2| != V|5|) /\ Distinct(V|1|, V|6|,
        V|7|) /\ Distinct(V|1|, V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|,
        V|8|, V|9|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|6|) /\ (V|6| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffff6) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|3|) /\ (V|2| == V|4|) /\
         (0b00 == extract[0-1](V|7|)) /\ (0b000 == extract[0-2](V|8|)) /\
         (0b00 == extract[0-1](V|9|)) /\ (0b000 == extract[0-2](V|10|)) /\
         (0b000 == extract[0-2](V|6|))
  PC 75: (V|2| <=u V|3|) /\ (V|3| <=u V|4|) /\ (V|4| <=u V|5|) /\
         Distinct(V|1|, V|6|) /\ Distinct(V|1|, V|6|, V|7|) /\ Distinct(V|1|,
        V|6|, V|7|, V|8|) /\ Distinct(V|1|, V|6|, V|7|, V|8|, V|9|) /\
         Distinct(V|1|, V|6|, V|7|, V|8|, V|9|, V|10|) /\
         (0x0000000000000004 <=u V|1|) /\ (V|1| <=u 0x7fffffffffffffee) /\
         (0x0000000000000008 <=u V|6|) /\ (V|6| <=u 0x7fffffffffffffc6) /\
         (0x0000000000000004 <=u V|7|) /\ (V|7| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|8|) /\ (V|8| <=u 0x7fffffffffffffb6) /\
         (0x0000000000000004 <=u V|9|) /\ (V|9| <=u 0x7ffffffffffffffa) /\
         (0x0000000000000008 <=u V|10|) /\ (V|10| <=u 0x7fffffffffffffb6) /\
         (0b00 == extract[0-1](V|1|)) /\ (V|2| == V|3|) /\ (V|2| == V|4|) /\
         (V|2| == V|5|) /\ (0b00 == extract[0-1](V|7|)) /\
         (0b000 == extract[0-2](V|8|)) /\ (0b00 == extract[0-1](V|9|)) /\
         (0b000 == extract[0-2](V|10|)) /\ (0b000 == extract[0-2](V|6|))
  
