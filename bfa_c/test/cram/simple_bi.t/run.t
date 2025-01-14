  $ bfa-c gen-summaries load.c
  Summaries for f_1:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(V|0| Eq 0); Not((V|2| Eq 0))];
      post =
      [(V|2|,
        [TypedVal {offset = 0; ty = signed int*; v = &(V|0|, V|1|)}; (Bound 8)])];
      ret = (Error NullDereference at load.c:3:10-12 (cursor: 3:10)) }
    { args = [&(V|0|, V|1|)];
      pre = [[(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])]];
      pc =
      [Not((0 Eq V|0|)); (V|3| Leq 2147483647); (-2147483648 Leq V|3|);
        Not((V|0| Eq 0)); Not((V|2| Eq 0))];
      post =
      [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}]);
       (V|2|, Freed)];
      ret = (Ok V|3|) }

  $ bfa-c gen-summaries manifest.c
  Summaries for load_2:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(V|0| Eq 0); Not((V|2| Eq 0))];
      post =
      [(V|2|,
        [TypedVal {offset = 0; ty = signed int*; v = &(V|0|, V|1|)}; (Bound 8)])];
      ret = (Error NullDereference at manifest.c:5:10-12 (cursor: 5:10)) }
    { args = [&(V|0|, V|1|)];
      pre = [[(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])]];
      pc =
      [Not((0 Eq V|0|)); (V|3| Leq 2147483647); (-2147483648 Leq V|3|);
        Not((V|0| Eq 0)); Not((V|2| Eq 0))];
      post =
      [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}]);
       (V|2|, Freed)];
      ret = (Ok V|3|) }
  Summaries for test_4:
    { args = []; pre = [];
      pc =
      [Not((V|6| Eq 0)); Distinct(V|6|, V|4|, V|5|); Not((V|5| Eq 0));
        Distinct(V|5|, V|4|); Not((V|4| Eq 0)); Not((0 Eq V|0|));
        (V|3| Leq 2147483647); (-2147483648 Leq V|3|); Not((V|0| Eq 0));
        Not((V|2| Eq 0))];
      post =
      [(V|4|,
        [TypedVal {offset = 0; ty = signed int*; v = &(V|5|, 0)}; (Bound 8)]);
       (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
       (V|6|,
        [TypedVal {offset = 0; ty = signed int*; v = &(V|5|, 0)}; (Bound 8)])];
      ret =
      (Error UninitializedMemoryAccess at manifest.c:5:10-12 (cursor: 5:10)) }
    { args = []; pre = [];
      pc =
      [Not((V|5| Eq 0)); Distinct(V|5|, V|4|); Not((V|4| Eq 0));
        Not((0 Eq V|0|)); (V|3| Leq 2147483647); (-2147483648 Leq V|3|);
        Not((V|0| Eq 0)); Not((V|2| Eq 0))];
      post =
      [(V|4|,
        [TypedVal {offset = 0; ty = signed int*; v = &(0, 0)}; (Bound 8)]);
       (V|5|,
        [TypedVal {offset = 0; ty = signed int*; v = &(0, 0)}; (Bound 8)])];
      ret = (Error NullDereference at manifest.c:5:10-12 (cursor: 5:10)) }
