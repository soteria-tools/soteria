  $ bfa-c exec-main array_add.c
  Symex terminated with the following outcomes:
    [Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
           (V|2|, Freed); (V|3|, Freed); (V|4|, Freed);
           (V|5|,
            [TypedVal {offset = 0; ty = size_t; v = 10};
             TypedVal {offset = 8; ty = size_t; v = 16};
             TypedVal {offset = 16; ty = void**; v = &(V|31|, 0)}; (Bound 24)]);
           (V|6|, Freed); (V|7|, Freed); (V|8|, Freed); (V|9|, Freed);
           (V|10|, Freed); (V|11|, Freed); (V|12|, Freed); (V|13|, Freed);
           (V|14|, Freed); (V|15|, Freed); (V|16|, Freed); (V|17|, Freed);
           (V|18|, Freed); (V|19|, Freed); (V|20|, Freed); (V|21|, Freed);
           (V|22|, Freed); (V|23|, Freed); (V|24|, Freed); (V|25|, Freed);
           (V|26|, Freed); (V|27|, Freed); (V|28|, Freed); (V|29|, Freed);
           (V|30|, Freed);
           (V|31|,
            [TypedVal {offset = 0; ty = void*; v = &(0, 0)};
             TypedVal {offset = 8; ty = void*; v = &(0, 0)};
             TypedVal {offset = 16; ty = void*; v = &(0, 0)};
             TypedVal {offset = 24; ty = void*; v = &(0, 0)};
             TypedVal {offset = 32; ty = void*; v = &(0, 0)};
             TypedVal {offset = 40; ty = void*; v = &(0, 0)};
             TypedVal {offset = 48; ty = void*; v = &(0, 0)};
             TypedVal {offset = 56; ty = void*; v = &(0, 0)};
             TypedVal {offset = 64; ty = void*; v = &(0, 0)};
             TypedVal {offset = 72; ty = void*; v = &(0, 0)};
             Uninit {offset = 80; len = 48}; (Bound 128)]);
           (V|32|, Freed); (V|33|, Freed)]);
     Error: OutOfBounds with trace [(array_add.c:127:7-31, Call trace);
                                    (array_add.c:79:3-33 (cursor: 79:24),
                                     Triggering memory operation)];
     Ok: (1,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
           (V|2|, Freed); (V|3|, Freed); (V|4|, Freed); (V|5|, Freed);
           (V|6|, Freed)]);
     Ok: (1,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
           (V|2|, Freed); (V|3|, Freed); (V|4|, Freed)]);
     Ok: (1, [(V|0|, Freed)])]
  Executed 163 statements
