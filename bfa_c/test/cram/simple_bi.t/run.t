  $ bfa-c gen-summaries load.c
  Summaries for f_484:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(0 Eq V|0|)];
      post = { heap = []; globs = [] };
      ret =
      (Error NullDereference with trace [(load.c:3:10-12 (cursor: 3:10),
                                          Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: []
    { args = [&(V|0|, V|1|)];
      pre =
      [{ heap = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
         globs = [] }
        ];
      pc = [(V|3| Leq 2147483647); (-2147483648 Leq V|3|); Not((0 Eq V|0|))];
      post =
      { heap = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
        globs = [] };
      ret = (Ok V|3|); memory_leak = false }
      manifest bugs: []
  

  $ bfa-c gen-summaries manifest.c
  Summaries for load_486:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(0 Eq V|0|)];
      post = { heap = []; globs = [] };
      ret =
      (Error NullDereference with trace [(manifest.c:6:10-12 (cursor: 6:10),
                                          Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: []
    { args = [&(V|0|, V|1|)];
      pre =
      [{ heap = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
         globs = [] }
        ];
      pc = [(V|3| Leq 2147483647); (-2147483648 Leq V|3|); Not((0 Eq V|0|))];
      post =
      { heap = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
        globs = [] };
      ret = (Ok V|3|); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_uninit_491:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret =
      (Error UninitializedMemoryAccess with trace [(manifest.c:21:3-10,
                                                    Call trace);
                                                   (manifest.c:6:10-12 (cursor: 6:10),
                                                    Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: [UninitializedMemoryAccess with trace [(manifest.c:21:3-10,
                                                             Call trace);
                                                            (manifest.c:6:10-12 (cursor: 6:10),
                                                             Triggering memory operation)]]
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 1); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_leak_494:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 0); memory_leak = true }
      manifest bugs: [Memory leak with trace [(manifest.c:26:1-34:2 (cursor: 26:5 - 26:14),
                                               )]]
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 1); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_np_500:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 0); memory_leak = false }
      manifest bugs: []
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret =
      (Error NullDereference with trace [(manifest.c:51:3-10 (cursor: 51:6),
                                          Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: [NullDereference with trace [(manifest.c:51:3-10 (cursor: 51:6),
                                                   Triggering memory operation)]]
  
  Summaries for test_ok_497:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 0); memory_leak = false }
      manifest bugs: []
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 1); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_np_uninit_488:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret =
      (Error UninitializedMemoryAccess with trace [(manifest.c:12:3-10,
                                                    Call trace);
                                                   (manifest.c:6:10-12 (cursor: 6:10),
                                                    Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: [UninitializedMemoryAccess with trace [(manifest.c:12:3-10,
                                                             Call trace);
                                                            (manifest.c:6:10-12 (cursor: 6:10),
                                                             Triggering memory operation)]]
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret =
      (Error NullDereference with trace [(manifest.c:12:3-10, Call trace);
                                         (manifest.c:6:10-12 (cursor: 6:10),
                                          Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: [NullDereference with trace [(manifest.c:12:3-10,
                                                   Call trace);
                                                  (manifest.c:6:10-12 (cursor: 6:10),
                                                   Triggering memory operation)]]
  
