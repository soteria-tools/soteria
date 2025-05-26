  $ soteria-c gen-summaries load.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
<<<<<<< HEAD
  Summaries for f_484:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(0 == V|0|)];
=======
  Summaries for f_485:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(0 Eq V|0|)];
>>>>>>> f8274ffd (short-circuit and tests)
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
      pc = [(V|3| <= 0x7fffffff); (-0x80000000 <= V|3|); (0 != V|0|)];
      post =
      { heap = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
        globs = [] };
      ret = (Ok V|3|); memory_leak = false }
      manifest bugs: []
  

  $ soteria-c gen-summaries manifest.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
<<<<<<< HEAD
  Summaries for load_486:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(0 == V|0|)];
=======
  Summaries for load_487:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(0 Eq V|0|)];
>>>>>>> f8274ffd (short-circuit and tests)
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
      pc = [(V|3| <= 0x7fffffff); (-0x80000000 <= V|3|); (0 != V|0|)];
      post =
      { heap = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
        globs = [] };
      ret = (Ok V|3|); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_leak_495:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 0); memory_leak = true }
      manifest bugs: [Memory leak with trace [(manifest.c:26:1-34:2 (cursor: 26:5 - 26:14),
                                               )]]
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 1); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_np_uninit_489:
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
  
  Summaries for test_ok_498:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 0); memory_leak = false }
      manifest bugs: []
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 1); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_uninit_492:
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
  
  Summaries for test_np_501:
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
  
The following test case is for regression testing.
if%sat1 had the wrong semantics and would not correctly backtrack.
  $ soteria-c gen-summaries if_sat_one_ok.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Summaries for test_486:
    { args = [V|0|; &(V|1|, V|2|)]; pre = [];
      pc =
      [(0 == V|1|); (0 < V|0|); (V|0| <= 0x7fffffff); (-0x80000000 <= V|0|)];
      post = { heap = []; globs = [] };
      ret =
      (Error NullDereference with trace [(if_sat_one_ok.c:6:12-14 (cursor: 6:12),
                                          Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: []
    { args = [V|0|; &(V|1|, V|2|)];
      pre =
      [{ heap = [(V|1|, [TypedVal {offset = V|2|; ty = signed int; v = V|5|}])];
         globs = [] }
        ];
      pc =
      [(V|5| <= 0x7fffffff); (-0x80000000 <= V|5|); (0 != V|1|); (0 < V|0|);
        (V|0| <= 0x7fffffff); (-0x80000000 <= V|0|)];
      post =
      { heap = [(V|1|, [TypedVal {offset = V|2|; ty = signed int; v = V|5|}])];
        globs = [] };
      ret = (Ok V|5|); memory_leak = false }
      manifest bugs: []
    { args = [V|0|; &(V|1|, V|2|)]; pre = [];
      pc = [(V|0| <= 0); (V|0| <= 0x7fffffff); (-0x80000000 <= V|0|)];
      post = { heap = []; globs = [] }; ret = (Ok 0); memory_leak = false }
      manifest bugs: []
  
