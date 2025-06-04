  $ soteria-c gen-summaries load.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --no-progress-bar --dump-summaries "out.summaries" && cat out.summaries
  Summaries for f_485:
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)]; pre = []; pc = [(0 == V|1|)];
        post = { heap = []; globs = [] };
        ret =
        (Error (NullDereference,
                [(load.c:3:10-12 (cursor: 3:10), Triggering memory operation)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)];
        pre =
        [{ heap =
           [(V|1|, [TypedVal {offset = V|2|; ty = signed int; v = V|4|}])];
           globs = [] }
          ];
        pc = [(V|4| <= 0x7fffffff); (-0x80000000 <= V|4|); (0 != V|1|)];
        post =
        { heap =
          [(V|1|, [TypedVal {offset = V|2|; ty = signed int; v = V|4|}])];
          globs = [] };
        ret = (Ok V|4|) };
      manifest_bugs = []}
  

  $ soteria-c gen-summaries manifest.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --no-progress-bar --dump-summaries "out.summaries" && cat out.summaries
  Function test_leak_495 has the following bugs:
    Memory leak with trace [(manifest.c:26:1-34:2 (cursor: 26:5 - 26:14), )]
  Function test_np_uninit_489 has the following bugs:
    NullDereference with trace [(manifest.c:12:3-10, Call trace);
                                (manifest.c:6:10-12 (cursor: 6:10),
                                 Triggering memory operation)]
    UninitializedMemoryAccess with trace [(manifest.c:12:3-10, Call trace);
                                          (manifest.c:6:10-12 (cursor: 6:10),
                                           Triggering memory operation)]
  Function test_uninit_492 has the following bugs:
    UninitializedMemoryAccess with trace [(manifest.c:21:3-10, Call trace);
                                          (manifest.c:6:10-12 (cursor: 6:10),
                                           Triggering memory operation)]
  Function test_np_501 has the following bugs:
    NullDereference with trace [(manifest.c:51:3-10 (cursor: 51:6),
                                 Triggering memory operation)]
  Summaries for load_487:
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)]; pre = []; pc = [(0 == V|1|)];
        post = { heap = []; globs = [] };
        ret =
        (Error (NullDereference,
                [(manifest.c:6:10-12 (cursor: 6:10),
                  Triggering memory operation)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)];
        pre =
        [{ heap =
           [(V|1|, [TypedVal {offset = V|2|; ty = signed int; v = V|4|}])];
           globs = [] }
          ];
        pc = [(V|4| <= 0x7fffffff); (-0x80000000 <= V|4|); (0 != V|1|)];
        post =
        { heap =
          [(V|1|, [TypedVal {offset = V|2|; ty = signed int; v = V|4|}])];
          globs = [] };
        ret = (Ok V|4|) };
      manifest_bugs = []}
  
  Summaries for test_leak_495:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 0) };
      manifest_bugs =
      [(Memory leak, [(manifest.c:26:1-34:2 (cursor: 26:5 - 26:14), )])]}
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 1) };
      manifest_bugs = []}
  
  Summaries for test_np_uninit_489:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret =
        (Error (UninitializedMemoryAccess,
                [(manifest.c:12:3-10, Call trace);
                 (manifest.c:6:10-12 (cursor: 6:10),
                  Triggering memory operation)]))
        };
      manifest_bugs =
      [(UninitializedMemoryAccess,
        [(manifest.c:12:3-10, Call trace);
         (manifest.c:6:10-12 (cursor: 6:10), Triggering memory operation)])]}
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret =
        (Error (NullDereference,
                [(manifest.c:12:3-10, Call trace);
                 (manifest.c:6:10-12 (cursor: 6:10),
                  Triggering memory operation)]))
        };
      manifest_bugs =
      [(NullDereference,
        [(manifest.c:12:3-10, Call trace);
         (manifest.c:6:10-12 (cursor: 6:10), Triggering memory operation)])]}
  
  Summaries for test_ok_498:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 0) };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 1) };
      manifest_bugs = []}
  
  Summaries for test_uninit_492:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret =
        (Error (UninitializedMemoryAccess,
                [(manifest.c:21:3-10, Call trace);
                 (manifest.c:6:10-12 (cursor: 6:10),
                  Triggering memory operation)]))
        };
      manifest_bugs =
      [(UninitializedMemoryAccess,
        [(manifest.c:21:3-10, Call trace);
         (manifest.c:6:10-12 (cursor: 6:10), Triggering memory operation)])]}
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 1) };
      manifest_bugs = []}
  
  Summaries for test_np_501:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 0) };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret =
        (Error (NullDereference,
                [(manifest.c:51:3-10 (cursor: 51:6),
                  Triggering memory operation)]))
        };
      manifest_bugs =
      [(NullDereference,
        [(manifest.c:51:3-10 (cursor: 51:6), Triggering memory operation)])]}
  
The following test case is for regression testing.
if%sat1 had the wrong semantics and would not correctly backtrack.
  $ soteria-c gen-summaries if_sat_one_ok.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --no-progress-bar --dump-summaries "out.summaries" && cat out.summaries
  Summaries for test_486:
    Analysed {
      raw =
      { args = [V|1|; &(V|2|, V|3|)]; pre = [];
        pc =
        [(0 == V|2|); (0 < V|1|); (V|1| <= 0x7fffffff); (-0x80000000 <= V|1|)];
        post = { heap = []; globs = [] };
        ret =
        (Error (NullDereference,
                [(if_sat_one_ok.c:6:12-14 (cursor: 6:12),
                  Triggering memory operation)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [V|1|; &(V|2|, V|3|)];
        pre =
        [{ heap =
           [(V|2|, [TypedVal {offset = V|3|; ty = signed int; v = V|6|}])];
           globs = [] }
          ];
        pc =
        [(V|6| <= 0x7fffffff); (-0x80000000 <= V|6|); (0 != V|2|); (0 < V|1|);
          (V|1| <= 0x7fffffff); (-0x80000000 <= V|1|)];
        post =
        { heap =
          [(V|2|, [TypedVal {offset = V|3|; ty = signed int; v = V|6|}])];
          globs = [] };
        ret = (Ok V|6|) };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [V|1|; &(V|2|, V|3|)]; pre = [];
        pc = [(V|1| <= 0); (V|1| <= 0x7fffffff); (-0x80000000 <= V|1|)];
        post = { heap = []; globs = [] }; ret = (Ok 0) };
      manifest_bugs = []}
  
