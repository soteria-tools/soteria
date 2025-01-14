  $ bfa-c gen-summaries load.c
  Summaries for f_1:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(V|0| Eq 0)]; post = [];
      ret = (Error NullDereference at load.c:3:10-12 (cursor: 3:10));
      memory_leak = false }
      manifest bugs: []
    { args = [&(V|0|, V|1|)];
      pre = [[(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])]];
      pc =
      [Not((0 Eq V|0|)); (V|3| Leq 2147483647); (-2147483648 Leq V|3|);
        Not((V|0| Eq 0))];
      post = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
      ret = (Ok V|3|); memory_leak = false }
      manifest bugs: []
  

  $ bfa-c gen-summaries manifest.c
  Summaries for load_3:
    { args = [&(V|0|, V|1|)]; pre = []; pc = [(V|0| Eq 0)]; post = [];
      ret = (Error NullDereference at manifest.c:6:10-12 (cursor: 6:10));
      memory_leak = false }
      manifest bugs: []
    { args = [&(V|0|, V|1|)];
      pre = [[(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])]];
      pc =
      [Not((0 Eq V|0|)); (V|3| Leq 2147483647); (-2147483648 Leq V|3|);
        Not((V|0| Eq 0))];
      post = [(V|0|, [TypedVal {offset = V|1|; ty = signed int; v = V|3|}])];
      ret = (Ok V|3|); memory_leak = false }
      manifest bugs: []
  
  Summaries for test_np_uninit_5:
    { args = []; pre = []; pc = []; post = [];
      ret =
      (Error UninitializedMemoryAccess at manifest.c:6:10-12 (cursor: 6:10));
      memory_leak = false }
      manifest bugs: [(Manifest_UB
                         UninitializedMemoryAccess at manifest.c:6:10-12 (cursor: 6:10))]
    { args = []; pre = []; pc = []; post = [];
      ret = (Error NullDereference at manifest.c:6:10-12 (cursor: 6:10));
      memory_leak = false }
      manifest bugs: [(Manifest_UB
                         NullDereference at manifest.c:6:10-12 (cursor: 6:10))]
  
  Summaries for test_leak_11:
    { args = []; pre = []; pc = []; post = []; ret = (Ok 0); memory_leak = true
      }
      manifest bugs: [Memory_leak]
    { args = []; pre = []; pc = []; post = []; ret = (Ok 1);
      memory_leak = false }
      manifest bugs: []
  
  Summaries for test_uninit_8:
    { args = []; pre = []; pc = []; post = [];
      ret =
      (Error UninitializedMemoryAccess at manifest.c:6:10-12 (cursor: 6:10));
      memory_leak = false }
      manifest bugs: [(Manifest_UB
                         UninitializedMemoryAccess at manifest.c:6:10-12 (cursor: 6:10))]
    { args = []; pre = []; pc = []; post = []; ret = (Ok 1);
      memory_leak = false }
      manifest bugs: []
  
  Summaries for test_np_17:
    { args = []; pre = []; pc = []; post = []; ret = (Ok 0);
      memory_leak = false }
      manifest bugs: []
    { args = []; pre = []; pc = []; post = [];
      ret = (Error NullDereference at manifest.c:51:3-10 (cursor: 51:6));
      memory_leak = false }
      manifest bugs: [(Manifest_UB
                         NullDereference at manifest.c:51:3-10 (cursor: 51:6))]
  
  Summaries for test_ok_14:
    { args = []; pre = []; pc = []; post = []; ret = (Ok 0);
      memory_leak = false }
      manifest bugs: []
    { args = []; pre = []; pc = []; post = []; ret = (Ok 1);
      memory_leak = false }
      manifest bugs: []
  
