  $ soteria-c gen-summaries load.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" ; cat out.summaries
  
  No bugs found
  Summaries for f_560:
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)]; pre = []; pc = [(0x0000000000000000 == V|1|)];
        post = [];
        ret =
        (Error (Null pointer dereference,
                [• Invalid memory load: load.c:3:10-12 (cursor: 3:10)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)];
        pre =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|3| : signed int};
               info = None }))
          ];
        pc = [(0x0000000000000000 != V|1|); (V|2| <=u 0x7ffffffffffffffb)];
        post =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|3| : signed int};
               info = None }))
          ];
        ret = (Ok V|3|) };
      manifest_bugs = []}
  
NO_COLOR=true is necessary to avoid test output changing in CI. For some reason, Grace doesn't prints a final caret at the end with color, and not without color.
  $ NO_COLOR=true soteria-c gen-summaries manifest.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" ; cat out.summaries
  
  error: Null pointer dereference in test_np
      ┌─ manifest.c:51:3
   51 │    *x = 12;
      │    ^^^^^^^ Invalid memory write
  error: Accessing uninitialized memory in test_uninit
      ┌─ manifest.c:6:10
    6 │    return *x;
      │           ^^ Invalid memory load
      ·  
   21 │    load(x);
      │    ------- 1: Called from here
  error: Accessing uninitialized memory in test_np_uninit
      ┌─ manifest.c:6:10
    6 │    return *x;
      │           ^^ Invalid memory load
      ·  
   12 │    load(x);
      │    ------- 1: Called from here
  error: Null pointer dereference in test_np_uninit
      ┌─ manifest.c:6:10
    6 │    return *x;
      │           ^^ Invalid memory load
      ·  
   12 │    load(x);
      │    ------- 1: Called from here
  warning: Memory leak in test_leak
      ┌─ manifest.c:26:1
   25 │    
   26 │ ╭  int test_leak()
   27 │ │  {
   28 │ │    int *x = (int *)malloc(sizeof(int));
      │ │                    ------------------- 1: This allocation leaked
      · │  
   33 │ │    return 0;
   34 │ │  }
      │ ╰──^ 
   35 │    
  Summaries for load_563:
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)]; pre = []; pc = [(0x0000000000000000 == V|1|)];
        post = [];
        ret =
        (Error (Null pointer dereference,
                [• Invalid memory load: manifest.c:6:10-12 (cursor: 6:10)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|)];
        pre =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|3| : signed int};
               info = None }))
          ];
        pc = [(0x0000000000000000 != V|1|); (V|2| <=u 0x7ffffffffffffffb)];
        post =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|3| : signed int};
               info = None }))
          ];
        ret = (Ok V|3|) };
      manifest_bugs = []}
  
  Summaries for test_ok_574:
    Analysed {
      raw = { args = []; pre = []; pc = []; post = []; ret = (Ok 0x00000000) };
      manifest_bugs = []}
    Analysed {
      raw = { args = []; pre = []; pc = []; post = []; ret = (Ok 0x00000001) };
      manifest_bugs = []}
  
  Summaries for test_np_577:
    Analysed {
      raw = { args = []; pre = []; pc = []; post = []; ret = (Ok 0x00000000) };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = [];
        ret =
        (Error (Null pointer dereference,
                [• Invalid memory write: manifest.c:51:3-10 (cursor: 51:6)]))
        };
      manifest_bugs =
      [(Null pointer dereference,
        [• Invalid memory write: manifest.c:51:3-10 (cursor: 51:6)])]}
  
  Summaries for test_uninit_568:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = [];
        ret =
        (Error (Accessing uninitialized memory,
                [• Called from here: manifest.c:21:3-10;
                 • Invalid memory load: manifest.c:6:10-12 (cursor: 6:10)]))
        };
      manifest_bugs =
      [(Accessing uninitialized memory,
        [• Called from here: manifest.c:21:3-10;
         • Invalid memory load: manifest.c:6:10-12 (cursor: 6:10)])]}
    Analysed {
      raw = { args = []; pre = []; pc = []; post = []; ret = (Ok 0x00000001) };
      manifest_bugs = []}
  
  Summaries for test_np_uninit_565:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = [];
        ret =
        (Error (Accessing uninitialized memory,
                [• Called from here: manifest.c:12:3-10;
                 • Invalid memory load: manifest.c:6:10-12 (cursor: 6:10)]))
        };
      manifest_bugs =
      [(Accessing uninitialized memory,
        [• Called from here: manifest.c:12:3-10;
         • Invalid memory load: manifest.c:6:10-12 (cursor: 6:10)])]}
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = [];
        ret =
        (Error (Null pointer dereference,
                [• Called from here: manifest.c:12:3-10;
                 • Invalid memory load: manifest.c:6:10-12 (cursor: 6:10)]))
        };
      manifest_bugs =
      [(Null pointer dereference,
        [• Called from here: manifest.c:12:3-10;
         • Invalid memory load: manifest.c:6:10-12 (cursor: 6:10)])]}
  
  Summaries for test_leak_571:
    Analysed {
      raw = { args = []; pre = []; pc = []; post = []; ret = (Ok 0x00000000) };
      manifest_bugs =
      [(Memory leak,
        [• This allocation leaked: manifest.c:28:19-38;
         • manifest.c:26:1-34:2 (cursor: 26:5 - 26:14)])]}
    Analysed {
      raw = { args = []; pre = []; pc = []; post = []; ret = (Ok 0x00000001) };
      manifest_bugs = []}
  
The following test case is for regression testing.
if%sat1 had the wrong semantics and would not correctly backtrack.
  $ soteria-c gen-summaries if_sat_one_ok.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" ; cat out.summaries
  
  No bugs found
  Summaries for test_561:
    Analysed {
      raw =
      { args = [V|1|; &(V|2|, V|3|)]; pre = [];
        pc =
        [(0x00000001 <=u V|1|); (V|1| <=u 0x7fffffff);
          (0x0000000000000000 == V|2|)];
        post = [];
        ret =
        (Error (Null pointer dereference,
                [• Invalid memory load: if_sat_one_ok.c:6:12-14 (cursor: 6:12)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [V|1|; &(V|2|, V|3|)];
        pre =
        [(Ser_heap
            (V|2|,
             { node =
               MemVal {offset = V|3|; len = 0x0000000000000004;
                 v = V|4| : signed int};
               info = None }))
          ];
        pc =
        [(0x0000000000000000 != V|2|); (0x00000001 <=u V|1|);
          (V|1| <=u 0x7fffffff); (V|3| <=u 0x7ffffffffffffffb)];
        post =
        [(Ser_heap
            (V|2|,
             { node =
               MemVal {offset = V|3|; len = 0x0000000000000004;
                 v = V|4| : signed int};
               info = None }))
          ];
        ret = (Ok V|4|) };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [V|1|; &(V|2|, V|3|)]; pre = [];
        pc = [((0x7fffffff <u V|1|) || (V|1| == 0x00000000))]; post = [];
        ret = (Ok 0x00000000) };
      manifest_bugs = []}
  
  $ soteria-c gen-summaries array_iter.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" ; cat out.summaries
  
  No bugs found
  Summaries for test_561:
    Analysed {
      raw =
      { args = [&(V|1|, V|2|); V|3|]; pre = [];
        pc = [((V|3| == 0x00000000) || (0x7fffffff <u V|3|))]; post = [];
        ret = (Ok 0x00000000) };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|); V|3|]; pre = [];
        pc =
        [(0x00000001 <=u V|3|); (V|3| <=u 0x7fffffff);
          (0x0000000000000000 == V|1|)];
        post = [];
        ret =
        (Error (Null pointer dereference,
                [• Invalid memory load: array_iter.c:6:12-16]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|); V|3|];
        pre =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|4| : signed int};
               info = None }))
          ];
        pc =
        [(0x0000000000000000 != V|1|); (V|2| <=u 0x7ffffffffffffffb);
          (V|3| == 0x00000001); (V|3| == 0x00000001)];
        post =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|4| : signed int};
               info = None }))
          ];
        ret = (Ok V|4|) };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|); V|3|];
        pre =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = (V|2| +ck 0x0000000000000004);
                 len = 0x0000000000000004; v = V|5| : signed int};
               info = None }));
          (Ser_heap
             (V|1|,
              { node =
                MemVal {offset = V|2|; len = 0x0000000000000004;
                  v = V|4| : signed int};
                info = None }))
          ];
        pc =
        [(0x0000000000000000 != V|1|); (V|4| +s_ovf V|5|);
          (V|2| <=u 0x7ffffffffffffff7); (0x00000002 <=u V|3|);
          (V|3| <=u 0x7fffffff)];
        post =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|4| : signed int};
               info = None }));
          (Ser_heap
             (V|1|,
              { node =
                MemVal {offset = (V|2| +ck 0x0000000000000004);
                  len = 0x0000000000000004; v = V|5| : signed int};
                info = None }))
          ];
        ret =
        (Error (Integer overflow,
                [• Triggering operation: array_iter.c:6:5-16 (cursor: 6:9)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [&(V|1|, V|2|); V|3|];
        pre =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = (V|2| +ck 0x0000000000000004);
                 len = 0x0000000000000004; v = V|5| : signed int};
               info = None }));
          (Ser_heap
             (V|1|,
              { node =
                MemVal {offset = V|2|; len = 0x0000000000000004;
                  v = V|4| : signed int};
                info = None }))
          ];
        pc =
        [(0x0000000000000000 != V|1|); !((V|4| +s_ovf V|5|));
          (V|2| <=u 0x7ffffffffffffff7); (V|3| == 0x00000002);
          (V|3| == 0x00000002)];
        post =
        [(Ser_heap
            (V|1|,
             { node =
               MemVal {offset = V|2|; len = 0x0000000000000004;
                 v = V|4| : signed int};
               info = None }));
          (Ser_heap
             (V|1|,
              { node =
                MemVal {offset = (V|2| +ck 0x0000000000000004);
                  len = 0x0000000000000004; v = V|5| : signed int};
                info = None }))
          ];
        ret = (Ok (V|4| +ck V|5|)) };
      manifest_bugs = []}
  
  $ soteria-c gen-summaries overflow.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" ; cat out.summaries
  
  error: Integer overflow in add_ovf_manifest
      ┌─ overflow.c:9:11
    9 │    int c = b + 1;
      │            ^^^^^ Triggering operation
  Summaries for add_561:
    Analysed {
      raw =
      { args = [V|1|; V|2|]; pre = []; pc = [(V|1| +s_ovf V|2|)]; post = [];
        ret =
        (Error (Integer overflow,
                [• Triggering operation: overflow.c:3:10-15 (cursor: 3:12)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = [V|1|; V|2|]; pre = []; pc = [!((V|1| +s_ovf V|2|))]; post = [];
        ret = (Ok (V|1| +ck V|2|)) };
      manifest_bugs = []}
  
  Summaries for add_ovf_manifest_564:
    Analysed {
      raw =
      { args = [V|1|]; pre = []; pc = []; post = [];
        ret =
        (Error (Integer overflow,
                [• Triggering operation: overflow.c:9:11-16 (cursor: 9:13)]))
        };
      manifest_bugs =
      [(Integer overflow,
        [• Triggering operation: overflow.c:9:11-16 (cursor: 9:13)])]}
  
