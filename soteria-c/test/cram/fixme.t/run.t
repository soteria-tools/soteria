  $ soteria-c gen-summaries global_local_eq.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" ; cat out.summaries
  
  No bugs found
  Summaries for main_560:
    Analysed {
      raw =
      { args = []; pre = [];
        pc =
        [(0x0000000000000000 != V|1|); (0x0000000000000000 != V|2|);
          (V|1| == V|2|)];
        post =
        [(Ser_heap
            (V|2|,
             { node =
               MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                 v = SUninit};
               info = (Some global_local_eq.c:6:28-30 (cursor: 6:28)) }));
          (Ser_heap
             (V|2|,
              { node = Bound(0x0000000000000004);
                info = (Some global_local_eq.c:6:28-30 (cursor: 6:28)) }));
          (Ser_globs (x_559, V|1|))];
        ret =
        (Error (Failed assertion,
                [• Called from here: global_local_eq.c:6:3-31;
                 • Triggering operation: global_local_eq.c:6:3-31]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = []; pre = [];
        pc = [(0x0000000000000000 != V|1|); (V|1| != V|2|)];
        post = [(Ser_globs (x_559, V|1|))]; ret = (Ok 0x00000000) };
      manifest_bugs = []}
  
