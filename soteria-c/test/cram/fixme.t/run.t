  $ soteria-c gen-summaries global_local_eq.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" && cat out.summaries
  
  No bugs found
  Summaries for main_487:
    Analysed {
      raw =
      { args = []; pre = []; pc = [(V|1| == V|2|); (0 != V|1|); (0 != V|2|)];
        post =
        { heap = [(V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
          globs = [(x_484, V|1|)] };
        ret =
        (Error (Failed assertion,
                [• Called from here: global_local_eq.c:8:3-23;
                 • Triggering memory operation: global_local_eq.c:8:3-23]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = []; pre = []; pc = [(V|1| != V|2|); (0 != V|1|)];
        post = { heap = []; globs = [(x_484, V|1|)] }; ret = (Ok 0) };
      manifest_bugs = []}
  
