  $ soteria-c gen-summaries global_local_eq.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --dump-summaries "out.summaries" && cat out.summaries
  
  No bugs found
  Summaries for main_545:
    Analysed {
      raw =
      { args = []; pre = []; pc = [(0 != V|1|); (0 != V|2|); (V|1| == V|2|)];
        post =
        { heap =
          [(V|2|,
            { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
              info = (Some global_local_eq.c:6:28-30 (cursor: 6:28)) })];
          globs = [(x_544, V|1|)] };
        ret =
        (Error (Failed assertion,
                [• Called from here: global_local_eq.c:6:3-31;
                 • Triggering operation: global_local_eq.c:6:3-31]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = []; pre = []; pc = [(0 != V|1|); (V|1| != V|2|)];
        post = { heap = []; globs = [(x_484, V|1|)] }; ret = (Ok 0) };
      manifest_bugs = []}
  
