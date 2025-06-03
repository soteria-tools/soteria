  $ soteria-c gen-summaries global_local_eq.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --no-progress-bar --dump-summaries "out.summaries" && cat out.summaries
  Summaries for main_487:
    Analysed {
      raw =
      { args = []; pre = []; pc = [(V|1| == V|2|); (0 != V|2|); (0 != V|1|)];
        post =
        { heap = [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
          globs = [(x_484, V|2|)] };
        ret =
        (Error (Failed assertion,
                [(global_local_eq.c:8:3-23, Call trace);
                 (global_local_eq.c:8:3-23, Triggering memory operation)]))
        };
      manifest_bugs = []}
    Analysed {
      raw =
      { args = []; pre = []; pc = [(V|1| != V|2|); (0 != V|2|)];
        post = { heap = []; globs = [(x_484, V|2|)] }; ret = (Ok 0) };
      manifest_bugs = []}
  
