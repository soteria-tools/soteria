  $ soteria-c gen-summaries global_local_eq.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Summaries for main_486:
    { args = []; pre = [];
      pc = [(0 Eq IntOfBool(Not((V|0| Eq V|1|)))); Not((0 Eq V|1|))];
      post = { heap = []; globs = [(x_483, V|1|)] };
      ret =
      (Error Failed assertion with trace [(global_local_eq.c:8:3-23,
                                           Call trace);
                                          (global_local_eq.c:8:3-23,
                                           Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: []
    { args = []; pre = [];
      pc = [Not((0 Eq IntOfBool(Not((V|0| Eq V|1|))))); Not((0 Eq V|1|))];
      post = { heap = []; globs = [(x_483, V|1|)] }; ret = (Ok 0);
      memory_leak = false }
      manifest bugs: []
  
