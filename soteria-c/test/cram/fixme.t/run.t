  $ soteria-c gen-summaries global_local_eq.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
<<<<<<< HEAD
  Summaries for main_486:
    { args = []; pre = []; pc = [(V|0| == V|1|); (0 != V|1|); (0 != V|0|)];
      post =
      { heap = [(V|0|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
        globs = [(x_483, V|1|)] };
=======
  Summaries for main_487:
    { args = []; pre = [];
      pc = [(0 Eq IntOfBool(Not((V|0| Eq V|1|)))); Not((0 Eq V|1|))];
      post = { heap = []; globs = [(x_484, V|1|)] };
>>>>>>> f8274ffd (short-circuit and tests)
      ret =
      (Error Failed assertion with trace [(global_local_eq.c:8:3-23,
                                           Call trace);
                                          (global_local_eq.c:8:3-23,
                                           Triggering memory operation)]);
      memory_leak = false }
      manifest bugs: []
<<<<<<< HEAD
    { args = []; pre = []; pc = [(V|0| != V|1|); (0 != V|1|)];
      post = { heap = []; globs = [(x_483, V|1|)] }; ret = (Ok 0);
=======
    { args = []; pre = [];
      pc = [Not((0 Eq IntOfBool(Not((V|0| Eq V|1|))))); Not((0 Eq V|1|))];
      post = { heap = []; globs = [(x_484, V|1|)] }; ret = (Ok 0);
>>>>>>> f8274ffd (short-circuit and tests)
      memory_leak = false }
      manifest bugs: []
  
