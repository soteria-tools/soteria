  $ soteria-c exec-main array_add.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Error: Buffer overflow or underflow with trace
            [• Called from here: array_add.c:127:7-31;
             • Triggering memory operation: array_add.c:79:3-33 (cursor: 79:24)];
     Ok: (1,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
             (V|2|, Freed); (V|3|, Freed)];
            globs = [] });
     Ok: (1,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
             (V|2|, Freed)];
            globs = [] });
     Ok: (1, { heap = []; globs = [] })]
  Executed 174 statements
