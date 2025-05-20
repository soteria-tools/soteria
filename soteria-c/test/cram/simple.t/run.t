Just reading an empty file
  $ soteria-c exec-main empty.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 2 statements

Symbolic execution of a simple program with concrete values only
  $ soteria-c exec-main conc.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (2, { heap = [(V|0|, Freed); (V|1|, Freed)]; globs = [] })]
  Executed 6 statements

Symbolic execution of a simple program with symbolic values
  $ soteria-c exec-main sym.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (1, { heap = [(V|0|, Freed); (V|2|, Freed)]; globs = [] });
     Ok: (2, { heap = [(V|0|, Freed); (V|2|, Freed)]; globs = [] })]
  Executed 11 statements

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ soteria-c exec-main err.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|0|, Freed);
             (V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 12};
               Uninit {offset = 4; len = 1020}; (Bound 1024)])];
            globs = [] });
     Error: NullDereference with trace [(err.c:6:3-10 (cursor: 6:6),
                                         Triggering memory operation)]]
  Executed 5 statements

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ soteria-c exec-main indirections.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|0|, Freed);
             (V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 12}; (Bound 4)])];
            globs = [] });
     Ok: (1, { heap = [(V|0|, Freed)]; globs = [] })]
  Executed 9 statements

Checking that memcpy works correctly
  $ soteria-c exec-main cpy.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (1,
          { heap =
            [(V|0|, Freed);
             (V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 0};
               TypedVal {offset = 4; ty = signed int; v = 1};
               Uninit {offset = 8; len = 4}; (Bound 12)]);
             (V|2|, Freed);
             (V|3|,
              [TypedVal {offset = 0; ty = signed int; v = 0};
               TypedVal {offset = 4; ty = signed int; v = 1};
               Uninit {offset = 8; len = 4}; (Bound 12)])];
            globs = [] });
     Ok: (3,
          { heap =
            [(V|0|, Freed);
             (V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 0};
               TypedVal {offset = 4; ty = signed int; v = 1};
               Uninit {offset = 8; len = 4}; (Bound 12)]);
             (V|2|, Freed)];
            globs = [] });
     Ok: (2, { heap = [(V|0|, Freed)]; globs = [] })]
  Executed 15 statements
Checking that fuel gets exhausted properly
  $ soteria-c exec-main while_true.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Error: Failed assertion with trace [(while_true.c:10:5-18, Call trace);
                                         (while_true.c:10:5-18,
                                          Triggering memory operation)]]
  Executed 152 statements
Checking that code cannot branch infinitely
  $ soteria-c exec-main max_branching.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|14|, Freed);
             (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|16|, Freed);
             (V|17|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, Freed); (V|7|, Freed);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
             (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|15|, Freed);
             (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, Freed); (V|5|, Freed);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
             (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|15|, Freed);
             (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, Freed); (V|5|, Freed); (V|6|, Freed);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|8|, Freed);
             (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|14|, Freed);
             (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, Freed);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
             (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|15|, Freed);
             (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, Freed);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
             (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|14|, Freed);
             (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, Freed); (V|4|, Freed);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|6|, Freed);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|8|, Freed);
             (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|14|, Freed);
             (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, Freed); (V|3|, Freed); (V|4|, Freed); (V|5|, Freed);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
             (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
             (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|15|, Freed);
             (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
             (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|14|, Freed);
             (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
             (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|14|, Freed);
             (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
             (V|4|, Freed); (V|5|, Freed);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
             (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|4|, Freed);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|6|, Freed);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|8|, Freed);
             (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|14|, Freed);
             (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|4|, Freed);
             (V|5|, Freed); (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, Freed); (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|9|, Freed); (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed); (V|3|, Freed);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
             (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|11|, Freed);
             (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|13|, Freed);
             (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed); (V|3|, Freed);
             (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|10|, Freed);
             (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|12|, Freed);
             (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] })]
  Executed 112 statements

  $ soteria-c exec-main global.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (1,
          { heap = [(V|0|, [TypedVal {offset = 0; ty = signed int; v = 1}])];
            globs = [(x_483, V|0|)] })]
  Executed 5 statements
  $ soteria-c exec-main global_alias.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|0|, [TypedVal {offset = 0; ty = signed int; v = 0}]);
             (V|1|, [TypedVal {offset = 0; ty = signed int; v = 0}])];
            globs = [(x_585, V|0|); (y_586, V|1|)] })]
  Executed 3 statements

  $ soteria-c exec-main structs.c --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap = [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed)]; globs = [] });
     Ok: (1, { heap = [(V|0|, Freed); (V|1|, Freed)]; globs = [] })]
  Executed 16 statements
