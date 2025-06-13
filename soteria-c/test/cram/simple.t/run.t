Just reading an empty file
  $ soteria-c exec-main empty.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 2 statements

Symbolic execution of a simple program with concrete values only
  $ soteria-c exec-main conc.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (2, { heap = []; globs = [] })]
  Executed 6 statements

Symbolic execution of a simple program with symbolic values
  $ soteria-c exec-main sym.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (1, { heap = []; globs = [] }); Ok: (2, { heap = []; globs = [] })]
  Executed 11 statements

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ soteria-c exec-main err.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 12};
               Uninit {offset = 4; len = 1020}; (Bound 1024)])];
            globs = [] });
     Error: Null pointer dereference with trace
            [• Triggering memory operation: err.c:6:3-10 (cursor: 6:6)]]
  Executed 5 statements

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ soteria-c exec-main indirections.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 12}; (Bound 4)])];
            globs = [] });
     Ok: (1, { heap = []; globs = [] })]
  Executed 9 statements

Checking that memcpy works correctly
  $ soteria-c exec-main cpy.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (1,
          { heap =
            [(V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 0};
               TypedVal {offset = 4; ty = signed int; v = 1};
               Uninit {offset = 8; len = 4}; (Bound 12)]);
             (V|2|,
              [TypedVal {offset = 0; ty = signed int; v = 0};
               TypedVal {offset = 4; ty = signed int; v = 1};
               Uninit {offset = 8; len = 4}; (Bound 12)])];
            globs = [] });
     Ok: (3,
          { heap =
            [(V|1|,
              [TypedVal {offset = 0; ty = signed int; v = 0};
               TypedVal {offset = 4; ty = signed int; v = 1};
               Uninit {offset = 8; len = 4}; (Bound 12)])];
            globs = [] });
     Ok: (2, { heap = []; globs = [] })]
  Executed 15 statements
Checking that fuel gets exhausted properly
  $ soteria-c exec-main while_true.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Error: Failed assertion with trace
            [• Called from here: while_true.c:10:5-18;
             • Triggering memory operation: while_true.c:10:5-18]]
  Executed 152 statements
Checking that code cannot branch infinitely
  $ soteria-c exec-main max_branching.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
             (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)])];
            globs = [] })]
  Executed 112 statements

  $ soteria-c exec-main global.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (1,
          { heap = [(V|1|, [TypedVal {offset = 0; ty = signed int; v = 1}])];
            globs = [(x_484, V|1|)] })]
  Executed 5 statements
  $ soteria-c exec-main global_alias.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|, [TypedVal {offset = 0; ty = signed int; v = 0}]);
             (V|2|, [TypedVal {offset = 0; ty = signed int; v = 0}])];
            globs = [(x_586, V|1|); (y_587, V|2|)] })]
  Executed 3 statements

  $ soteria-c exec-main structs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = [(V|1|, Freed); (V|2|, Freed)]; globs = [] });
     Ok: (1, { heap = [(V|1|, Freed)]; globs = [] })]
  Executed 16 statements

  $ soteria-c exec-main short_circuit.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 7 statements
Should return a single branch!
  $ soteria-c exec-main short_circuit_opt.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (b2i(((0 != V|2|) && (0 != V|1|))), { heap = []; globs = [] })]
  Executed 4 statements
