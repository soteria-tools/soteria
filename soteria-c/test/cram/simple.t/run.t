Just reading an empty file
  $ soteria-c exec empty.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 2 statements

Symbolic execution of a simple program with concrete values only
  $ soteria-c exec conc.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (2, { heap = []; globs = [] })]
  Executed 6 statements

Symbolic execution of a simple program with symbolic values
  $ soteria-c exec sym.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (1, { heap = []; globs = [] }); Ok: (2, { heap = []; globs = [] })]
  Executed 11 statements

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ soteria-c exec err.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node =
                [TypedVal {offset = 0; ty = signed int; v = 12};
                 Uninit {offset = 4; len = 1020}; (Bound 1024)];
                info = (Some err.c:5:12-24) })];
            globs = [] });
     Error: Null pointer dereference with trace
            [• Triggering write: err.c:6:3-10 (cursor: 6:6)]]
  Executed 5 statements

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ soteria-c exec indirections.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node =
                [TypedVal {offset = 0; ty = signed int; v = 12}; (Bound 4)];
                info = (Some indirections.c:5:12-31) })];
            globs = [] });
     Ok: (1, { heap = []; globs = [] })]
  Executed 9 statements

Checking that memcpy works correctly
  $ soteria-c exec cpy.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (1,
          { heap =
            [(V|1|,
              { node =
                [TypedVal {offset = 0; ty = signed int; v = 0};
                 TypedVal {offset = 4; ty = signed int; v = 1}; (Bound 8)];
                info = (Some cpy.c:7:12-35) });
             (V|2|,
              { node =
                [TypedVal {offset = 0; ty = signed int; v = 0};
                 TypedVal {offset = 4; ty = signed int; v = 1}; (Bound 8)];
                info = (Some cpy.c:12:12-35) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node =
                [TypedVal {offset = 0; ty = signed int; v = 0};
                 TypedVal {offset = 4; ty = signed int; v = 1}; (Bound 8)];
                info = (Some cpy.c:7:12-35) })];
            globs = [] });
     Ok: (0, { heap = []; globs = [] })]
  Executed 15 statements
Checking that fuel gets exhausted properly
  $ soteria-c exec while_true.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Error: Failed assertion with trace
            [• Called from here: while_true.c:6:5-26;
             • Triggering operation: while_true.c:6:5-26]]
  Executed 152 statements
Checking that code cannot branch infinitely
  $ soteria-c exec max_branching.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|8|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|9|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:7:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:8:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:9:12-31) });
             (V|2|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:10:12-31) });
             (V|3|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:11:12-31) });
             (V|4|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:12:12-31) });
             (V|5|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] })]
  Executed 112 statements

  $ soteria-c exec global.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (1,
          { heap =
            [(V|1|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 1}];
                info = None })];
            globs = [(x_559, V|1|)] })]
  Executed 5 statements
  $ soteria-c exec global_alias.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 0}];
                info = None });
             (V|2|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 0}];
                info = None })];
            globs = [(x_559, V|1|); (y_560, V|2|)] })]
  Executed 3 statements

  $ soteria-c exec structs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|, { node = Freed; info = (Some structs.c:13:3-4) });
             (V|2|, { node = Freed; info = (Some structs.c:17:21-48) })];
            globs = [] });
     Ok: (1,
          { heap = [(V|1|, { node = Freed; info = (Some structs.c:13:3-4) })];
            globs = [] })]
  Executed 16 statements

  $ soteria-c exec short_circuit.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 7 statements

Should return a single branch!
  $ soteria-c exec short_circuit_opt.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (b2i(((0 != V|2|) && (0 != V|1|))), { heap = []; globs = [] })]
  Executed 4 statements

  $ soteria-c exec loop.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (4, { heap = []; globs = [] })]
  Executed 72 statements

  $ soteria-c exec gotos.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (1042, { heap = []; globs = [] });
     Ok: (1043, { heap = []; globs = [] })]
  Executed 23 statements

  $ soteria-c exec duffs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (42, { heap = []; globs = [] })]
  Executed 101 statements

  $ soteria-c exec switch.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (42, { heap = []; globs = [] })]
  Executed 33 statements

  $ soteria-c exec switch_no_match.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (42, { heap = []; globs = [] })]
  Executed 4 statements

  $ soteria-c exec sizeof.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 7 statements

Expected to fail because no main function is defined
  $ soteria-c exec harness.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Error: Parsing Error: Entry point "main" not found with trace []]
  Executed 0 statements

Expected to correctly find the harness function
  $ soteria-c exec harness.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --harness harness
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 2 statements

  $ soteria-c exec float.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node = [TypedVal {offset = 0; ty = float; v = 0.0f}];
                info = None });
             (V|2|,
              { node = [Zeros {offset = 0; len = 8}; (Bound 8)];
                info = (Some float.c:10:23-47) })];
            globs = [(f_560, V|1|)] });
     Ok: (1,
          { heap =
            [(V|1|,
              { node = [TypedVal {offset = 0; ty = float; v = 0.0f}];
                info = None })];
            globs = [(f_560, V|1|)] })]
  Executed 11 statements
 
Check without the proper flag we obtain two branches  
  $ soteria-c exec alloc_cannot_fail.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some alloc_cannot_fail.c:5:19-38) })];
            globs = [] });
     Ok: (1, { heap = []; globs = [] })]
  Executed 7 statements

Check with the proper flag we obtain only one branch
  $ soteria-c exec alloc_cannot_fail.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --alloc-cannot-fail
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node = [Uninit {offset = 0; len = 4}; (Bound 4)];
                info = (Some alloc_cannot_fail.c:5:19-38) })];
            globs = [] })]
  Executed 5 statements

Check that, without proper flag, undefined function calls are not-implemented
  $ soteria-c exec havoc_undef.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    []
  Executed 2 statements

Check that, with proper flag, undefined function calls are havoced. Expecting 2 branches.
  $ soteria-c exec havoc_undef.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --havoc-undef
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] }); Ok: (1, { heap = []; globs = [] })]
  Executed 7 statements

  $ soteria-c exec glob_struct.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node =
                [TypedVal {offset = 0; ty = signed int; v = 0};
                 Uninit {offset = 4; len = 4};
                 TypedVal {offset = 8; ty = signed long; v = 0}];
                info = None });
             (V|2|, { node = Freed; info = (Some glob_struct.c:16:22-23) })];
            globs = [(x_561, V|1|)] })]
  Executed 6 statements
