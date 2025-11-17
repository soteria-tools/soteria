Just reading an empty file
  $ soteria-c exec empty.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  Executed 2 statements

Symbolic execution of a simple program with concrete values only
  $ soteria-c exec conc.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000002, { heap = []; globs = [] })]
  Executed 6 statements

Symbolic execution of a simple program with symbolic values
  $ soteria-c exec sym.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000001, { heap = []; globs = [] });
     Ok: (0x00000002, { heap = []; globs = [] })]
  Executed 11 statements

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ soteria-c exec err.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x0000000c : signed int};
                 MemVal {offset = 0x0000000000000004; len = 0x00000000000003fc;
                   v = SUninit};
                 Bound(0x0000000000000400)];
                info = (Some err.c:5:12-24) })];
            globs = [] });
     Error: Null pointer dereference with trace
            [• Triggering write: err.c:6:3-10 (cursor: 6:6)]]
  Executed 5 statements

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ soteria-c exec indirections.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x0000000c : signed int};
                 Bound(0x0000000000000004)];
                info = (Some indirections.c:5:12-31) })];
            globs = [] });
     Ok: (0x00000001, { heap = []; globs = [] })]
  Executed 9 statements

Checking that memcpy works correctly
  $ soteria-c exec cpy.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0b1,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SZeros};
                 MemVal {offset = 0x0000000000000004; len = 0x0000000000000004;
                   v = 0x00000001 : signed int};
                 Bound(0x0000000000000008)];
                info = (Some cpy.c:7:12-35) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SZeros};
                 MemVal {offset = 0x0000000000000004; len = 0x0000000000000004;
                   v = 0x00000001 : signed int};
                 Bound(0x0000000000000008)];
                info = (Some cpy.c:12:12-35) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SZeros};
                 MemVal {offset = 0x0000000000000004; len = 0x0000000000000004;
                   v = 0x00000001 : signed int};
                 Bound(0x0000000000000008)];
                info = (Some cpy.c:7:12-35) })];
            globs = [] });
     Ok: (0x00000000, { heap = []; globs = [] })]
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
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|8|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|9|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:5:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|8|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:6:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|7|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:7:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:8:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|6|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] });
     Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:9:12-31) });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:10:12-31) });
             (V|3|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:11:12-31) });
             (V|4|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:12:12-31) });
             (V|5|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some max_branching.c:13:12-31) })];
            globs = [] })]
  Executed 112 statements

  $ soteria-c exec global.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000001,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x00000001 : signed int}];
                info = None })];
            globs = [(x_559, V|1|)] })]
  Executed 5 statements
  $ soteria-c exec global_alias.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x00000000 : signed int}];
                info = None });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x00000000 : signed int}];
                info = None })];
            globs = [(x_559, V|1|); (y_560, V|2|)] })]
  Executed 3 statements

  $ soteria-c exec structs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|, { node = Freed; info = (Some structs.c:13:3-4) });
             (V|2|, { node = Freed; info = (Some structs.c:17:21-48) })];
            globs = [] });
     Ok: (0x00000001,
          { heap = [(V|1|, { node = Freed; info = (Some structs.c:13:3-4) })];
            globs = [] })]
  Executed 16 statements

  $ soteria-c exec short_circuit.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  Executed 7 statements

Should return a single branch!
  $ soteria-c exec short_circuit_opt.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (b2bv[1](((V|2| != 0x00000000) && (V|1| != 0x00000000))),
          { heap = []; globs = [] })]
  Executed 4 statements

  $ soteria-c exec loop.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000004, { heap = []; globs = [] })]
  Executed 72 statements

  $ soteria-c exec gotos.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000412, { heap = []; globs = [] });
     Ok: (0x00000413, { heap = []; globs = [] })]
  Executed 23 statements

  $ soteria-c exec duffs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x0000002a, { heap = []; globs = [] })]
  Executed 101 statements

  $ soteria-c exec switch.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x0000002a, { heap = []; globs = [] })]
  Executed 33 statements

  $ soteria-c exec switch_no_match.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x0000002a, { heap = []; globs = [] })]
  Executed 4 statements

  $ soteria-c exec sizeof.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  Executed 7 statements

Expected to fail because no main function is defined
  $ soteria-c exec harness.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Error: Parsing Error: Entry point "main" not found with trace []]
  Executed 0 statements

Expected to correctly find the harness function
  $ soteria-c exec harness.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --harness harness
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  Executed 2 statements

  $ soteria-c exec float.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000008;
                   v = 0.0f : float}];
                info = None });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000008;
                   v = SZeros};
                 Bound(0x0000000000000008)];
                info = (Some float.c:10:23-47) })];
            globs = [(f_560, V|1|)] });
     Ok: (0x00000001,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000008;
                   v = 0.0f : float}];
                info = None })];
            globs = [(f_560, V|1|)] })]
  Executed 11 statements
 
Check without the proper flag we obtain two branches  
  $ soteria-c exec alloc_cannot_fail.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some alloc_cannot_fail.c:5:19-38) })];
            globs = [] });
     Ok: (0x00000001, { heap = []; globs = [] })]
  Executed 7 statements

Check with the proper flag we obtain only one branch
  $ soteria-c exec alloc_cannot_fail.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --alloc-cannot-fail
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = SUninit};
                 Bound(0x0000000000000004)];
                info = (Some alloc_cannot_fail.c:5:19-38) })];
            globs = [] })]
  Executed 5 statements

Check that, without proper flag, undefined function calls are not-implemented
  $ soteria-c exec havoc_undef.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Error: Gave up: MISSING FEATURE, VANISHING: Cannot call external function: nondet_int_559]
  Executed 2 statements

Check that, with proper flag, undefined function calls are havoced. Expecting 2 branches.
  $ soteria-c exec havoc_undef.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --havoc-undef
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] });
     Ok: (0x00000001, { heap = []; globs = [] })]
  Executed 7 statements

  $ soteria-c exec glob_struct.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x00000000 : signed int};
                 MemVal {offset = 0x0000000000000004; len = 0x0000000000000004;
                   v = SUninit};
                 MemVal {offset = 0x0000000000000008; len = 0x0000000000000008;
                   v = 0x0000000000000000 : signed long}];
                info = None });
             (V|2|, { node = Freed; info = (Some glob_struct.c:16:22-23) })];
            globs = [(x_561, V|1|)] })]
  Executed 6 statements

Should return -1
  $ soteria-c exec constants.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --use-cerb-headers
  Symex terminated with the following outcomes:
    [Ok: (0xffffffff, { heap = []; globs = [] })]
  Executed 4 statements

  $ soteria-c exec array0.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|, { node = Freed; info = (Some array0.c:5:22-27) });
             (V|2|, { node = Freed; info = (Some array0.c:5:34-39) })];
            globs = [] })]
  Executed 6 statements

  $ soteria-c exec strcmp.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|, { node = Freed; info = (Some strcmp.c:7:29-30) });
             (V|2|, { node = Freed; info = (Some strcmp.c:7:32-33) })];
            globs = [] })]
  Executed 7 statements

  $ soteria-c exec no_unsigned_overflows.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  Executed 4 statements


  $ soteria-c exec memset.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v 
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000028;
                   v = SZeros};
                 Bound(0x0000000000000028)];
                info = (Some memset.c:6:12-36) })];
            globs = [] });
     Ok: (0x00000000, { heap = []; globs = [] })]
  Executed 51 statements
