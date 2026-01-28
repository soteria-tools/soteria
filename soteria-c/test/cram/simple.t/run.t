Just reading an empty file
  $ soteria-c exec empty.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, None)]
  
  Executed 2 statements
  Verification Success!

Symbolic execution of a simple program with concrete values only
  $ soteria-c exec conc.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000002, None)]
  
  Executed 6 statements
  Verification Success!

Symbolic execution of a simple program with symbolic values
  $ soteria-c exec sym.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000001, None); Ok: (0x00000002, None)]
  
  Executed 11 statements
  Verification Success!

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ soteria-c exec err.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000c : signed int};
                   info = (Some err.c:5:12-24) }));
             (Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x00000000000003fc; v = SUninit};
                   info = (Some err.c:5:12-24) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000400);
                   info = (Some err.c:5:12-24) }))]);
     Error: (Null pointer dereference with trace
             [• Triggering write: err.c:6:3-10 (cursor: 6:6)], None)]
  error: Null pointer dereference in main
      ┌─ err.c:6:3
    6 │    *x = 12;
      │    ^^^^^^^ Triggering write
  Executed 5 statements
  Verification Failure!
  [13]

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ soteria-c exec indirections.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000c : signed int};
                   info = (Some indirections.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some indirections.c:5:12-31) }))]);
     Ok: (0x00000001, None)]
  
  Executed 9 statements
  Verification Success!

Checking that memcpy works correctly
  $ soteria-c exec cpy.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x1,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SZeros};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000008);
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SZeros};
                   info = (Some cpy.c:12:12-35) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = (Some cpy.c:12:12-35) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000008);
                   info = (Some cpy.c:12:12-35) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SZeros};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000008);
                   info = (Some cpy.c:7:12-35) }))]);
     Ok: (0x00000000, None)]
  
  Executed 15 statements
  Verification Success!
Checking that fuel gets exhausted properly
  $ soteria-c exec while_true.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Error: (Failed assertion with trace
             [• Called from here: while_true.c:6:5-26;
              • Triggering operation: while_true.c:6:5-26],
             None)]
  error: Failed assertion in main
      ┌─ while_true.c:6:5
    6 │      __soteria___assert(0);
      │      ^^^^^^^^^^^^^^^^^^^^^
      │      │
      │      Triggering operation
      │      1: Called from here
  Executed 152 statements
  Verification Failure!
  [13]
Checking that code cannot branch infinitely
  $ soteria-c exec max_branching.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|8|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|8|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|9|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|9|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|8|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|8|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|8|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|8|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|8|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|8|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|8|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|8|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|7|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|7|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|6|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|6|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (V|3|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|3|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (V|4|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|4|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (V|5|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (V|5|,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))])]
  
  Executed 112 statements
  Verification Success!

  $ soteria-c exec global.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000001,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = None }));
             (Ser_globs (x_559, V|1|))])]
  
  Executed 5 statements
  Verification Success!
  $ soteria-c exec global_alias.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000000 : signed int};
                   info = None }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000000 : signed int};
                   info = None }));
             (Ser_globs (x_559, V|1|)); (Ser_globs (y_560, V|2|))])]
  
  Executed 3 statements
  Verification Success!

  $ soteria-c exec structs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap (V|1|, { node = Freed; info = (Some structs.c:13:3-4) }));
             (Ser_heap
                (V|2|, { node = Freed; info = (Some structs.c:17:21-48) }))]);
     Ok: (0x00000001,
          Some
            [(Ser_heap (V|1|, { node = Freed; info = (Some structs.c:13:3-4) }))])]
  
  Executed 16 statements
  Verification Success!

  $ soteria-c exec short_circuit.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, None)]
  
  Executed 7 statements
  Verification Success!

Should return a single branch!
  $ soteria-c exec short_circuit_opt.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (b2bv[4](((V|2| != 0x00000000) && (V|1| != 0x00000000))), None)]
  
  Executed 4 statements
  Verification Success!

  $ soteria-c exec loop.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000004, None)]
  
  Executed 72 statements
  Verification Success!

  $ soteria-c exec gotos.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000412, None); Ok: (0x00000413, None)]
  
  Executed 23 statements
  Verification Success!

  $ soteria-c exec duffs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x0000002a, None)]
  
  Executed 101 statements
  Verification Success!

  $ soteria-c exec switch.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x0000002a, None)]
  
  Executed 33 statements
  Verification Success!

  $ soteria-c exec switch_no_match.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x0000002a, None)]
  
  Executed 4 statements
  Verification Success!

  $ soteria-c exec sizeof.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, None)]
  
  Executed 7 statements
  Verification Success!

Expected to fail because no main function is defined
  $ soteria-c exec harness.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Error: (Parsing Error: Entry point "main" not found with trace [], None)]
  error: Parsing Error: Entry point "main" not found in main
  Executed 0 statements
  Verification Failure!
  [13]

Expected to correctly find the harness function
  $ soteria-c exec harness.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --harness harness --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, None)]
  
  Executed 2 statements
  Verification Success!

  $ soteria-c exec float.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000008; v = 0.0f : float};
                   info = None }));
             (Ser_heap
                (V|2|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000008; v = SZeros};
                   info = (Some float.c:10:23-47) }));
             (Ser_heap
                (V|2|,
                 { node = Bound(0x0000000000000008);
                   info = (Some float.c:10:23-47) }));
             (Ser_globs (f_560, V|1|))]);
     Ok: (0x00000001,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000008; v = 0.0f : float};
                   info = None }));
             (Ser_globs (f_560, V|1|))])]
  
  Executed 11 statements
  Verification Success!
 
Check without the proper flag we obtain two branches  
  $ soteria-c exec alloc_cannot_fail.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some alloc_cannot_fail.c:5:19-38) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some alloc_cannot_fail.c:5:19-38) }))]);
     Ok: (0x00000001, None)]
  
  Executed 7 statements
  Verification Success!

Check with the proper flag we obtain only one branch
  $ soteria-c exec alloc_cannot_fail.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --alloc-cannot-fail --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some alloc_cannot_fail.c:5:19-38) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some alloc_cannot_fail.c:5:19-38) }))])]
  
  Executed 5 statements
  Verification Success!

Check that, without proper flag, undefined function calls are not-implemented
  $ soteria-c exec havoc_undef.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Error: Gave up: Unsupported: Cannot call external function: nondet_int_559]
  error: Analysis gave up: Unsupported: Cannot call external function: nondet_int_559 in main
  Executed 2 statements
  Verification Failure! (Unsupported features)
  [2]

Check that, with proper flag, undefined function calls are havoced. Expecting 2 branches.
  $ soteria-c exec havoc_undef.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --havoc-undef --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, None); Ok: (0x00000001, None)]
  
  Executed 7 statements
  Verification Success!

  $ soteria-c exec glob_struct.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000000 : signed int};
                   info = None }));
             (Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = SUninit};
                   info = None }));
             (Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000008;
                     len = 0x0000000000000008;
                     v = 0x0000000000000000 : signed long};
                   info = None }));
             (Ser_heap
                (V|2|, { node = Freed; info = (Some glob_struct.c:16:22-23) }));
             (Ser_globs (x_561, V|1|))])]
  
  Executed 6 statements
  Verification Success!

Should return -1
  $ soteria-c exec constants.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --use-cerb-headers --print-states
  Symex terminated with the following outcomes:
    [Ok: (0xffffffff, None)]
  
  Executed 4 statements
  Verification Success!

  $ soteria-c exec array0.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap (V|1|, { node = Freed; info = (Some array0.c:5:22-27) }));
             (Ser_heap (V|2|, { node = Freed; info = (Some array0.c:5:34-39) }))])]
  
  Executed 6 statements
  Verification Success!

  $ soteria-c exec strcmp.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap (V|1|, { node = Freed; info = (Some strcmp.c:7:29-30) }));
             (Ser_heap (V|2|, { node = Freed; info = (Some strcmp.c:7:32-33) }))])]
  
  Executed 7 statements
  Verification Success!

  $ soteria-c exec no_unsigned_overflows.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, None)]
  
  Executed 4 statements
  Verification Success!


  $ soteria-c exec memset.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v  --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000028; v = SZeros};
                   info = (Some memset.c:6:12-36) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000028);
                   info = (Some memset.c:6:12-36) }))]);
     Ok: (0x00000000, None)]
  
  Executed 51 statements
  Verification Success!
Does find UB but says Verification Success!
  $ soteria-c exec ignore_ub.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --print-states --ignore-ub
  Symex terminated with the following outcomes:
    [Ok: (0x00,
          Some
            [(Ser_heap
                (V|1|,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000c : signed int};
                   info = (Some ignore_ub.c:5:19-38) }));
             (Ser_heap
                (V|1|,
                 { node = Bound(0x0000000000000004);
                   info = (Some ignore_ub.c:5:19-38) }))]);
     Error: (Null pointer dereference with trace
             [• Triggering write: ignore_ub.c:7:3-10 (cursor: 7:6)], None)]
  
  Executed 5 statements
  Verification Success!
