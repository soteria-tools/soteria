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
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000c : signed int};
                   info = (Some err.c:5:12-24) }));
             (Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x00000000000003fc; v = SUninit};
                   info = (Some err.c:5:12-24) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000400);
                   info = (Some err.c:5:12-24) }))]);
     Error: (Null pointer dereference with trace
             [• Invalid memory write: err.c:6:3-10 (cursor: 6:6)], None)]
  
  error: Null pointer dereference in main
      ┌─ err.c:6:3
    6 │    *x = 12;
      │    ^^^^^^^ Invalid memory write
  Executed 5 statements
  Verification Failure!
  [13]

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ soteria-c exec indirections.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000c : signed int};
                   info = (Some indirections.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some indirections.c:5:12-31) }))]);
     Ok: (0x00000001, None)]
  
  Executed 9 statements
  Verification Success!

Checking that memcpy works correctly
  $ soteria-c exec cpy.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000001,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SZeros};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000008);
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SZeros};
                   info = (Some cpy.c:12:12-35) }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = (Some cpy.c:12:12-35) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Bound(0x0000000000000008);
                   info = (Some cpy.c:12:12-35) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SZeros};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = (Some cpy.c:7:12-35) }));
             (Ser_heap
                (0x0000000000000001,
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
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000003,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000003,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000004,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000004,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000005,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000005,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000006,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000006,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000007,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000007,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000008,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000008,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000009,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000009,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000003,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000003,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x000000000000000a,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000000a,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000000b,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000000b,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000000c,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000000c,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000000d,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000000d,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000000e,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x000000000000000e,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x000000000000000f,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x000000000000000f,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000010,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000010,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000011,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000011,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000012,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000012,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000013,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000013,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000014,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000014,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000015,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000015,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000016,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000016,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000017,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000017,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000018,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000018,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000019,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000019,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x000000000000001a,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x000000000000001a,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x000000000000001b,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x000000000000001b,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x000000000000001c,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000001c,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000001d,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000001d,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000001e,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000001e,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000001f,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000001f,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000020,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000020,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x000000000000001a,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x000000000000001a,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000021,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000021,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000022,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000022,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000023,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000023,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000024,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000024,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000025,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000025,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000026,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000026,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000027,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000027,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000028,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000028,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000029,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000029,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000002a,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000002a,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000002b,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x000000000000002b,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:5:12-31) }));
             (Ser_heap
                (0x000000000000002c,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000002c,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000002d,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000002d,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000002e,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000002e,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000002f,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000002f,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000030,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000030,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000031,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000031,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000032,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000032,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000033,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000033,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000034,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000034,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000035,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000035,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000036,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000036,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000037,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000037,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000038,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000038,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000031,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000031,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000032,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000032,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000039,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000039,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000003a,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000003a,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000003b,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000003b,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000003c,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000003c,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000003d,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x000000000000003d,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000031,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000031,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x000000000000003e,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x000000000000003e,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x000000000000003f,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000003f,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000040,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000040,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000041,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000041,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000042,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000042,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000043,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000043,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000031,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000031,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:6:12-31) }));
             (Ser_heap
                (0x0000000000000044,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000044,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000045,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000045,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000046,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000046,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000047,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000047,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000048,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000048,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000049,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000049,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x000000000000004a,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x000000000000004a,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x000000000000004b,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000004b,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000004c,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000004c,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000004d,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000004d,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000004e,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000004e,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000004f,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x000000000000004f,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000049,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000049,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:7:12-31) }));
             (Ser_heap
                (0x0000000000000050,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000050,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000051,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000051,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000052,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000052,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000053,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000053,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000054,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x0000000000000054,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000055,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000055,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:8:12-31) }));
             (Ser_heap
                (0x0000000000000056,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000056,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x0000000000000057,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000057,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x0000000000000058,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000058,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x0000000000000059,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x0000000000000059,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000005a,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x000000000000005a,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))]);
     Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x000000000000005b,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000005b,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:9:12-31) }));
             (Ser_heap
                (0x000000000000005c,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000005c,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:10:12-31) }));
             (Ser_heap
                (0x000000000000005d,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000005d,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:11:12-31) }));
             (Ser_heap
                (0x000000000000005e,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000005e,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:12:12-31) }));
             (Ser_heap
                (0x000000000000005f,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some max_branching.c:13:12-31) }));
             (Ser_heap
                (0x000000000000005f,
                 { node = Bound(0x0000000000000004);
                   info = (Some max_branching.c:13:12-31) }))])]
  
  Executed 112 statements
  Verification Success!

  $ soteria-c exec global.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000001,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000001 : signed int};
                   info = None }));
             (Ser_globs (x_559, 0x0000000000000001))])]
  
  Executed 5 statements
  Verification Success!
  $ soteria-c exec global_alias.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000000 : signed int};
                   info = None }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000000 : signed int};
                   info = None }));
             (Ser_globs (x_559, 0x0000000000000001));
             (Ser_globs (y_560, 0x0000000000000002))])]
  
  Executed 3 statements
  Verification Success!

  $ soteria-c exec structs.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node = Freed; info = (Some structs.c:13:3-4) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Freed; info = (Some structs.c:17:21-48) }))]);
     Ok: (0x00000001,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node = Freed; info = (Some structs.c:13:3-4) }))])]
  
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
    [Ok: (b2bv[32](((V|2| != 0x00000000) && (V|1| != 0x00000000))), None)]
  
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
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000008; v = 0.0f : float};
                   info = None }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000008; v = SZeros};
                   info = (Some float.c:10:23-47) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Bound(0x0000000000000008);
                   info = (Some float.c:10:23-47) }));
             (Ser_globs (f_560, 0x0000000000000001))]);
     Ok: (0x00000001,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000008; v = 0.0f : float};
                   info = None }));
             (Ser_globs (f_560, 0x0000000000000001))])]
  
  Executed 11 statements
  Verification Success!
 
Check without the proper flag we obtain two branches  
  $ soteria-c exec alloc_cannot_fail.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some alloc_cannot_fail.c:5:19-38) }));
             (Ser_heap
                (0x0000000000000001,
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
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = SUninit};
                   info = (Some alloc_cannot_fail.c:5:19-38) }));
             (Ser_heap
                (0x0000000000000001,
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
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x00000000 : signed int};
                   info = None }));
             (Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000004;
                     len = 0x0000000000000004; v = SUninit};
                   info = None }));
             (Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000008;
                     len = 0x0000000000000008;
                     v = 0x0000000000000000 : signed long};
                   info = None }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Freed; info = (Some glob_struct.c:16:22-23) }));
             (Ser_globs (x_561, 0x0000000000000001))])]
  
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
            [(Ser_heap
                (0x0000000000000001,
                 { node = Freed; info = (Some array0.c:5:22-27) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Freed; info = (Some array0.c:5:34-39) }))])]
  
  Executed 6 statements
  Verification Success!

  $ soteria-c exec strcmp.c --no-ignore-parse-failures --no-ignore-duplicate-symbols -v --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node = Freed; info = (Some strcmp.c:7:29-30) }));
             (Ser_heap
                (0x0000000000000002,
                 { node = Freed; info = (Some strcmp.c:7:32-33) }))])]
  
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
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000028; v = SZeros};
                   info = (Some memset.c:6:12-36) }));
             (Ser_heap
                (0x0000000000000001,
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
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000c : signed int};
                   info = (Some ignore_ub.c:5:19-38) }));
             (Ser_heap
                (0x0000000000000001,
                 { node = Bound(0x0000000000000004);
                   info = (Some ignore_ub.c:5:19-38) }))]);
     Error: (Null pointer dereference with trace
             [• Invalid memory write: ignore_ub.c:7:3-10 (cursor: 7:6)], 
             None)]
  
  Executed 5 statements
  Verification Success!
