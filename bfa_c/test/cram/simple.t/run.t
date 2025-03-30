Just reading an empty file
  $ bfa-c exec-main empty.c
  Symex terminated with the following outcomes:
    [Ok: (0, [])]
  Executed 2 statements

Symbolic execution of a simple program with concrete values only
  $ bfa-c exec-main conc.c
  Symex terminated with the following outcomes:
    [Ok: (2, [(V|0|, Freed); (V|1|, Freed)])]
  Executed 6 statements

Symbolic execution of a simple program with symbolic values
  $ bfa-c exec-main sym.c
  Symex terminated with the following outcomes:
    [Ok: (1, [(V|0|, Freed); (V|2|, Freed)]);
     Ok: (2, [(V|0|, Freed); (V|2|, Freed)])]
  Executed 11 statements

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ bfa-c exec-main err.c
  Symex terminated with the following outcomes:
    [Ok: (0,
          [(V|0|, Freed);
           (V|1|,
            [TypedVal {offset = 0; ty = signed int; v = 12};
             Uninit {offset = 4; len = 1020}; (Bound 1024)])]);
     Error: NullDereference with trace [(err.c:6:3-10 (cursor: 6:6),
                                         Triggering memory operation)]]
  Executed 5 statements

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ bfa-c exec-main indirections.c
  Symex terminated with the following outcomes:
    [Ok: (0,
          [(V|0|, Freed);
           (V|1|, [TypedVal {offset = 0; ty = signed int; v = 12}; (Bound 4)])]);
     Ok: (1, [(V|0|, Freed)])]
  Executed 9 statements

Checking that memcpy works correctly
  $ bfa-c exec-main cpy.c
  Symex terminated with the following outcomes:
    [Ok: (1,
          [(V|0|, Freed);
           (V|1|,
            [TypedVal {offset = 0; ty = signed int; v = 0};
             TypedVal {offset = 4; ty = signed int; v = 1};
             Uninit {offset = 8; len = 4}; (Bound 12)]);
           (V|2|, Freed);
           (V|3|,
            [TypedVal {offset = 0; ty = signed int; v = 0};
             TypedVal {offset = 4; ty = signed int; v = 1};
             Uninit {offset = 8; len = 4}; (Bound 12)])]);
     Ok: (3,
          [(V|0|, Freed);
           (V|1|,
            [TypedVal {offset = 0; ty = signed int; v = 0};
             TypedVal {offset = 4; ty = signed int; v = 1};
             Uninit {offset = 8; len = 4}; (Bound 12)]);
           (V|2|, Freed)]);
     Ok: (2, [(V|0|, Freed)])]
  Executed 15 statements
Checking that fuel gets exhausted properly
  $ bfa-c exec-main while_true.c
  Symex terminated with the following outcomes:
    [Error: Failed assertion with trace [(while_true.c:10:5-18, Call trace);
                                         (while_true.c:10:5-18,
                                          Triggering memory operation)]]
  Executed 152 statements
Checking that code cannot branch infinitely
  $ bfa-c exec-main max_branching.c
  Symex terminated with the following outcomes:
    [Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|10|, Freed); (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|12|, Freed); (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|14|, Freed); (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|16|, Freed); (V|17|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|6|, Freed); (V|7|, Freed);
           (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
           (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|11|, Freed);
           (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|13|, Freed);
           (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|15|, Freed);
           (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|4|, Freed); (V|5|, Freed);
           (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
           (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
           (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|11|, Freed);
           (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|13|, Freed);
           (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|15|, Freed);
           (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|4|, Freed); (V|5|, Freed); (V|6|, Freed);
           (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|8|, Freed);
           (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|10|, Freed);
           (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|12|, Freed);
           (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|14|, Freed);
           (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, Freed);
           (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
           (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
           (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
           (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|11|, Freed);
           (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|13|, Freed);
           (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|15|, Freed);
           (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, Freed);
           (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
           (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|10|, Freed); (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|12|, Freed); (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|14|, Freed); (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, Freed); (V|4|, Freed);
           (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|6|, Freed);
           (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|8|, Freed);
           (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|10|, Freed);
           (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|12|, Freed);
           (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|14|, Freed);
           (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|2|, Freed); (V|3|, Freed); (V|4|, Freed); (V|5|, Freed);
           (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
           (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
           (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|11|, Freed);
           (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|13|, Freed);
           (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed);
           (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
           (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
           (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
           (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
           (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|11|, Freed);
           (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|13|, Freed);
           (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|15|, Freed);
           (V|16|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed);
           (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
           (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
           (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|10|, Freed); (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|12|, Freed); (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|14|, Freed); (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed);
           (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
           (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|10|, Freed); (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|12|, Freed); (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|14|, Freed); (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed);
           (V|2|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|3|, Freed);
           (V|4|, Freed); (V|5|, Freed);
           (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
           (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
           (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|11|, Freed);
           (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|13|, Freed);
           (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed);
           (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|4|, Freed);
           (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|6|, Freed);
           (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|8|, Freed);
           (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|10|, Freed);
           (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|12|, Freed);
           (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|14|, Freed);
           (V|15|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed);
           (V|3|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|4|, Freed);
           (V|5|, Freed); (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|7|, Freed); (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|9|, Freed); (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|11|, Freed); (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|13|, Freed); (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed); (V|3|, Freed);
           (V|4|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|5|, Freed);
           (V|6|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|7|, Freed);
           (V|8|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|9|, Freed);
           (V|10|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|11|, Freed);
           (V|12|, [Uninit {offset = 0; len = 4}; (Bound 4)]); (V|13|, Freed);
           (V|14|, [Uninit {offset = 0; len = 4}; (Bound 4)])]);
     Ok: (0,
          [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed); (V|3|, Freed);
           (V|4|, Freed); (V|5|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|6|, Freed); (V|7|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|8|, Freed); (V|9|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|10|, Freed); (V|11|, [Uninit {offset = 0; len = 4}; (Bound 4)]);
           (V|12|, Freed); (V|13|, [Uninit {offset = 0; len = 4}; (Bound 4)])])]
  Executed 112 statements
