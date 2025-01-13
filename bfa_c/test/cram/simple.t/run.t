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
     Error: NullDereference at err.c:6:3-10 (cursor: 6:6)]
  Executed 5 statements

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ bfa-c exec-main indirections.c
  Symex terminated with the following outcomes:
    [Ok: (0,
          [(V|0|, Freed);
           (V|1|, [TypedVal {offset = 0; ty = signed int; v = 12}; (Bound 4)])]);
     Ok: (1, [(V|0|, Freed)])]
  Executed 9 statements
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
