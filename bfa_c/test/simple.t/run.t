Just reading an empty file
  $ bfa-c exec-main empty.c
  Symex terminated with the following outcomes:
    [Ok (0, {})]
  Executed 2 statements

Symbolic execution of a simple program with concrete values only
  $ bfa-c exec-main conc.c
  Symex terminated with the following outcomes:
    [Ok (2, {})]
  Executed 6 statements

Symbolic execution of a simple program with symbolic values
  $ bfa-c exec-main sym.c
  Symex terminated with the following outcomes:
    [Ok (1, {}); Ok (2, {})]
  Executed 11 statements

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ bfa-c exec-main err.c
  Symex terminated with the following outcomes:
    [Ok
       (0,
        {V|3| ->
           ┌───────────────┬───────────┬─────────┐
           │[0, 4[         │[4, 1024[  │[1024; ∞[│
           ├───────────────┼───────────┼─────────┤
           │12 : signed int│Uninit Tot.│OOB      │
           └───────────────┴───────────┴─────────┘});
     Error NullDereference]
  Executed 5 statements

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ bfa-c exec-main indirections.c
  Symex terminated with the following outcomes:
    [Ok
       (0,
        {V|3| ->
           ┌───────────────┬──────┐
           │[0, 4[         │[4; ∞[│
           ├───────────────┼──────┤
           │12 : signed int│OOB   │
           └───────────────┴──────┘});
     Ok (1, {})]
  Executed 9 statements
  $ bfa-c exec-main cpy.c
  Symex terminated with the following outcomes:
    [Ok
       (1,
        {V|3| ->
           ┌──────────────┬──────────────┬───────────┬───────┐
           │[0, 4[        │[4, 8[        │[8, 12[    │[12; ∞[│
           ├──────────────┼──────────────┼───────────┼───────┤
           │0 : signed int│1 : signed int│Uninit Tot.│OOB    │
           └──────────────┴──────────────┴───────────┴───────┘;
         V|7| ->
           ┌──────────────┬──────────────┬───────────┬───────┐
           │[0, 4[        │[4, 8[        │[8, 12[    │[12; ∞[│
           ├──────────────┼──────────────┼───────────┼───────┤
           │0 : signed int│1 : signed int│Uninit Tot.│OOB    │
           └──────────────┴──────────────┴───────────┴───────┘});
     Ok
       (3,
        {V|3| ->
           ┌──────────────┬──────────────┬───────────┬───────┐
           │[0, 4[        │[4, 8[        │[8, 12[    │[12; ∞[│
           ├──────────────┼──────────────┼───────────┼───────┤
           │0 : signed int│1 : signed int│Uninit Tot.│OOB    │
           └──────────────┴──────────────┴───────────┴───────┘});
     Ok (2, {})]
  Executed 15 statements
