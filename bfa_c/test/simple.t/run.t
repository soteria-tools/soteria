Just reading an empty file
  $ bfa-c exec-main empty.c
  Symex terminated with the following outcomes:
    [Ok (0, {})]

Symbolic execution of a simple program with concrete values only
  $ bfa-c exec-main conc.c
  Symex terminated with the following outcomes:
    [Ok
       (2,
        {(Var (|1|, Svalue.TInt)) -> (Substate
                                        { root =
                                          { node =
                                            (Owned
                                               (Init
                                                  { value = 11; ty = signed int
                                                    }));
                                            range = [0, 4[; children = None };
                                          bound = (Some 4) }); (Var
                                                                  (|3|,
                                                                   Svalue.TInt)) -> (
        Substate
          { root =
            { node = (Owned (Init { value = 12; ty = signed int }));
              range = [0, 4[; children = None };
            bound = (Some 4) })})]

Symbolic execution of a simple program with symbolic values
  $ bfa-c exec-main sym.c
  Symex terminated with the following outcomes:
    [Ok
       (1,
        {(Var (|1|, Svalue.TInt)) -> (Substate
                                        { root =
                                          { node =
                                            (Owned
                                               (Init
                                                  { value =
                                                    (Var (|3|, Svalue.TInt));
                                                    ty = signed int }));
                                            range = [0, 4[; children = None };
                                          bound = (Some 4) }); (Var
                                                                  (|5|,
                                                                   Svalue.TInt)) -> (
        Substate
          { root =
            { node =
              (Owned
                 (Init { value = (Var (|3|, Svalue.TInt)); ty = signed int }));
              range = [0, 4[; children = None };
            bound = (Some 4) })});
     Ok
       (2,
        {(Var (|1|, Svalue.TInt)) -> (Substate
                                        { root =
                                          { node =
                                            (Owned
                                               (Init
                                                  { value =
                                                    (Var (|3|, Svalue.TInt));
                                                    ty = signed int }));
                                            range = [0, 4[; children = None };
                                          bound = (Some 4) }); (Var
                                                                  (|5|,
                                                                   Svalue.TInt)) -> (
        Substate
          { root =
            { node =
              (Owned
                 (Init { value = (Var (|3|, Svalue.TInt)); ty = signed int }));
              range = [0, 4[; children = None };
            bound = (Some 4) })})]

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ bfa-c exec-main err.c
  Symex terminated with the following outcomes:
    [Ok
       (0,
        {(Var (|1|, Svalue.TInt)) -> (Substate
                                        { root =
                                          { node =
                                            (Owned
                                               (Init
                                                  { value =
                                                    (Ptr
                                                       ((Var (|3|, Svalue.TInt)),
                                                        0));
                                                    ty = signed int* }));
                                            range = [0, 8[; children = None };
                                          bound = (Some 8) }); (Var
                                                                  (|3|,
                                                                   Svalue.TInt)) -> (
        Substate
          { root =
            { node = (Owned Lazy); range = [0, 1024[;
              children =
              (Some ({ node = (Owned (Init { value = 12; ty = signed int }));
                       range = [0, 4[; children = None },
                     { node = (Owned (Uninit Totally)); range = [4, 1024[;
                       children = None }))
              };
            bound = (Some 1024) })});
     Error NullDereference]
