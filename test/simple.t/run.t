Just reading an empty file
  $ bfa-c exec-main empty.c -q
  Symex terminated with the following outcomes: [Ok (0, {})]

  $ bfa-c exec-main conc.c -q
  Symex terminated with the following outcomes: [Ok
                                                   (2,
                                                    {(Var (|1|, Svalue.TInt)) -> (
                                                    Substate
                                                      { root =
                                                        { node =
                                                          (Owned
                                                             (Init
                                                                { value = 11;
                                                                  ty =
                                                                  signed int }));
                                                          range = [0, 4[;
                                                          children = None };
                                                        bound = (Some 4) }); (
                                                    Var (|3|, Svalue.TInt)) -> (
                                                    Substate
                                                      { root =
                                                        { node =
                                                          (Owned
                                                             (Init
                                                                { value = 12;
                                                                  ty =
                                                                  signed int }));
                                                          range = [0, 4[;
                                                          children = None };
                                                        bound = (Some 4) })})]

  $ bfa-c exec-main sym.c -q
  Symex terminated with the following outcomes: [Ok
                                                   (1,
                                                    {(Var (|1|, Svalue.TInt)) -> (
                                                    Substate
                                                      { root =
                                                        { node =
                                                          (Owned
                                                             (Init
                                                                { value =
                                                                  (Var
                                                                     (|3|,
                                                                      Svalue.TInt));
                                                                  ty =
                                                                  signed int }));
                                                          range = [0, 4[;
                                                          children = None };
                                                        bound = (Some 4) }); (
                                                    Var (|5|, Svalue.TInt)) -> (
                                                    Substate
                                                      { root =
                                                        { node =
                                                          (Owned
                                                             (Init
                                                                { value =
                                                                  (Var
                                                                     (|3|,
                                                                      Svalue.TInt));
                                                                  ty =
                                                                  signed int }));
                                                          range = [0, 4[;
                                                          children = None };
                                                        bound = (Some 4) })});
                                                 Ok
                                                   (2,
                                                    {(Var (|1|, Svalue.TInt)) -> (
                                                    Substate
                                                      { root =
                                                        { node =
                                                          (Owned
                                                             (Init
                                                                { value =
                                                                  (Var
                                                                     (|3|,
                                                                      Svalue.TInt));
                                                                  ty =
                                                                  signed int }));
                                                          range = [0, 4[;
                                                          children = None };
                                                        bound = (Some 4) }); (
                                                    Var (|5|, Svalue.TInt)) -> (
                                                    Substate
                                                      { root =
                                                        { node =
                                                          (Owned
                                                             (Init
                                                                { value =
                                                                  (Var
                                                                     (|3|,
                                                                      Svalue.TInt));
                                                                  ty =
                                                                  signed int }));
                                                          range = [0, 4[;
                                                          children = None };
                                                        bound = (Some 4) })})]
