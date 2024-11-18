Just reading an empty file
  $ bfa-c exec-main empty.c
  Symex terminated with the following outcomes:
    [Ok ({ kind = 0; ty = Svalue.TInt }, {})]

Symbolic execution of a simple program with concrete values only
  $ bfa-c exec-main conc.c
  Symex terminated with the following outcomes:
    [Ok
       ({ kind = 2; ty = Svalue.TInt },
        {[{ kind = (Var |1|); ty = Svalue.TLoc } ->
         (Alive
            { root =
              { node =
                (Owned
                   (Init
                      { value = { kind = 11; ty = Svalue.TInt };
                        ty = signed int }));
                range =
                [{ kind = 0; ty = Svalue.TInt }, { kind = 4; ty = Svalue.TInt }[;
                children = None };
              bound = (Some { kind = 4; ty = Svalue.TInt }) });
        [{ kind = (Var |3|); ty = Svalue.TLoc } ->
        (Alive
           { root =
             { node =
               (Owned
                  (Init
                     { value = { kind = 12; ty = Svalue.TInt }; ty = signed int
                       }));
               range =
               [{ kind = 0; ty = Svalue.TInt }, { kind = 4; ty = Svalue.TInt }[;
               children = None };
             bound = (Some { kind = 4; ty = Svalue.TInt }) })})]

Symbolic execution of a simple program with symbolic values
  $ bfa-c exec-main sym.c
  Symex terminated with the following outcomes:
    [Ok
       ({ kind = 1; ty = Svalue.TInt },
        {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
        [{ kind = (Var |5|); ty = Svalue.TLoc } ->
        (Alive
           { root =
             { node =
               (Owned
                  (Init
                     { value = { kind = (Var |3|); ty = Svalue.TInt };
                       ty = signed int }));
               range =
               [{ kind = 0; ty = Svalue.TInt }, { kind = 4; ty = Svalue.TInt }[;
               children = None };
             bound = (Some { kind = 4; ty = Svalue.TInt }) })});
  Ok
    ({ kind = 2; ty = Svalue.TInt },
     {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
     [{ kind = (Var |5|); ty = Svalue.TLoc } ->
     (Alive
        { root =
          { node =
            (Owned
               (Init
                  { value = { kind = (Var |3|); ty = Svalue.TInt };
                    ty = signed int }));
            range =
            [{ kind = 0; ty = Svalue.TInt }, { kind = 4; ty = Svalue.TInt }[;
            children = None };
          bound = (Some { kind = 4; ty = Svalue.TInt }) })})]

Symbolic execution of a simple program with symbolic values that fails because of an allocation error
  $ bfa-c exec-main err.c
  Symex terminated with the following outcomes:
    [Ok
       ({ kind = 0; ty = Svalue.TInt },
        {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
        [{ kind = (Var |3|); ty = Svalue.TLoc } ->
        (Alive
           { root =
             { node = (Owned Lazy);
               range =
               [{ kind = 0; ty = Svalue.TInt }, { kind = 1024; ty = Svalue.TInt
                                                  }[;
               children =
               (Some ({ node =
                        (Owned
                           (Init
                              { value = { kind = 12; ty = Svalue.TInt };
                                ty = signed int }));
                        range =
                        [{ kind = 0; ty = Svalue.TInt }, { kind = 4;
                                                           ty = Svalue.TInt }[;
                        children = None },
                      { node = (Owned (Uninit Totally));
                        range =
                        [{ kind = 4; ty = Svalue.TInt }, { kind = 1024;
                                                           ty = Svalue.TInt }[;
                        children = None }))
               };
             bound = (Some { kind = 1024; ty = Svalue.TInt }) })});
  Error NullDereference]

Symbolic execution of a simple program with a horrible pointer indirection *&*x
  $ bfa-c exec-main indirections.c
  Symex terminated with the following outcomes:
    [Ok
       ({ kind = 0; ty = Svalue.TInt },
        {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
        [{ kind = (Var |3|); ty = Svalue.TLoc } ->
        (Alive
           { root =
             { node =
               (Owned
                  (Init
                     { value = { kind = 12; ty = Svalue.TInt }; ty = signed int
                       }));
               range =
               [{ kind = 0; ty = Svalue.TInt }, { kind = 4; ty = Svalue.TInt }[;
               children = None };
             bound = (Some { kind = 4; ty = Svalue.TInt }) })});
  Ok
    ({ kind = 1; ty = Svalue.TInt },
     {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed})]
  $ bfa-c exec-main cpy.c
  Symex terminated with the following outcomes:
    [Ok
       ({ kind = 1; ty = Svalue.TInt },
        {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
        [{ kind = (Var |3|); ty = Svalue.TLoc } ->
        (Alive
           { root =
             { node = (Owned Lazy);
               range =
               [{ kind = 0; ty = Svalue.TInt }, { kind = 12; ty = Svalue.TInt }[;
               children =
               (Some ({ node =
                        (Owned
                           (Init
                              { value = { kind = 0; ty = Svalue.TInt };
                                ty = signed int }));
                        range =
                        [{ kind = 0; ty = Svalue.TInt }, { kind = 4;
                                                           ty = Svalue.TInt }[;
                        children = None },
                      { node = (Owned Lazy);
                        range =
                        [{ kind = 4; ty = Svalue.TInt }, { kind = 12;
                                                           ty = Svalue.TInt }[;
                        children =
                        (Some ({ node =
                                 (Owned
                                    (Init
                                       { value = { kind = 1; ty = Svalue.TInt };
                                         ty = signed int }));
                                 range =
                                 [{ kind = 4; ty = Svalue.TInt }, { kind = 8;
                                                                    ty =
                                                                    Svalue.TInt
                                                                    }[;
                                 children = None },
                               { node = (Owned (Uninit Totally));
                                 range =
                                 [{ kind = 8; ty = Svalue.TInt }, { kind = 12;
                                                                    ty =
                                                                    Svalue.TInt
                                                                    }[;
                                 children = None }))
                        }))
               };
             bound = (Some { kind = 12; ty = Svalue.TInt }) });
        [{ kind = (Var |5|); ty = Svalue.TLoc } -> Freed;
       [{ kind = (Var |7|); ty = Svalue.TLoc } ->
       (Alive
          { root =
            { node = (Owned Lazy);
              range =
              [{ kind = 0; ty = Svalue.TInt }, { kind = 12; ty = Svalue.TInt }[;
              children =
              (Some ({ node = (Owned Lazy);
                       range =
                       [{ kind = 0; ty = Svalue.TInt }, { kind = 8;
                                                          ty = Svalue.TInt }[;
                       children =
                       (Some ({ node =
                                (Owned
                                   (Init
                                      { value = { kind = 0; ty = Svalue.TInt };
                                        ty = signed int }));
                                range =
                                [{ kind = 0; ty = Svalue.TInt }, { kind = 4;
                                                                   ty =
                                                                   Svalue.TInt
                                                                   }[;
                                children = None },
                              { node =
                                (Owned
                                   (Init
                                      { value = { kind = 1; ty = Svalue.TInt };
                                        ty = signed int }));
                                range =
                                [{ kind = 4; ty = Svalue.TInt }, { kind = 8;
                                                                   ty =
                                                                   Svalue.TInt
                                                                   }[;
                                children = None }))
                       },
                     { node = (Owned (Uninit Totally));
                       range =
                       [{ kind = 8; ty = Svalue.TInt }, { kind = 12;
                                                          ty = Svalue.TInt }[;
                       children = None }))
              };
            bound = (Some { kind = 12; ty = Svalue.TInt }) })});
  Ok
    ({ kind = 3; ty = Svalue.TInt },
     {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
     [{ kind = (Var |3|); ty = Svalue.TLoc } ->
     (Alive
        { root =
          { node = (Owned Lazy);
            range =
            [{ kind = 0; ty = Svalue.TInt }, { kind = 12; ty = Svalue.TInt }[;
            children =
            (Some ({ node =
                     (Owned
                        (Init
                           { value = { kind = 0; ty = Svalue.TInt };
                             ty = signed int }));
                     range =
                     [{ kind = 0; ty = Svalue.TInt }, { kind = 4;
                                                        ty = Svalue.TInt }[;
                     children = None },
                   { node = (Owned Lazy);
                     range =
                     [{ kind = 4; ty = Svalue.TInt }, { kind = 12;
                                                        ty = Svalue.TInt }[;
                     children =
                     (Some ({ node =
                              (Owned
                                 (Init
                                    { value = { kind = 1; ty = Svalue.TInt };
                                      ty = signed int }));
                              range =
                              [{ kind = 4; ty = Svalue.TInt }, { kind = 8;
                                                                 ty =
                                                                 Svalue.TInt }[;
                              children = None },
                            { node = (Owned (Uninit Totally));
                              range =
                              [{ kind = 8; ty = Svalue.TInt }, { kind = 12;
                                                                 ty =
                                                                 Svalue.TInt }[;
                              children = None }))
                     }))
            };
          bound = (Some { kind = 12; ty = Svalue.TInt }) });
     [{ kind = (Var |5|); ty = Svalue.TLoc } -> Freed});
  Ok
    ({ kind = 2; ty = Svalue.TInt },
     {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed})]
