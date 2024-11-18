  $ bfa-c exec-main array_remove.c
  Symex terminated with the following outcomes:
    [Error OutOfBounds;
     Ok
       ({ kind = 1; ty = Svalue.TInt },
        {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
        [{ kind = (Var |3|); ty = Svalue.TLoc } ->
        (Alive
           { root =
             { node = (Owned (Uninit Totally));
               range =
               [{ kind = 0; ty = Svalue.TInt }, { kind = 24; ty = Svalue.TInt }[;
               children = None };
             bound = (Some { kind = 24; ty = Svalue.TInt }) });
        [{ kind = (Var |5|); ty = Svalue.TLoc } -> Freed;
       [{ kind = (Var |7|); ty = Svalue.TLoc } ->
       (Alive
          { root =
            { node =
              (Owned
                 (Init
                    { value =
                      { kind =
                        (Ptr
                           ({ kind = (Var |1|); ty = Svalue.TLoc },
                            { kind = 0; ty = Svalue.TInt }));
                        ty = Svalue.TPointer };
                      ty = struct array_s** }));
              range =
              [{ kind = 0; ty = Svalue.TInt }, { kind = 8; ty = Svalue.TInt }[;
              children = None };
            bound = (Some { kind = 8; ty = Svalue.TInt }) });
       [{ kind = (Var |9|); ty = Svalue.TLoc } -> Freed;
     [{ kind = (Var |11|); ty = Svalue.TLoc } -> Freed;
     [{ kind = (Var |13|); ty = Svalue.TLoc } -> Freed});
  Ok
    ({ kind = 1; ty = Svalue.TInt },
     {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed;
     [{ kind = (Var |3|); ty = Svalue.TLoc } ->
     (Alive
        { root =
          { node = (Owned (Uninit Totally));
            range =
            [{ kind = 0; ty = Svalue.TInt }, { kind = 24; ty = Svalue.TInt }[;
            children = None };
          bound = (Some { kind = 24; ty = Svalue.TInt }) });
     [{ kind = (Var |5|); ty = Svalue.TLoc } -> Freed;
    [{ kind = (Var |7|); ty = Svalue.TLoc } ->
    (Alive
       { root =
         { node =
           (Owned
              (Init
                 { value =
                   { kind =
                     (Ptr
                        ({ kind = (Var |1|); ty = Svalue.TLoc },
                         { kind = 0; ty = Svalue.TInt }));
                     ty = Svalue.TPointer };
                   ty = struct array_s** }));
           range =
           [{ kind = 0; ty = Svalue.TInt }, { kind = 8; ty = Svalue.TInt }[;
           children = None };
         bound = (Some { kind = 8; ty = Svalue.TInt }) });
    [{ kind = (Var |9|); ty = Svalue.TLoc } -> Freed});
  Ok
    ({ kind = 1; ty = Svalue.TInt },
     {[{ kind = (Var |1|); ty = Svalue.TLoc } -> Freed})]
