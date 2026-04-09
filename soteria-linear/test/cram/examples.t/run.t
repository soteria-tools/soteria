  $ ../../../bin/soteria_linear.exe exec simple.lin
  Parsed program:
    abs -> { name = "abs"; args = ["x"];
             body =
             (If ((Pure_expr (BinOp ((Var "x"), Lt, (Int 0)))),
                (Pure_expr (BinOp ((Int 0), Sub, (Var "x")))),
                (Pure_expr (Var "x"))))
             }
    main -> { name = "main"; args = [];
              body =
              (Let ((Some "x"), (Pure_expr NondetInt),
                 (Call ("abs", [(Var "x")]))))
              }
  Program executed with result:
    [(Ok: ((0 - V|1|), None), [(V|1| <= -1)]);
     (Ok: (V|1|, None), [(0 <= V|1|)])]

  $ ../../../bin/soteria_linear.exe gen-summaries simple.lin
  Specs for main:
    { args = []; pre = []; post = []; pc = [(V|1| <= -1)];
      ret = (Ok (0 - V|1|)) }
    
    { args = []; pre = []; post = []; pc = [(0 <= V|1|)]; ret = (Ok V|1|) }
    
    { args = []; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
  
  Specs for abs:
    { args = [V|1|]; pre = []; post = []; pc = [(V|1| <= -1)];
      ret = (Ok (0 - V|1|)) }
    
    { args = [V|1|]; pre = []; post = []; pc = [(0 <= V|1|)]; ret = (Ok V|1|) }
    
    { args = [V|1|]; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
  
  
  $ ../../../bin/soteria_linear.exe exec compo.lin
  Parsed program:
    load -> { name = "load"; args = ["l"];
              body =
              (Let ((Some "x"), (Load (Var "l")), (Pure_expr (Var "x")))) }
    main -> { name = "main"; args = [];
              body =
              (Let ((Some "l"), Alloc,
                 (Let (None, (Store ((Var "l"), (Int 10))),
                    (Call ("load", [(Var "l")]))))
                 ))
              }
  Program executed with result:
    [(Ok: (10, Some {V|1| -> 10}), [])]

  $ ../../../bin/soteria_linear.exe gen-summaries compo.lin
  Specs for main:
    { args = []; pre = []; post = [(V|1|, 10)]; pc = []; ret = (Ok 10) }
    
    { args = []; pre = []; post = [(V|1|, 10)]; pc = []; ret = (Ok 10) }
    
    { args = []; pre = []; post = [(V|1|, 10)]; pc = [];
      ret = (Error `Interp ("Type error")) }
  
  Specs for load:
    { args = [V|1|]; pre = [(V|1|, V|2|)]; post = [(V|1|, V|2|)]; pc = [];
      ret = (Ok V|2|) }
    
    { args = [V|1|]; pre = [(V|1|, V|2|)]; post = [(V|1|, V|2|)]; pc = [];
      ret = (Ok V|2|) }
    
    { args = [V|1|]; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
  
  
  $ ../../../bin/soteria_linear.exe exec memory.lin
  Parsed program:
    main -> { name = "main"; args = [];
              body =
              (Let ((Some "p"), Alloc,
                 (Let ((Some "q"), Alloc,
                    (Let (None, (Store ((Var "p"), (Int 10))),
                       (Let (None, (Store ((Var "q"), (Int 20))),
                          (Let (None, (Call ("swap", [(Var "p"); (Var "q")])),
                             (Let ((Some "a"), (Load (Var "p")),
                                (Let ((Some "b"), (Load (Var "q")),
                                   (Pure_expr
                                      (BinOp ((Var "a"), Sub, (Var "b"))))
                                   ))
                                ))
                             ))
                          ))
                       ))
                    ))
                 ))
              }
    swap -> { name = "swap"; args = ["p"; "q"];
              body =
              (Let ((Some "x"), (Load (Var "p")),
                 (Let ((Some "y"), (Load (Var "q")),
                    (Let (None, (Store ((Var "p"), (Var "y"))),
                       (Let (None, (Store ((Var "q"), (Var "x"))),
                          (Pure_expr (Int 0))))
                       ))
                    ))
                 ))
              }
  Program executed with result:
    [(Ok: (10, Some {V|1| -> 20;
                     V|2| -> 10}), [Distinct(V|1-2|)])]

  $ ../../../bin/soteria_linear.exe gen-summaries memory.lin
  Specs for main:
    { args = []; pre = []; post = [(V|1|, 10); (V|2|, 20)];
      pc = [Distinct(V|1-2|); (V|1| != V|2|)];
      ret = (Error `Interp ("Type error")) }
    
    { args = []; pre = []; post = [(V|1|, 10); (V|2|, 20)];
      pc = [Distinct(V|1-2|); (V|1| != V|2|)];
      ret = (Error `Interp ("Type error")) }
  
  Specs for swap:
    { args = [V|1|; V|2|]; pre = [(V|1|, V|3|)]; post = [(V|1|, V|3|)];
      pc = [(V|1| == V|2|)]; ret = (Ok 0) }
    
    { args = [V|1|; V|2|]; pre = [(V|1|, V|3|)]; post = [(V|1|, V|3|)];
      pc = [(V|1| == V|2|)]; ret = (Ok 0) }
    
    { args = [V|1|; V|2|]; pre = [(V|1|, V|3|)]; post = [(V|1|, V|3|)];
      pc = []; ret = (Error `Interp ("Type error")) }
    
    { args = [V|1|; V|2|]; pre = [(V|1|, V|3|)]; post = [(V|1|, V|3|)];
      pc = []; ret = (Error `Interp ("Type error")) }
    
    { args = [V|1|; V|2|]; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
    
    { args = [V|1|; V|2|]; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
  
  
  $ ../../../bin/soteria_linear.exe exec multi_arg.lin
  Parsed program:
    add -> { name = "add"; args = ["x"; "y"];
             body = (Pure_expr (BinOp ((Var "x"), Add, (Var "y")))) }
    main -> { name = "main"; args = [];
              body =
              (Let ((Some "a"), (Pure_expr NondetInt),
                 (Let ((Some "b"), (Pure_expr NondetInt),
                    (Let ((Some "c"), (Pure_expr NondetInt),
                       (Let ((Some "r"),
                          (Call ("add", [(Var "a"); (Var "b")])),
                          (Pure_expr (BinOp ((Var "r"), Add, (Var "c"))))))
                       ))
                    ))
                 ))
              }
  Program executed with result:
    [(Ok: ((V|3| + (V|1| + V|2|)), None), [])]

  $ ../../../bin/soteria_linear.exe gen-summaries multi_arg.lin
  Specs for main:
    { args = []; pre = []; post = []; pc = [];
      ret = (Ok ((V|1| + V|2|) + V|3|)) }
    
    { args = []; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
    
    { args = []; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
    
    { args = []; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
  
  Specs for add:
    { args = [V|1|; V|2|]; pre = []; post = []; pc = [];
      ret = (Ok (V|1| + V|2|)) }
    
    { args = [V|1|; V|2|]; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
    
    { args = [V|1|; V|2|]; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
    
    { args = [V|1|; V|2|]; pre = []; post = []; pc = [];
      ret = (Error `Interp ("Type error")) }
  
  
  $ ../../../bin/soteria_linear.exe exec list.lin
  Parsed program:
    append -> { name = "append"; args = ["l"];
                body =
                (If ((Pure_expr (BinOp ((Var "l"), Eq, (Bool false)))), Alloc,
                   (Let ((Some "ll"), (Load (Var "l")),
                      (Let ((Some "t"), (Call ("append", [(Var "ll")])),
                         (Let (None, (Store ((Var "l"), (Var "t"))),
                            (Pure_expr (Var "l"))))
                         ))
                      ))
                   ))
                }
    empty -> { name = "empty"; args = []; body = (Pure_expr (Bool false)) }
    main -> { name = "main"; args = [];
              body =
              (Let ((Some "l"), (Call ("empty", [])),
                 (Let ((Some "l"), (Call ("prepend", [(Var "l")])),
                    (Let ((Some "l"), (Call ("append", [(Var "l")])),
                       (Call ("append", [(Var "l")]))))
                    ))
                 ))
              }
    prepend -> { name = "prepend"; args = ["l"];
                 body =
                 (Let ((Some "n"), Alloc,
                    (Let (None, (Store ((Var "n"), (Var "l"))),
                       (Pure_expr (Var "n"))))
                    ))
                 }
  Program executed with result:
    [(Ok: (V|1|, Some {V|1| -> V|2|;
                       V|2| -> V|3|;
                       V|3| -> false}),
      [Distinct(V|1-2|); Distinct(V|1-3|)])]

  $ ../../../bin/soteria_linear.exe gen-summaries list.lin
  Specs for main:
    
  
  Specs for empty:
    { args = []; pre = []; post = []; pc = []; ret = (Ok false) }
  
  Specs for append:
    { args = [V|1|]; pre = [(V|1|, V|2|)];
      post = [(V|1|, V|3|); (V|3|, false)];
      pc = [(false == V|2|); Distinct(V|3|, V|1|)]; ret = (Ok V|1|) }
    
    { args = [V|1|]; pre = [(V|1|, V|2|)]; post = [(V|1|, V|2|)];
      pc = [(false != V|2|)]; ret = (Error `Interp ("Type error")) }
    
    { args = [V|1|]; pre = []; post = [(V|2|, false)]; pc = [(false == V|1|)];
      ret = (Ok V|2|) }
    
    { args = [V|1|]; pre = []; post = []; pc = [(false != V|1|)];
      ret = (Error `Interp ("Type error")) }
  
  Specs for prepend:
    { args = [V|1|]; pre = []; post = [(V|2|, V|1|)]; pc = []; ret = (Ok V|2|)
      }
    
    { args = [V|1|]; pre = []; post = [(V|2|, V|1|)]; pc = []; ret = (Ok V|2|)
      }
  
  
