  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __soteria_nondet__ -> (__soteria_nondet___490, decl);
    get_x -> (get_x_486, def);
    get_xx -> (get_xx_484, def);
    main -> (main_493, def);
    x -> (x_492, def)
  
  Declarations:
    __soteria_nondet___483 -> function;
    get_xx_484 -> function;
    x_485 -> object;
    get_x_486 -> function;
    __soteria_nondet___490 -> function;
    get_xx_491 -> function;
    x_492 -> object;
    main_493 -> function
  
  Object definitions:
    (x_492, 13)
    (x_485, 12)
  
  Function definitions:
    get_xx_484
    get_x_486
    main_493
  
   Symmap:
    __soteria_nondet___483 -> __soteria_nondet___490;
    get_xx_484 -> get_xx_484;
    get_x_486 -> get_x_486;
    get_xx_491 -> get_xx_484;
    x_492 -> x_492;
    main_493 -> main_493
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare get_xx WITH PROTO as function () returning signed int
  signed int get_xx()
  {
    return function_decay(get_x)();
  }
  
  // declare x as signed int
  signed int x = 12;
  
  // declare get_x as function () returning signed int
  signed int get_x()
  {
    return rvalue(x);
  }
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare get_xx WITH PROTO as function () returning signed int
  signed int get_xx();
  
  // declare x as signed int
  signed int x = 13;
  
  // declare main as function () returning signed int
  signed int main()
  {
    signed int ret = function_decay(get_xx)();
    return rvalue(ret) + rvalue(x);
  }

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (25,
          { heap =
            [(V|1|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 13}];
                info = None });
             (V|2|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 12}];
                info = None })];
            globs = [(x_485, V|2|); (x_492, V|1|)] })]
  Executed 7 statements
