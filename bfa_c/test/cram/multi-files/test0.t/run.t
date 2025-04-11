  $ bfa-c show-ail file1.c file2.c -I .
  Redudant globs: 
  Extern idmap:
    get_x -> (get_x_485, def);
    get_xx -> (get_xx_483, def);
    main -> (main_491, def);
    x -> (x_490, def)
  
  Declarations:
    get_xx_483 -> function;
    x_484 -> object;
    get_x_485 -> function;
    get_xx_489 -> function;
    x_490 -> object;
    main_491 -> function
  
  Object definitions:
    (x_484, 12)
    (x_490, 13)
  
  Function definitions:
    get_xx_483
    get_x_485
    main_491
  
   Symmap:
    get_xx_483 -> get_xx_483;
    get_x_485 -> get_x_485;
    get_xx_489 -> get_xx_483;
    x_490 -> x_490;
    main_491 -> main_491
  
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

  $ bfa-c exec-main file1.c file2.c -I .
  Redudant globs: 
  Symex terminated with the following outcomes:
    [Ok: (25,
          { heap =
            [(V|0|, [TypedVal {offset = 0; ty = signed int; v = 12}]);
             (V|1|, [TypedVal {offset = 0; ty = signed int; v = 13}]);
             (V|2|, Freed)];
            globs = [(x_484, V|0|); (x_490, V|1|)] })]
  Executed 7 statements
