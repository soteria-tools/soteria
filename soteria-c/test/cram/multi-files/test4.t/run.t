  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___491, decl);
    funcA -> (funcA_492, def);
    funcB -> (funcB_485, def);
    main -> (main_488, def)
  
  Declarations:
    __assert___483 -> function;
    funcA_484 -> function;
    funcB_485 -> function;
    main_488 -> function;
    __assert___491 -> function;
    funcA_492 -> function;
    funcB_493 -> function
  
  Object definitions:
    
  
  Function definitions:
    funcB_485
    main_488
    funcA_492
  
   Symmap:
    __assert___483 -> __assert___491;
    funcA_484 -> funcA_492;
    funcB_485 -> funcB_485;
    main_488 -> main_488;
    funcA_492 -> funcA_492;
    funcB_493 -> funcB_485
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare funcA WITH PROTO as function (signed int) returning signed int
  signed int funcA(signed int);
  
  // declare funcB WITH PROTO as function (signed int) returning signed int
  signed int funcB(signed int x)
  {
    if (rvalue(x) <= 0) {
      return 1;
    }
    else
      ;
    return function_decay(funcA)((rvalue(x) - 1));
  }
  
  // declare main as function () returning signed int
  signed int main()
  {
    function_decay(__assert__)((function_decay(funcA)(5) == 1));
    return 0;
  }
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare funcA WITH PROTO as function (signed int) returning signed int
  signed int funcA(signed int x)
  {
    if (rvalue(x) <= 0) {
      return 0;
    }
    else
      ;
    return function_decay(funcB)((rvalue(x) - 1));
  }
  
  // declare funcB WITH PROTO as function (signed int) returning signed int
  signed int funcB(signed int);

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|0|, Freed); (V|1|, Freed); (V|2|, Freed); (V|3|, Freed);
             (V|4|, Freed); (V|5|, Freed)];
            globs = [] })]
  Executed 27 statements
