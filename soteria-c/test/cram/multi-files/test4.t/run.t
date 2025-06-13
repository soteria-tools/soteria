  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___493, decl);
    __soteria_nondet__ -> (__soteria_nondet___492, decl);
    funcA -> (funcA_494, def);
    funcB -> (funcB_486, def);
    main -> (main_489, def)
  
  Declarations:
    __soteria_nondet___483 -> function;
    __assert___484 -> function;
    funcA_485 -> function;
    funcB_486 -> function;
    main_489 -> function;
    __soteria_nondet___492 -> function;
    __assert___493 -> function;
    funcA_494 -> function;
    funcB_495 -> function
  
  Object definitions:
    
  
  Function definitions:
    funcB_486
    main_489
    funcA_494
  
   Symmap:
    __soteria_nondet___483 -> __soteria_nondet___492;
    __assert___484 -> __assert___493;
    funcA_485 -> funcA_494;
    funcB_486 -> funcB_486;
    main_489 -> main_489;
    funcA_494 -> funcA_494;
    funcB_495 -> funcB_486
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
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
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
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

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 27 statements
