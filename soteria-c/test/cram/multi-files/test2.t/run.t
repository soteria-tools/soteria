  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___493, decl);
    __soteria_nondet__ -> (__soteria_nondet___491, decl);
    compute -> (compute_486, def);
    main -> (main_495, def)
  
  Declarations:
    __soteria_nondet___483 -> function;
    __assert___485 -> function;
    compute_486 -> function;
    helper_487 -> function;
    __soteria_nondet___491 -> function;
    __assert___493 -> function;
    compute_494 -> function;
    main_495 -> function
  
  Object definitions:
    
  
  Function definitions:
    compute_486
    helper_487
    main_495
  
   Symmap:
    compute_494 -> compute_486;
    main_495 -> main_495;
    __soteria_nondet___483 -> __soteria_nondet___491;
    __assert___485 -> __assert___493;
    compute_486 -> compute_486
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare compute WITH PROTO as function () returning signed int
  signed int compute()
  {
    return 5 + function_decay(helper)();
  }
  
  // declare helper as function () returning signed int
  signed int helper()
  {
    return 7;
  }
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare compute WITH PROTO as function () returning signed int
  signed int compute();
  
  // declare main as function () returning signed int
  signed int main()
  {
    signed int ret = function_decay(compute)();
    function_decay(__assert__)((rvalue(ret) == 12));
    return 0;
  }

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 8 statements
