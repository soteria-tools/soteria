  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___491, decl);
    compute -> (compute_485, def);
    main -> (main_493, def)
  
  Declarations:
    __assert___484 -> function;
    compute_485 -> function;
    helper_486 -> function;
    __assert___491 -> function;
    compute_492 -> function;
    main_493 -> function
  
  Object definitions:
    
  
  Function definitions:
    compute_485
    helper_486
    main_493
  
   Symmap:
    compute_492 -> compute_485;
    main_493 -> main_493;
    __assert___484 -> __assert___491;
    compute_485 -> compute_485
  
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

  $ soteria-c exec-main file1.c file2.c -I .
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = [(V|0|, Freed)]; globs = [] })]
  Executed 8 statements
