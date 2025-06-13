  $ soteria-c show-ail file1.c file2.c file3.c -I .
  Extern idmap:
    __assert__ -> (__assert___492, decl);
    __soteria_nondet__ -> (__soteria_nondet___491, decl);
    fnA -> (fnA_484, def);
    fnB -> (fnB_488, def);
    main -> (main_495, def)
  
  Declarations:
    __soteria_nondet___483 -> function;
    fnA_484 -> function;
    __soteria_nondet___487 -> function;
    fnB_488 -> function;
    __soteria_nondet___491 -> function;
    __assert___492 -> function;
    fnA_493 -> function;
    fnB_494 -> function;
    main_495 -> function
  
  Object definitions:
    
  
  Function definitions:
    fnA_484
    fnB_488
    main_495
  
   Symmap:
    __soteria_nondet___487 -> __soteria_nondet___491;
    fnB_488 -> fnB_488;
    fnA_493 -> fnA_484;
    fnB_494 -> fnB_488;
    main_495 -> main_495;
    __soteria_nondet___483 -> __soteria_nondet___491;
    fnA_484 -> fnA_484
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare fnA as function () returning signed int
  signed int fnA()
  {
    return 12;
  }
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare fnB as function () returning signed int
  signed int fnB()
  {
    return 16;
  }
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare __assert__ WITH PROTO as function (signed int) returning signed int
  signed int __assert__(signed int);
  
  // declare fnA WITH PROTO as function () returning signed int
  signed int fnA();
  
  // declare fnB WITH PROTO as function () returning signed int
  signed int fnB();
  
  // declare main as function () returning signed int
  signed int main()
  {
    signed int ret = function_decay(fnA)() + function_decay(fnB)();
    function_decay(__assert__)((rvalue(ret) == 28));
    return 0;
  }

  $ soteria-c exec-main file1.c file2.c file3.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 8 statements
