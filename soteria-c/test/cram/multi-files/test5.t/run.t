  $ soteria-c show-ail file1.c file2.c file3.c -I .
  Extern idmap:
    __assert__ -> (__assert___489, decl);
    fnA -> (fnA_483, def);
    fnB -> (fnB_486, def);
    main -> (main_492, def)
  
  Declarations:
    fnA_483 -> function;
    fnB_486 -> function;
    __assert___489 -> function;
    fnA_490 -> function;
    fnB_491 -> function;
    main_492 -> function
  
  Object definitions:
    
  
  Function definitions:
    fnA_483
    fnB_486
    main_492
  
   Symmap:
    fnB_486 -> fnB_486;
    fnA_490 -> fnA_483;
    fnB_491 -> fnB_486;
    main_492 -> main_492;
    fnA_483 -> fnA_483
  
  // declare fnA as function () returning signed int
  signed int fnA()
  {
    return 12;
  }
  
  // declare fnB as function () returning signed int
  signed int fnB()
  {
    return 16;
  }
  
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

  $ soteria-c exec-main file1.c file2.c file3.c -I . --no-ignore-parse-failures
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = [(V|0|, Freed)]; globs = [] })]
  Executed 8 statements
