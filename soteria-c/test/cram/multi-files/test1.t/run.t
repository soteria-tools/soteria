  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___487, decl);
    __soteria_nondet__ -> (__soteria_nondet___495, decl);
    exposed -> (exposed_498, def);
    glob -> (glob_496, def);
    main -> (main_492, def);
    ref -> (ref_497, def);
    test -> (test_488, def);
    update -> (update_490, def)
  
  Declarations:
    __soteria_nondet___483 -> function;
    glob_484 -> object;
    exposed_485 -> function;
    __assert___487 -> function;
    test_488 -> function;
    update_490 -> function;
    main_492 -> function;
    __soteria_nondet___495 -> function;
    glob_496 -> object;
    ref_497 -> object;
    exposed_498 -> function
  
  Object definitions:
    (ref_497, &glob)
    (glob_496, 42)
  
  Function definitions:
    test_488
    update_490
    main_492
    exposed_498
  
   Symmap:
    __soteria_nondet___483 -> __soteria_nondet___495;
    glob_484 -> glob_496;
    exposed_485 -> exposed_498;
    test_488 -> test_488;
    update_490 -> update_490;
    main_492 -> main_492;
    glob_496 -> glob_496;
    ref_497 -> ref_497;
    exposed_498 -> exposed_498
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare glob as signed int
  signed int glob;
  
  // declare exposed WITH PROTO as function () returning signed int
  signed int exposed();
  
  // declare __assert__ WITH PROTO as function (signed int) returning void
  void __assert__(signed int);
  
  // declare test as function () returning signed int
  signed int test()
  {
    return function_decay(exposed)();
  }
  
  // declare update as function () returning void
  void update()
  {
    glob = 1024;
  }
  
  // declare main as function () returning signed int
  signed int main()
  {
    function_decay(__assert__)((function_decay(test)() == 42));
    function_decay(update)();
    function_decay(__assert__)((function_decay(test)() == 1024));
    return 0;
  }
  
  // declare __soteria_nondet__ WITH PROTO as function () returning signed int
  signed int __soteria_nondet__();
  
  // declare glob as signed int
  signed int glob = 42;
  
  // declare ref as pointer to signed int
  signed int* ref = &glob;
  
  // declare exposed as function () returning signed int
  signed int exposed()
  {
    return rvalue(*rvalue(ref));
  }

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|,
              { node =
                [TypedVal {offset = 0; ty = signed int*; v = &(V|2|, 0)}];
                info = None });
             (V|2|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 1024}];
                info = None })];
            globs = [(glob_496, V|2|); (ref_497, V|1|)] })]
  Executed 15 statements
