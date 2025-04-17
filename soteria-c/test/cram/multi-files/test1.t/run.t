  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __assert__ -> (__assert___486, decl);
    exposed -> (exposed_496, def);
    glob -> (glob_494, def);
    main -> (main_491, def);
    ref -> (ref_495, def);
    test -> (test_487, def);
    update -> (update_489, def)
  
  Declarations:
    glob_483 -> object;
    exposed_484 -> function;
    __assert___486 -> function;
    test_487 -> function;
    update_489 -> function;
    main_491 -> function;
    glob_494 -> object;
    ref_495 -> object;
    exposed_496 -> function
  
  Object definitions:
    (ref_495, &glob)
    (glob_494, 42)
  
  Function definitions:
    test_487
    update_489
    main_491
    exposed_496
  
   Symmap:
    glob_483 -> glob_494;
    exposed_484 -> exposed_496;
    test_487 -> test_487;
    update_489 -> update_489;
    main_491 -> main_491;
    glob_494 -> glob_494;
    ref_495 -> ref_495;
    exposed_496 -> exposed_496
  
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
  
  // declare glob as signed int
  signed int glob = 42;
  
  // declare ref as pointer to signed int
  signed int* ref = &glob;
  
  // declare exposed as function () returning signed int
  signed int exposed()
  {
    return rvalue(*rvalue(ref));
  }

  $ soteria-c exec-main file1.c file2.c -I .
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|0|, [TypedVal {offset = 0; ty = signed int*; v = &(V|1|, 0)}]);
             (V|1|, [TypedVal {offset = 0; ty = signed int; v = 1024}])];
            globs = [(glob_494, V|1|); (ref_495, V|0|)] })]
  Executed 15 statements
