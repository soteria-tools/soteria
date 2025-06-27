  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_587, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_592, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_582, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_601, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_596, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_606, decl);
    __builtin_bswap32 -> (__builtin_bswap32_611, decl);
    __builtin_bswap64 -> (__builtin_bswap64_613, decl);
    __builtin_object_size -> (__builtin_object_size_609, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_561, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_577, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_573, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_565, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_569, decl);
    __soteria___assert -> (__soteria___assert_555, decl);
    __soteria___debug_show -> (__soteria___debug_show_556, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_553, decl);
    exposed -> (exposed_616, def);
    glob -> (glob_614, def);
    main -> (main_550, def);
    ref -> (ref_615, def);
    test -> (test_546, def);
    update -> (update_548, def)
  
  Declarations:
    __soteria___nondet_int_483 -> function;
    __soteria___assert_485 -> function;
    __soteria___debug_show_486 -> function;
    __builtin_sadd_overflow_491 -> function;
    __builtin_umul_overflow_495 -> function;
    __builtin_umull_overflow_499 -> function;
    __builtin_uaddl_overflow_503 -> function;
    __builtin_ssub_overflow_507 -> function;
    __builtin___memset_chk_512 -> function;
    __builtin___memcpy_chk_517 -> function;
    __builtin___memmove_chk_522 -> function;
    __builtin___strcpy_chk_526 -> function;
    __builtin___snprintf_chk_531 -> function;
    __builtin___strncpy_chk_536 -> function;
    __builtin_object_size_539 -> function;
    __builtin_bswap32_541 -> function;
    __builtin_bswap64_543 -> function;
    glob_544 -> object;
    exposed_545 -> function;
    test_546 -> function;
    update_548 -> function;
    main_550 -> function;
    __soteria___nondet_int_553 -> function;
    __soteria___assert_555 -> function;
    __soteria___debug_show_556 -> function;
    __builtin_sadd_overflow_561 -> function;
    __builtin_umul_overflow_565 -> function;
    __builtin_umull_overflow_569 -> function;
    __builtin_uaddl_overflow_573 -> function;
    __builtin_ssub_overflow_577 -> function;
    __builtin___memset_chk_582 -> function;
    __builtin___memcpy_chk_587 -> function;
    __builtin___memmove_chk_592 -> function;
    __builtin___strcpy_chk_596 -> function;
    __builtin___snprintf_chk_601 -> function;
    __builtin___strncpy_chk_606 -> function;
    __builtin_object_size_609 -> function;
    __builtin_bswap32_611 -> function;
    __builtin_bswap64_613 -> function;
    glob_614 -> object;
    ref_615 -> object;
    exposed_616 -> function
  
  Object definitions:
    (ref_615, &glob)
    (glob_614, 42)
  
  Function definitions:
    test_546
    update_548
    main_550
    exposed_616
  
   Symmap:
    glob_614 -> glob_614;
    ref_615 -> ref_615;
    exposed_616 -> exposed_616;
    __soteria___nondet_int_483 -> __soteria___nondet_int_553;
    __soteria___assert_485 -> __soteria___assert_555;
    __soteria___debug_show_486 -> __soteria___debug_show_556;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_561;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_565;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_569;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_573;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_577;
    __builtin___memset_chk_512 -> __builtin___memset_chk_582;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_587;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_592;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_596;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_601;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_606;
    __builtin_object_size_539 -> __builtin_object_size_609;
    __builtin_bswap32_541 -> __builtin_bswap32_611;
    __builtin_bswap64_543 -> __builtin_bswap64_613;
    glob_544 -> glob_614;
    exposed_545 -> exposed_616;
    test_546 -> test_546;
    update_548 -> update_548;
    main_550 -> main_550
  
  // declare __soteria___nondet_int WITH PROTO as function () returning signed int
  signed int __soteria___nondet_int();
  
  // declare __soteria___assert WITH PROTO as function (signed int) returning void
  void __soteria___assert(signed int);
  
  // declare __soteria___debug_show WITH PROTO as function () returning void
  void __soteria___debug_show();
  
  // declare __builtin_sadd_overflow WITH PROTO as function (signed int, signed int, pointer to signed int) returning _Bool
  _Bool __builtin_sadd_overflow(signed int, signed int, signed int*);
  
  // declare __builtin_umul_overflow WITH PROTO as function (unsigned int, unsigned int, pointer to unsigned int) returning _Bool
  _Bool __builtin_umul_overflow(unsigned int, unsigned int, unsigned int*);
  
  // declare __builtin_umull_overflow WITH PROTO as function (unsigned long, unsigned long, pointer to unsigned long) returning _Bool
  _Bool __builtin_umull_overflow(unsigned long, unsigned long, unsigned long*);
  
  // declare __builtin_uaddl_overflow WITH PROTO as function (unsigned long, unsigned long, pointer to unsigned long) returning _Bool
  _Bool __builtin_uaddl_overflow(unsigned long, unsigned long, unsigned long*);
  
  // declare __builtin_ssub_overflow WITH PROTO as function (signed int, signed int, pointer to signed int) returning _Bool
  _Bool __builtin_ssub_overflow(signed int, signed int, signed int*);
  
  // declare __builtin___memset_chk WITH PROTO as function (pointer to void, signed int, size_t, size_t) returning pointer to void
  void* __builtin___memset_chk(void*, signed int, size_t, size_t);
  
  // declare __builtin___memcpy_chk WITH PROTO as function (pointer to void, pointer to {const} void, size_t, size_t) returning pointer to void
  void* __builtin___memcpy_chk(void*, const void*, size_t, size_t);
  
  // declare __builtin___memmove_chk WITH PROTO as function (pointer to void, pointer to {const} void, size_t, size_t) returning pointer to void
  void* __builtin___memmove_chk(void*, const void*, size_t, size_t);
  
  // declare __builtin___strcpy_chk WITH PROTO as function (pointer to char, pointer to {const} char, size_t) returning pointer to char
  char* __builtin___strcpy_chk(char*, const char*, size_t);
  
  // declare __builtin___snprintf_chk WITH PROTO as variadic function (pointer to char, size_t, signed int, size_t) returning signed int
  signed int __builtin___snprintf_chk(char*, size_t, signed int, size_t, ...);
  
  // declare __builtin___strncpy_chk WITH PROTO as function (pointer to char, pointer to {const} char, size_t, size_t) returning pointer to char
  char* __builtin___strncpy_chk(char*, const char*, size_t, size_t);
  
  // declare __builtin_object_size WITH PROTO as function (pointer to {const} void, signed int) returning size_t
  size_t __builtin_object_size(const void*, signed int);
  
  // declare __builtin_bswap32 WITH PROTO as function (uint32_t) returning uint32_t
  uint32_t __builtin_bswap32(uint32_t);
  
  // declare __builtin_bswap64 WITH PROTO as function (uint64_t) returning uint64_t
  uint64_t __builtin_bswap64(uint64_t);
  
  // declare glob as signed int
  signed int glob;
  
  // declare exposed WITH PROTO as function () returning signed int
  signed int exposed();
  
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
    function_decay(__soteria___assert)((function_decay(test)() == 42));
    function_decay(update)();
    function_decay(__soteria___assert)((function_decay(test)() == 1024));
    return 0;
  }
  
  // declare __soteria___nondet_int WITH PROTO as function () returning signed int
  signed int __soteria___nondet_int();
  
  // declare __soteria___assert WITH PROTO as function (signed int) returning void
  void __soteria___assert(signed int);
  
  // declare __soteria___debug_show WITH PROTO as function () returning void
  void __soteria___debug_show();
  
  // declare __builtin_sadd_overflow WITH PROTO as function (signed int, signed int, pointer to signed int) returning _Bool
  _Bool __builtin_sadd_overflow(signed int, signed int, signed int*);
  
  // declare __builtin_umul_overflow WITH PROTO as function (unsigned int, unsigned int, pointer to unsigned int) returning _Bool
  _Bool __builtin_umul_overflow(unsigned int, unsigned int, unsigned int*);
  
  // declare __builtin_umull_overflow WITH PROTO as function (unsigned long, unsigned long, pointer to unsigned long) returning _Bool
  _Bool __builtin_umull_overflow(unsigned long, unsigned long, unsigned long*);
  
  // declare __builtin_uaddl_overflow WITH PROTO as function (unsigned long, unsigned long, pointer to unsigned long) returning _Bool
  _Bool __builtin_uaddl_overflow(unsigned long, unsigned long, unsigned long*);
  
  // declare __builtin_ssub_overflow WITH PROTO as function (signed int, signed int, pointer to signed int) returning _Bool
  _Bool __builtin_ssub_overflow(signed int, signed int, signed int*);
  
  // declare __builtin___memset_chk WITH PROTO as function (pointer to void, signed int, size_t, size_t) returning pointer to void
  void* __builtin___memset_chk(void*, signed int, size_t, size_t);
  
  // declare __builtin___memcpy_chk WITH PROTO as function (pointer to void, pointer to {const} void, size_t, size_t) returning pointer to void
  void* __builtin___memcpy_chk(void*, const void*, size_t, size_t);
  
  // declare __builtin___memmove_chk WITH PROTO as function (pointer to void, pointer to {const} void, size_t, size_t) returning pointer to void
  void* __builtin___memmove_chk(void*, const void*, size_t, size_t);
  
  // declare __builtin___strcpy_chk WITH PROTO as function (pointer to char, pointer to {const} char, size_t) returning pointer to char
  char* __builtin___strcpy_chk(char*, const char*, size_t);
  
  // declare __builtin___snprintf_chk WITH PROTO as variadic function (pointer to char, size_t, signed int, size_t) returning signed int
  signed int __builtin___snprintf_chk(char*, size_t, signed int, size_t, ...);
  
  // declare __builtin___strncpy_chk WITH PROTO as function (pointer to char, pointer to {const} char, size_t, size_t) returning pointer to char
  char* __builtin___strncpy_chk(char*, const char*, size_t, size_t);
  
  // declare __builtin_object_size WITH PROTO as function (pointer to {const} void, signed int) returning size_t
  size_t __builtin_object_size(const void*, signed int);
  
  // declare __builtin_bswap32 WITH PROTO as function (uint32_t) returning uint32_t
  uint32_t __builtin_bswap32(uint32_t);
  
  // declare __builtin_bswap64 WITH PROTO as function (uint64_t) returning uint64_t
  uint64_t __builtin_bswap64(uint64_t);
  
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
            globs = [(glob_614, V|2|); (ref_615, V|1|)] })]
  Executed 15 statements
