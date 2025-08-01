  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __atomic_add_fetch -> (__atomic_add_fetch_636, decl);
    __atomic_exchange -> (__atomic_exchange_645, decl);
    __atomic_load_n -> (__atomic_load_n_632, decl);
    __atomic_sub_fetch -> (__atomic_sub_fetch_640, decl);
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_603, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_608, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_598, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_617, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_612, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_622, decl);
    __builtin_bswap32 -> (__builtin_bswap32_627, decl);
    __builtin_bswap64 -> (__builtin_bswap64_629, decl);
    __builtin_object_size -> (__builtin_object_size_625, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_577, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_593, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_589, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_581, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_585, decl);
    __soteria___assert -> (__soteria___assert_571, decl);
    __soteria___debug_show -> (__soteria___debug_show_572, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_569, decl);
    exposed -> (exposed_648, def);
    glob -> (glob_646, def);
    main -> (main_566, def);
    ref -> (ref_647, def);
    test -> (test_562, def);
    update -> (update_564, def)
  
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
    __atomic_load_n_546 -> function;
    __atomic_add_fetch_550 -> function;
    __atomic_sub_fetch_554 -> function;
    __atomic_exchange_559 -> function;
    glob_560 -> object;
    exposed_561 -> function;
    test_562 -> function;
    update_564 -> function;
    main_566 -> function;
    __soteria___nondet_int_569 -> function;
    __soteria___assert_571 -> function;
    __soteria___debug_show_572 -> function;
    __builtin_sadd_overflow_577 -> function;
    __builtin_umul_overflow_581 -> function;
    __builtin_umull_overflow_585 -> function;
    __builtin_uaddl_overflow_589 -> function;
    __builtin_ssub_overflow_593 -> function;
    __builtin___memset_chk_598 -> function;
    __builtin___memcpy_chk_603 -> function;
    __builtin___memmove_chk_608 -> function;
    __builtin___strcpy_chk_612 -> function;
    __builtin___snprintf_chk_617 -> function;
    __builtin___strncpy_chk_622 -> function;
    __builtin_object_size_625 -> function;
    __builtin_bswap32_627 -> function;
    __builtin_bswap64_629 -> function;
    __atomic_load_n_632 -> function;
    __atomic_add_fetch_636 -> function;
    __atomic_sub_fetch_640 -> function;
    __atomic_exchange_645 -> function;
    glob_646 -> object;
    ref_647 -> object;
    exposed_648 -> function
  
  Object definitions:
    (ref_647, &glob)
    (glob_646, 42)
  
  Function definitions:
    test_562
    update_564
    main_566
    exposed_648
  
   Symmap:
    glob_646 -> glob_646;
    ref_647 -> ref_647;
    exposed_648 -> exposed_648;
    __soteria___nondet_int_483 -> __soteria___nondet_int_569;
    __soteria___assert_485 -> __soteria___assert_571;
    __soteria___debug_show_486 -> __soteria___debug_show_572;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_577;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_581;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_585;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_589;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_593;
    __builtin___memset_chk_512 -> __builtin___memset_chk_598;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_603;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_608;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_612;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_617;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_622;
    __builtin_object_size_539 -> __builtin_object_size_625;
    __builtin_bswap32_541 -> __builtin_bswap32_627;
    __builtin_bswap64_543 -> __builtin_bswap64_629;
    __atomic_load_n_546 -> __atomic_load_n_632;
    __atomic_add_fetch_550 -> __atomic_add_fetch_636;
    __atomic_sub_fetch_554 -> __atomic_sub_fetch_640;
    __atomic_exchange_559 -> __atomic_exchange_645;
    glob_560 -> glob_646;
    exposed_561 -> exposed_648;
    test_562 -> test_562;
    update_564 -> update_564;
    main_566 -> main_566
  
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
  
  // declare __atomic_load_n WITH PROTO as function (pointer to {volatile} signed int, signed int) returning signed int
  signed int __atomic_load_n(volatile signed int*, signed int);
  
  // declare __atomic_add_fetch WITH PROTO as function (pointer to {volatile} signed int, signed int, signed int) returning signed int
  signed int __atomic_add_fetch(volatile signed int*, signed int, signed int);
  
  // declare __atomic_sub_fetch WITH PROTO as function (pointer to {volatile} signed int, signed int, signed int) returning signed int
  signed int __atomic_sub_fetch(volatile signed int*, signed int, signed int);
  
  // declare __atomic_exchange WITH PROTO as function (pointer to {volatile} signed int, pointer to signed int, pointer to signed int, signed int) returning void
  void __atomic_exchange(volatile signed int*, signed int*, signed int*, signed int);
  
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
  
  // declare __atomic_load_n WITH PROTO as function (pointer to {volatile} signed int, signed int) returning signed int
  signed int __atomic_load_n(volatile signed int*, signed int);
  
  // declare __atomic_add_fetch WITH PROTO as function (pointer to {volatile} signed int, signed int, signed int) returning signed int
  signed int __atomic_add_fetch(volatile signed int*, signed int, signed int);
  
  // declare __atomic_sub_fetch WITH PROTO as function (pointer to {volatile} signed int, signed int, signed int) returning signed int
  signed int __atomic_sub_fetch(volatile signed int*, signed int, signed int);
  
  // declare __atomic_exchange WITH PROTO as function (pointer to {volatile} signed int, pointer to signed int, pointer to signed int, signed int) returning void
  void __atomic_exchange(volatile signed int*, signed int*, signed int*, signed int);
  
  // declare glob as signed int
  signed int glob = 42;
  
  // declare ref as pointer to signed int
  signed int* ref = &glob;
  
  // declare exposed as function () returning signed int
  signed int exposed()
  {
    return rvalue(*rvalue(ref));
  }

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
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
            globs = [(glob_646, V|2|); (ref_647, V|1|)] })]
  Executed 15 statements
