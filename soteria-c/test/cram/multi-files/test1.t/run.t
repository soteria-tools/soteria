  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __atomic_add_fetch -> (__atomic_add_fetch_634, decl);
    __atomic_exchange -> (__atomic_exchange_643, decl);
    __atomic_load_n -> (__atomic_load_n_630, decl);
    __atomic_sub_fetch -> (__atomic_sub_fetch_638, decl);
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_601, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_606, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_596, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_615, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_610, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_620, decl);
    __builtin_bswap32 -> (__builtin_bswap32_625, decl);
    __builtin_bswap64 -> (__builtin_bswap64_627, decl);
    __builtin_object_size -> (__builtin_object_size_623, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_575, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_591, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_587, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_579, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_583, decl);
    __soteria___assert -> (__soteria___assert_570, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_568, decl);
    exposed -> (exposed_646, def);
    glob -> (glob_644, def);
    main -> (main_565, def);
    ref -> (ref_645, def);
    test -> (test_561, def);
    update -> (update_563, def)
  
  Declarations:
    __soteria___nondet_int_483 -> function;
    __soteria___assert_485 -> function;
    __builtin_sadd_overflow_490 -> function;
    __builtin_umul_overflow_494 -> function;
    __builtin_umull_overflow_498 -> function;
    __builtin_uaddl_overflow_502 -> function;
    __builtin_ssub_overflow_506 -> function;
    __builtin___memset_chk_511 -> function;
    __builtin___memcpy_chk_516 -> function;
    __builtin___memmove_chk_521 -> function;
    __builtin___strcpy_chk_525 -> function;
    __builtin___snprintf_chk_530 -> function;
    __builtin___strncpy_chk_535 -> function;
    __builtin_object_size_538 -> function;
    __builtin_bswap32_540 -> function;
    __builtin_bswap64_542 -> function;
    __atomic_load_n_545 -> function;
    __atomic_add_fetch_549 -> function;
    __atomic_sub_fetch_553 -> function;
    __atomic_exchange_558 -> function;
    glob_559 -> object;
    exposed_560 -> function;
    test_561 -> function;
    update_563 -> function;
    main_565 -> function;
    __soteria___nondet_int_568 -> function;
    __soteria___assert_570 -> function;
    __builtin_sadd_overflow_575 -> function;
    __builtin_umul_overflow_579 -> function;
    __builtin_umull_overflow_583 -> function;
    __builtin_uaddl_overflow_587 -> function;
    __builtin_ssub_overflow_591 -> function;
    __builtin___memset_chk_596 -> function;
    __builtin___memcpy_chk_601 -> function;
    __builtin___memmove_chk_606 -> function;
    __builtin___strcpy_chk_610 -> function;
    __builtin___snprintf_chk_615 -> function;
    __builtin___strncpy_chk_620 -> function;
    __builtin_object_size_623 -> function;
    __builtin_bswap32_625 -> function;
    __builtin_bswap64_627 -> function;
    __atomic_load_n_630 -> function;
    __atomic_add_fetch_634 -> function;
    __atomic_sub_fetch_638 -> function;
    __atomic_exchange_643 -> function;
    glob_644 -> object;
    ref_645 -> object;
    exposed_646 -> function
  
  Object definitions:
    (ref_645, &glob)
    (glob_644, 42)
  
  Function definitions:
    test_561
    update_563
    main_565
    exposed_646
  
   Symmap:
    glob_644 -> glob_644;
    ref_645 -> ref_645;
    exposed_646 -> exposed_646;
    __soteria___nondet_int_483 -> __soteria___nondet_int_568;
    __soteria___assert_485 -> __soteria___assert_570;
    __builtin_sadd_overflow_490 -> __builtin_sadd_overflow_575;
    __builtin_umul_overflow_494 -> __builtin_umul_overflow_579;
    __builtin_umull_overflow_498 -> __builtin_umull_overflow_583;
    __builtin_uaddl_overflow_502 -> __builtin_uaddl_overflow_587;
    __builtin_ssub_overflow_506 -> __builtin_ssub_overflow_591;
    __builtin___memset_chk_511 -> __builtin___memset_chk_596;
    __builtin___memcpy_chk_516 -> __builtin___memcpy_chk_601;
    __builtin___memmove_chk_521 -> __builtin___memmove_chk_606;
    __builtin___strcpy_chk_525 -> __builtin___strcpy_chk_610;
    __builtin___snprintf_chk_530 -> __builtin___snprintf_chk_615;
    __builtin___strncpy_chk_535 -> __builtin___strncpy_chk_620;
    __builtin_object_size_538 -> __builtin_object_size_623;
    __builtin_bswap32_540 -> __builtin_bswap32_625;
    __builtin_bswap64_542 -> __builtin_bswap64_627;
    __atomic_load_n_545 -> __atomic_load_n_630;
    __atomic_add_fetch_549 -> __atomic_add_fetch_634;
    __atomic_sub_fetch_553 -> __atomic_sub_fetch_638;
    __atomic_exchange_558 -> __atomic_exchange_643;
    glob_559 -> glob_644;
    exposed_560 -> exposed_646;
    test_561 -> test_561;
    update_563 -> update_563;
    main_565 -> main_565
  
  // declare __soteria___nondet_int WITH PROTO as function () returning signed int
  signed int __soteria___nondet_int();
  
  // declare __soteria___assert WITH PROTO as function (signed int) returning void
  void __soteria___assert(signed int);
  
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

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000008;
                   v = &(V|2|, 0x0000000000000000) : signed int*}];
                info = None });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x00000400 : signed int}];
                info = None })];
            globs = [(glob_644, V|2|); (ref_645, V|1|)] })]
  
  Executed 15 statements
  Verification Success!
