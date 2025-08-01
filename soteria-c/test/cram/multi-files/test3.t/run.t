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
    __soteria___assert -> (__soteria___assert_569, decl);
    __soteria___debug_show -> (__soteria___debug_show_570, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_567, decl);
    add -> (add_562, def);
    main -> (main_656, def);
    operate -> (operate_651, def)
  
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
    add_562 -> function;
    __soteria___nondet_int_567 -> function;
    __soteria___assert_569 -> function;
    __soteria___debug_show_570 -> function;
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
    add_646 -> function;
    operate_651 -> function;
    main_656 -> function
  
  Object definitions:
    
  
  Function definitions:
    add_562
    operate_651
    main_656
  
   Symmap:
    add_646 -> add_562;
    operate_651 -> operate_651;
    main_656 -> main_656;
    __soteria___nondet_int_483 -> __soteria___nondet_int_567;
    __soteria___assert_485 -> __soteria___assert_569;
    __soteria___debug_show_486 -> __soteria___debug_show_570;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_575;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_579;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_583;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_587;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_591;
    __builtin___memset_chk_512 -> __builtin___memset_chk_596;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_601;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_606;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_610;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_615;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_620;
    __builtin_object_size_539 -> __builtin_object_size_623;
    __builtin_bswap32_541 -> __builtin_bswap32_625;
    __builtin_bswap64_543 -> __builtin_bswap64_627;
    __atomic_load_n_546 -> __atomic_load_n_630;
    __atomic_add_fetch_550 -> __atomic_add_fetch_634;
    __atomic_sub_fetch_554 -> __atomic_sub_fetch_638;
    __atomic_exchange_559 -> __atomic_exchange_643;
    add_562 -> add_562
  
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
  
  // declare add WITH PROTO as function (signed int, signed int) returning signed int
  signed int add(signed int x, signed int y)
  {
    return rvalue(x) + rvalue(y);
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
  
  // declare add WITH PROTO as function (signed int, signed int) returning signed int
  signed int add(signed int, signed int);
  
  // declare operate WITH PROTO as function (pointer to function (signed int, signed int) returning signed int, signed int, signed int) returning signed int
  signed int operate(signed int (* op) (signed int, signed int), signed int a, signed int b)
  {
    return rvalue(op)(rvalue(a), rvalue(b));
  }
  
  // declare main as function () returning signed int
  signed int main()
  {
    signed int ret = function_decay(operate)(function_decay(add), 5, 3);
    function_decay(__soteria___assert)((rvalue(ret) == 8));
    return 0;
  }

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 8 statements
