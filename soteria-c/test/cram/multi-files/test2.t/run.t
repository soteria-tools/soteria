  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __atomic_add_fetch -> (__atomic_add_fetch_632, decl);
    __atomic_exchange -> (__atomic_exchange_641, decl);
    __atomic_load_n -> (__atomic_load_n_628, decl);
    __atomic_sub_fetch -> (__atomic_sub_fetch_636, decl);
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_599, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_604, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_594, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_613, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_608, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_618, decl);
    __builtin_bswap32 -> (__builtin_bswap32_623, decl);
    __builtin_bswap64 -> (__builtin_bswap64_625, decl);
    __builtin_object_size -> (__builtin_object_size_621, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_573, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_589, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_585, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_577, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_581, decl);
    __soteria___assert -> (__soteria___assert_567, decl);
    __soteria___debug_show -> (__soteria___debug_show_568, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_565, decl);
    compute -> (compute_560, def);
    main -> (main_643, def)
  
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
    compute_560 -> function;
    helper_561 -> function;
    __soteria___nondet_int_565 -> function;
    __soteria___assert_567 -> function;
    __soteria___debug_show_568 -> function;
    __builtin_sadd_overflow_573 -> function;
    __builtin_umul_overflow_577 -> function;
    __builtin_umull_overflow_581 -> function;
    __builtin_uaddl_overflow_585 -> function;
    __builtin_ssub_overflow_589 -> function;
    __builtin___memset_chk_594 -> function;
    __builtin___memcpy_chk_599 -> function;
    __builtin___memmove_chk_604 -> function;
    __builtin___strcpy_chk_608 -> function;
    __builtin___snprintf_chk_613 -> function;
    __builtin___strncpy_chk_618 -> function;
    __builtin_object_size_621 -> function;
    __builtin_bswap32_623 -> function;
    __builtin_bswap64_625 -> function;
    __atomic_load_n_628 -> function;
    __atomic_add_fetch_632 -> function;
    __atomic_sub_fetch_636 -> function;
    __atomic_exchange_641 -> function;
    compute_642 -> function;
    main_643 -> function
  
  Object definitions:
    
  
  Function definitions:
    compute_560
    helper_561
    main_643
  
   Symmap:
    __soteria___nondet_int_483 -> __soteria___nondet_int_565;
    __soteria___assert_485 -> __soteria___assert_567;
    __soteria___debug_show_486 -> __soteria___debug_show_568;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_573;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_577;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_581;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_585;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_589;
    __builtin___memset_chk_512 -> __builtin___memset_chk_594;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_599;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_604;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_608;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_613;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_618;
    __builtin_object_size_539 -> __builtin_object_size_621;
    __builtin_bswap32_541 -> __builtin_bswap32_623;
    __builtin_bswap64_543 -> __builtin_bswap64_625;
    __atomic_load_n_546 -> __atomic_load_n_628;
    __atomic_add_fetch_550 -> __atomic_add_fetch_632;
    __atomic_sub_fetch_554 -> __atomic_sub_fetch_636;
    __atomic_exchange_559 -> __atomic_exchange_641;
    compute_560 -> compute_560;
    compute_642 -> compute_560;
    main_643 -> main_643
  
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
  
  // declare compute WITH PROTO as function () returning signed int
  signed int compute();
  
  // declare main as function () returning signed int
  signed int main()
  {
    signed int ret = function_decay(compute)();
    function_decay(__soteria___assert)((rvalue(ret) == 12));
    return 0;
  }

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 8 statements
