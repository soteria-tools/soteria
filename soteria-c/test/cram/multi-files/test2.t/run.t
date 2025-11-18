  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __atomic_add_fetch -> (__atomic_add_fetch_630, decl);
    __atomic_exchange -> (__atomic_exchange_639, decl);
    __atomic_load_n -> (__atomic_load_n_626, decl);
    __atomic_sub_fetch -> (__atomic_sub_fetch_634, decl);
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_597, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_602, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_592, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_611, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_606, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_616, decl);
    __builtin_bswap32 -> (__builtin_bswap32_621, decl);
    __builtin_bswap64 -> (__builtin_bswap64_623, decl);
    __builtin_object_size -> (__builtin_object_size_619, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_571, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_587, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_583, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_575, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_579, decl);
    __soteria___assert -> (__soteria___assert_566, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_564, decl);
    compute -> (compute_559, def);
    main -> (main_641, def)
  
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
    compute_559 -> function;
    helper_560 -> function;
    __soteria___nondet_int_564 -> function;
    __soteria___assert_566 -> function;
    __builtin_sadd_overflow_571 -> function;
    __builtin_umul_overflow_575 -> function;
    __builtin_umull_overflow_579 -> function;
    __builtin_uaddl_overflow_583 -> function;
    __builtin_ssub_overflow_587 -> function;
    __builtin___memset_chk_592 -> function;
    __builtin___memcpy_chk_597 -> function;
    __builtin___memmove_chk_602 -> function;
    __builtin___strcpy_chk_606 -> function;
    __builtin___snprintf_chk_611 -> function;
    __builtin___strncpy_chk_616 -> function;
    __builtin_object_size_619 -> function;
    __builtin_bswap32_621 -> function;
    __builtin_bswap64_623 -> function;
    __atomic_load_n_626 -> function;
    __atomic_add_fetch_630 -> function;
    __atomic_sub_fetch_634 -> function;
    __atomic_exchange_639 -> function;
    compute_640 -> function;
    main_641 -> function
  
  Object definitions:
    
  
  Function definitions:
    compute_559
    helper_560
    main_641
  
   Symmap:
    __soteria___nondet_int_483 -> __soteria___nondet_int_564;
    __soteria___assert_485 -> __soteria___assert_566;
    __builtin_sadd_overflow_490 -> __builtin_sadd_overflow_571;
    __builtin_umul_overflow_494 -> __builtin_umul_overflow_575;
    __builtin_umull_overflow_498 -> __builtin_umull_overflow_579;
    __builtin_uaddl_overflow_502 -> __builtin_uaddl_overflow_583;
    __builtin_ssub_overflow_506 -> __builtin_ssub_overflow_587;
    __builtin___memset_chk_511 -> __builtin___memset_chk_592;
    __builtin___memcpy_chk_516 -> __builtin___memcpy_chk_597;
    __builtin___memmove_chk_521 -> __builtin___memmove_chk_602;
    __builtin___strcpy_chk_525 -> __builtin___strcpy_chk_606;
    __builtin___snprintf_chk_530 -> __builtin___snprintf_chk_611;
    __builtin___strncpy_chk_535 -> __builtin___strncpy_chk_616;
    __builtin_object_size_538 -> __builtin_object_size_619;
    __builtin_bswap32_540 -> __builtin_bswap32_621;
    __builtin_bswap64_542 -> __builtin_bswap64_623;
    __atomic_load_n_545 -> __atomic_load_n_626;
    __atomic_add_fetch_549 -> __atomic_add_fetch_630;
    __atomic_sub_fetch_553 -> __atomic_sub_fetch_634;
    __atomic_exchange_558 -> __atomic_exchange_639;
    compute_559 -> compute_559;
    compute_640 -> compute_559;
    main_641 -> main_641
  
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

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  
  Executed 8 statements
  Verification Success!
