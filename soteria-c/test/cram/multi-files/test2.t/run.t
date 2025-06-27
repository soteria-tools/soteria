  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_583, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_588, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_578, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_597, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_592, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_602, decl);
    __builtin_bswap32 -> (__builtin_bswap32_607, decl);
    __builtin_bswap64 -> (__builtin_bswap64_609, decl);
    __builtin_object_size -> (__builtin_object_size_605, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_557, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_573, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_569, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_561, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_565, decl);
    __soteria___assert -> (__soteria___assert_551, decl);
    __soteria___debug_show -> (__soteria___debug_show_552, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_549, decl);
    compute -> (compute_544, def);
    main -> (main_611, def)
  
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
    compute_544 -> function;
    helper_545 -> function;
    __soteria___nondet_int_549 -> function;
    __soteria___assert_551 -> function;
    __soteria___debug_show_552 -> function;
    __builtin_sadd_overflow_557 -> function;
    __builtin_umul_overflow_561 -> function;
    __builtin_umull_overflow_565 -> function;
    __builtin_uaddl_overflow_569 -> function;
    __builtin_ssub_overflow_573 -> function;
    __builtin___memset_chk_578 -> function;
    __builtin___memcpy_chk_583 -> function;
    __builtin___memmove_chk_588 -> function;
    __builtin___strcpy_chk_592 -> function;
    __builtin___snprintf_chk_597 -> function;
    __builtin___strncpy_chk_602 -> function;
    __builtin_object_size_605 -> function;
    __builtin_bswap32_607 -> function;
    __builtin_bswap64_609 -> function;
    compute_610 -> function;
    main_611 -> function
  
  Object definitions:
    
  
  Function definitions:
    compute_544
    helper_545
    main_611
  
   Symmap:
    __soteria___nondet_int_483 -> __soteria___nondet_int_549;
    __soteria___assert_485 -> __soteria___assert_551;
    __soteria___debug_show_486 -> __soteria___debug_show_552;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_557;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_561;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_565;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_569;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_573;
    __builtin___memset_chk_512 -> __builtin___memset_chk_578;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_583;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_588;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_592;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_597;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_602;
    __builtin_object_size_539 -> __builtin_object_size_605;
    __builtin_bswap32_541 -> __builtin_bswap32_607;
    __builtin_bswap64_543 -> __builtin_bswap64_609;
    compute_544 -> compute_544;
    compute_610 -> compute_544;
    main_611 -> main_611
  
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
