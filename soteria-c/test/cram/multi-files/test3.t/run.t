  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_585, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_590, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_580, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_599, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_594, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_604, decl);
    __builtin_bswap32 -> (__builtin_bswap32_609, decl);
    __builtin_bswap64 -> (__builtin_bswap64_611, decl);
    __builtin_object_size -> (__builtin_object_size_607, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_559, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_575, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_571, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_563, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_567, decl);
    __soteria___assert -> (__soteria___assert_553, decl);
    __soteria___debug_show -> (__soteria___debug_show_554, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_551, decl);
    add -> (add_546, def);
    main -> (main_624, def);
    operate -> (operate_619, def)
  
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
    add_546 -> function;
    __soteria___nondet_int_551 -> function;
    __soteria___assert_553 -> function;
    __soteria___debug_show_554 -> function;
    __builtin_sadd_overflow_559 -> function;
    __builtin_umul_overflow_563 -> function;
    __builtin_umull_overflow_567 -> function;
    __builtin_uaddl_overflow_571 -> function;
    __builtin_ssub_overflow_575 -> function;
    __builtin___memset_chk_580 -> function;
    __builtin___memcpy_chk_585 -> function;
    __builtin___memmove_chk_590 -> function;
    __builtin___strcpy_chk_594 -> function;
    __builtin___snprintf_chk_599 -> function;
    __builtin___strncpy_chk_604 -> function;
    __builtin_object_size_607 -> function;
    __builtin_bswap32_609 -> function;
    __builtin_bswap64_611 -> function;
    add_614 -> function;
    operate_619 -> function;
    main_624 -> function
  
  Object definitions:
    
  
  Function definitions:
    add_546
    operate_619
    main_624
  
   Symmap:
    add_614 -> add_546;
    operate_619 -> operate_619;
    main_624 -> main_624;
    __soteria___nondet_int_483 -> __soteria___nondet_int_551;
    __soteria___assert_485 -> __soteria___assert_553;
    __soteria___debug_show_486 -> __soteria___debug_show_554;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_559;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_563;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_567;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_571;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_575;
    __builtin___memset_chk_512 -> __builtin___memset_chk_580;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_585;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_590;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_594;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_599;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_604;
    __builtin_object_size_539 -> __builtin_object_size_607;
    __builtin_bswap32_541 -> __builtin_bswap32_609;
    __builtin_bswap64_543 -> __builtin_bswap64_611;
    add_546 -> add_546
  
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

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0, { heap = []; globs = [] })]
  Executed 8 statements
