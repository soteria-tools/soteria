  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __atomic_add_fetch -> (__atomic_add_fetch_631, decl);
    __atomic_exchange -> (__atomic_exchange_640, decl);
    __atomic_load_n -> (__atomic_load_n_627, decl);
    __atomic_sub_fetch -> (__atomic_sub_fetch_635, decl);
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_598, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_603, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_593, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_612, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_607, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_617, decl);
    __builtin_bswap32 -> (__builtin_bswap32_622, decl);
    __builtin_bswap64 -> (__builtin_bswap64_624, decl);
    __builtin_object_size -> (__builtin_object_size_620, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_572, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_588, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_584, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_576, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_580, decl);
    __soteria___assert -> (__soteria___assert_567, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_565, decl);
    get_x -> (get_x_561, def);
    get_xx -> (get_xx_559, def);
    main -> (main_643, def);
    x -> (x_642, def)
  
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
    get_xx_559 -> function;
    x_560 -> object;
    get_x_561 -> function;
    __soteria___nondet_int_565 -> function;
    __soteria___assert_567 -> function;
    __builtin_sadd_overflow_572 -> function;
    __builtin_umul_overflow_576 -> function;
    __builtin_umull_overflow_580 -> function;
    __builtin_uaddl_overflow_584 -> function;
    __builtin_ssub_overflow_588 -> function;
    __builtin___memset_chk_593 -> function;
    __builtin___memcpy_chk_598 -> function;
    __builtin___memmove_chk_603 -> function;
    __builtin___strcpy_chk_607 -> function;
    __builtin___snprintf_chk_612 -> function;
    __builtin___strncpy_chk_617 -> function;
    __builtin_object_size_620 -> function;
    __builtin_bswap32_622 -> function;
    __builtin_bswap64_624 -> function;
    __atomic_load_n_627 -> function;
    __atomic_add_fetch_631 -> function;
    __atomic_sub_fetch_635 -> function;
    __atomic_exchange_640 -> function;
    get_xx_641 -> function;
    x_642 -> object;
    main_643 -> function
  
  Object definitions:
    (x_642, 13)
    (x_560, 12)
  
  Function definitions:
    get_xx_559
    get_x_561
    main_643
  
   Symmap:
    __soteria___nondet_int_483 -> __soteria___nondet_int_565;
    __soteria___assert_485 -> __soteria___assert_567;
    __builtin_sadd_overflow_490 -> __builtin_sadd_overflow_572;
    __builtin_umul_overflow_494 -> __builtin_umul_overflow_576;
    __builtin_umull_overflow_498 -> __builtin_umull_overflow_580;
    __builtin_uaddl_overflow_502 -> __builtin_uaddl_overflow_584;
    __builtin_ssub_overflow_506 -> __builtin_ssub_overflow_588;
    __builtin___memset_chk_511 -> __builtin___memset_chk_593;
    __builtin___memcpy_chk_516 -> __builtin___memcpy_chk_598;
    __builtin___memmove_chk_521 -> __builtin___memmove_chk_603;
    __builtin___strcpy_chk_525 -> __builtin___strcpy_chk_607;
    __builtin___snprintf_chk_530 -> __builtin___snprintf_chk_612;
    __builtin___strncpy_chk_535 -> __builtin___strncpy_chk_617;
    __builtin_object_size_538 -> __builtin_object_size_620;
    __builtin_bswap32_540 -> __builtin_bswap32_622;
    __builtin_bswap64_542 -> __builtin_bswap64_624;
    __atomic_load_n_545 -> __atomic_load_n_627;
    __atomic_add_fetch_549 -> __atomic_add_fetch_631;
    __atomic_sub_fetch_553 -> __atomic_sub_fetch_635;
    __atomic_exchange_558 -> __atomic_exchange_640;
    get_xx_559 -> get_xx_559;
    get_x_561 -> get_x_561;
    get_xx_641 -> get_xx_559;
    x_642 -> x_642;
    main_643 -> main_643
  
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
  
  // declare get_xx WITH PROTO as function () returning signed int
  signed int get_xx()
  {
    return function_decay(get_x)();
  }
  
  // declare x as signed int
  signed int x = 12;
  
  // declare get_x as function () returning signed int
  signed int get_x()
  {
    return rvalue(x);
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
  
  // declare get_xx WITH PROTO as function () returning signed int
  signed int get_xx();
  
  // declare x as signed int
  signed int x = 13;
  
  // declare main as function () returning signed int
  signed int main()
  {
    signed int ret = function_decay(get_xx)();
    return rvalue(ret) + rvalue(x);
  }

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (25,
          { heap =
            [(V|1|,
              { node = [MemVal {offset = 0; len = 4; v = 13 : signed int}];
                info = None });
             (V|2|,
              { node = [MemVal {offset = 0; len = 4; v = 12 : signed int}];
                info = None })];
            globs = [(x_560, V|2|); (x_642, V|1|)] })]
  Executed 7 statements
