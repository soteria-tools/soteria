  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __atomic_add_fetch -> (__atomic_add_fetch_633, decl);
    __atomic_exchange -> (__atomic_exchange_642, decl);
    __atomic_load_n -> (__atomic_load_n_629, decl);
    __atomic_sub_fetch -> (__atomic_sub_fetch_637, decl);
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_600, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_605, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_595, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_614, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_609, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_619, decl);
    __builtin_bswap32 -> (__builtin_bswap32_624, decl);
    __builtin_bswap64 -> (__builtin_bswap64_626, decl);
    __builtin_object_size -> (__builtin_object_size_622, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_574, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_590, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_586, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_578, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_582, decl);
    __soteria___assert -> (__soteria___assert_568, decl);
    __soteria___debug_show -> (__soteria___debug_show_569, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_566, decl);
    get_x -> (get_x_562, def);
    get_xx -> (get_xx_560, def);
    main -> (main_645, def);
    x -> (x_644, def)
  
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
    get_xx_560 -> function;
    x_561 -> object;
    get_x_562 -> function;
    __soteria___nondet_int_566 -> function;
    __soteria___assert_568 -> function;
    __soteria___debug_show_569 -> function;
    __builtin_sadd_overflow_574 -> function;
    __builtin_umul_overflow_578 -> function;
    __builtin_umull_overflow_582 -> function;
    __builtin_uaddl_overflow_586 -> function;
    __builtin_ssub_overflow_590 -> function;
    __builtin___memset_chk_595 -> function;
    __builtin___memcpy_chk_600 -> function;
    __builtin___memmove_chk_605 -> function;
    __builtin___strcpy_chk_609 -> function;
    __builtin___snprintf_chk_614 -> function;
    __builtin___strncpy_chk_619 -> function;
    __builtin_object_size_622 -> function;
    __builtin_bswap32_624 -> function;
    __builtin_bswap64_626 -> function;
    __atomic_load_n_629 -> function;
    __atomic_add_fetch_633 -> function;
    __atomic_sub_fetch_637 -> function;
    __atomic_exchange_642 -> function;
    get_xx_643 -> function;
    x_644 -> object;
    main_645 -> function
  
  Object definitions:
    (x_561, 12)
    (x_644, 13)
  
  Function definitions:
    get_xx_560
    get_x_562
    main_645
  
   Symmap:
    __soteria___nondet_int_483 -> __soteria___nondet_int_566;
    __soteria___assert_485 -> __soteria___assert_568;
    __soteria___debug_show_486 -> __soteria___debug_show_569;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_574;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_578;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_582;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_586;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_590;
    __builtin___memset_chk_512 -> __builtin___memset_chk_595;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_600;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_605;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_609;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_614;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_619;
    __builtin_object_size_539 -> __builtin_object_size_622;
    __builtin_bswap32_541 -> __builtin_bswap32_624;
    __builtin_bswap64_543 -> __builtin_bswap64_626;
    __atomic_load_n_546 -> __atomic_load_n_629;
    __atomic_add_fetch_550 -> __atomic_add_fetch_633;
    __atomic_sub_fetch_554 -> __atomic_sub_fetch_637;
    __atomic_exchange_559 -> __atomic_exchange_642;
    get_xx_560 -> get_xx_560;
    get_x_562 -> get_x_562;
    get_xx_643 -> get_xx_560;
    x_644 -> x_644;
    main_645 -> main_645
  
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
              { node = [TypedVal {offset = 0; ty = signed int; v = 12}];
                info = None });
             (V|2|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 13}];
                info = None })];
            globs = [(x_561, V|1|); (x_644, V|2|)] })]
  Executed 7 statements
