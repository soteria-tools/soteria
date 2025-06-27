  $ soteria-c show-ail file1.c file2.c -I .
  Extern idmap:
    __builtin___memcpy_chk -> (__builtin___memcpy_chk_584, decl);
    __builtin___memmove_chk -> (__builtin___memmove_chk_589, decl);
    __builtin___memset_chk -> (__builtin___memset_chk_579, decl);
    __builtin___snprintf_chk -> (__builtin___snprintf_chk_598, decl);
    __builtin___strcpy_chk -> (__builtin___strcpy_chk_593, decl);
    __builtin___strncpy_chk -> (__builtin___strncpy_chk_603, decl);
    __builtin_bswap32 -> (__builtin_bswap32_608, decl);
    __builtin_bswap64 -> (__builtin_bswap64_610, decl);
    __builtin_object_size -> (__builtin_object_size_606, decl);
    __builtin_sadd_overflow -> (__builtin_sadd_overflow_558, decl);
    __builtin_ssub_overflow -> (__builtin_ssub_overflow_574, decl);
    __builtin_uaddl_overflow -> (__builtin_uaddl_overflow_570, decl);
    __builtin_umul_overflow -> (__builtin_umul_overflow_562, decl);
    __builtin_umull_overflow -> (__builtin_umull_overflow_566, decl);
    __soteria___assert -> (__soteria___assert_552, decl);
    __soteria___debug_show -> (__soteria___debug_show_553, decl);
    __soteria___nondet_int -> (__soteria___nondet_int_550, decl);
    get_x -> (get_x_546, def);
    get_xx -> (get_xx_544, def);
    main -> (main_613, def);
    x -> (x_612, def)
  
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
    get_xx_544 -> function;
    x_545 -> object;
    get_x_546 -> function;
    __soteria___nondet_int_550 -> function;
    __soteria___assert_552 -> function;
    __soteria___debug_show_553 -> function;
    __builtin_sadd_overflow_558 -> function;
    __builtin_umul_overflow_562 -> function;
    __builtin_umull_overflow_566 -> function;
    __builtin_uaddl_overflow_570 -> function;
    __builtin_ssub_overflow_574 -> function;
    __builtin___memset_chk_579 -> function;
    __builtin___memcpy_chk_584 -> function;
    __builtin___memmove_chk_589 -> function;
    __builtin___strcpy_chk_593 -> function;
    __builtin___snprintf_chk_598 -> function;
    __builtin___strncpy_chk_603 -> function;
    __builtin_object_size_606 -> function;
    __builtin_bswap32_608 -> function;
    __builtin_bswap64_610 -> function;
    get_xx_611 -> function;
    x_612 -> object;
    main_613 -> function
  
  Object definitions:
    (x_612, 13)
    (x_545, 12)
  
  Function definitions:
    get_xx_544
    get_x_546
    main_613
  
   Symmap:
    __soteria___nondet_int_483 -> __soteria___nondet_int_550;
    __soteria___assert_485 -> __soteria___assert_552;
    __soteria___debug_show_486 -> __soteria___debug_show_553;
    __builtin_sadd_overflow_491 -> __builtin_sadd_overflow_558;
    __builtin_umul_overflow_495 -> __builtin_umul_overflow_562;
    __builtin_umull_overflow_499 -> __builtin_umull_overflow_566;
    __builtin_uaddl_overflow_503 -> __builtin_uaddl_overflow_570;
    __builtin_ssub_overflow_507 -> __builtin_ssub_overflow_574;
    __builtin___memset_chk_512 -> __builtin___memset_chk_579;
    __builtin___memcpy_chk_517 -> __builtin___memcpy_chk_584;
    __builtin___memmove_chk_522 -> __builtin___memmove_chk_589;
    __builtin___strcpy_chk_526 -> __builtin___strcpy_chk_593;
    __builtin___snprintf_chk_531 -> __builtin___snprintf_chk_598;
    __builtin___strncpy_chk_536 -> __builtin___strncpy_chk_603;
    __builtin_object_size_539 -> __builtin_object_size_606;
    __builtin_bswap32_541 -> __builtin_bswap32_608;
    __builtin_bswap64_543 -> __builtin_bswap64_610;
    get_xx_544 -> get_xx_544;
    get_x_546 -> get_x_546;
    get_xx_611 -> get_xx_544;
    x_612 -> x_612;
    main_613 -> main_613
  
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

  $ soteria-c exec-main file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (25,
          { heap =
            [(V|1|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 13}];
                info = None });
             (V|2|,
              { node = [TypedVal {offset = 0; ty = signed int; v = 12}];
                info = None })];
            globs = [(x_545, V|2|); (x_612, V|1|)] })]
  Executed 7 statements
