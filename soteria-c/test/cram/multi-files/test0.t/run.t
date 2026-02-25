  $ soteria-c show-ail file1.c file2.c -I .
  AilSigma
  |-AilTypedefAttributes
  | |-__builtin_va_list EMPTY
  | `-__builtin_va_list EMPTY
  |-AilDeclarations
  | |-Decl_function __soteria___nondet_int 'function () returning signed int' EMPTY
  | |-Decl_function __soteria___assert 'function (signed int) returning void' EMPTY
  | |-Decl_function __builtin_sadd_overflow 'function (signed int,
  signed int, pointer to signed int) returning _Bool' EMPTY
  | |-Decl_function __builtin_umul_overflow 'function (unsigned int,
  unsigned int, pointer to unsigned int) returning _Bool' EMPTY
  | |-Decl_function __builtin_umull_overflow 'function (unsigned long,
  unsigned long, pointer to unsigned long) returning _Bool' EMPTY
  | |-Decl_function __builtin_uaddl_overflow 'function (unsigned long,
  unsigned long, pointer to unsigned long) returning _Bool' EMPTY
  | |-Decl_function __builtin_ssub_overflow 'function (signed int,
  signed int, pointer to signed int) returning _Bool' EMPTY
  | |-Decl_function __builtin___memset_chk 'function (pointer to void,
  signed int, size_t, size_t) returning pointer to void' EMPTY
  | |-Decl_function __builtin___memcpy_chk 'function (pointer to void,
  pointer to {const} void, size_t, size_t) returning pointer to void' EMPTY
  | |-Decl_function __builtin___memmove_chk 'function (pointer to void,
  pointer to {const} void, size_t, size_t) returning pointer to void' EMPTY
  | |-Decl_function __builtin___strcpy_chk 'function (pointer to char,
  pointer to {const} char, size_t) returning pointer to char' EMPTY
  | |-Decl_function __builtin___snprintf_chk 'variadic function (pointer to char,
  size_t, signed int, size_t) returning signed int' EMPTY
  | |-Decl_function __builtin___strncpy_chk 'function (pointer to char,
  pointer to {const} char, size_t, size_t) returning pointer to char' EMPTY
  | |-Decl_function __builtin_object_size 'function (pointer to {const} void,
  signed int) returning size_t' EMPTY
  | |-Decl_function __builtin_bswap32 'function (uint32_t) returning uint32_t' EMPTY
  | |-Decl_function __builtin_bswap64 'function (uint64_t) returning uint64_t' EMPTY
  | |-Decl_function __atomic_load_n 'function (pointer to {volatile} signed int,
  signed int) returning signed int' EMPTY
  | |-Decl_function __atomic_add_fetch 'function (pointer to {volatile} signed int,
  signed int, signed int) returning signed int' EMPTY
  | |-Decl_function __atomic_sub_fetch 'function (pointer to {volatile} signed int,
  signed int, signed int) returning signed int' EMPTY
  | |-Decl_function __atomic_exchange 'function (pointer to {volatile} signed int,
  pointer to signed int,
  pointer to signed int, signed int) returning void' EMPTY
  | |-Decl_function get_xx 'function () returning signed int' EMPTY
  | |-Decl_object x 'static signed int' EMPTY
  | |-Decl_function get_x 'function () returning signed int' EMPTY
  | |-Decl_function __soteria___nondet_int 'function () returning signed int' EMPTY
  | |-Decl_function __soteria___assert 'function (signed int) returning void' EMPTY
  | |-Decl_function __builtin_sadd_overflow 'function (signed int,
  signed int, pointer to signed int) returning _Bool' EMPTY
  | |-Decl_function __builtin_umul_overflow 'function (unsigned int,
  unsigned int, pointer to unsigned int) returning _Bool' EMPTY
  | |-Decl_function __builtin_umull_overflow 'function (unsigned long,
  unsigned long, pointer to unsigned long) returning _Bool' EMPTY
  | |-Decl_function __builtin_uaddl_overflow 'function (unsigned long,
  unsigned long, pointer to unsigned long) returning _Bool' EMPTY
  | |-Decl_function __builtin_ssub_overflow 'function (signed int,
  signed int, pointer to signed int) returning _Bool' EMPTY
  | |-Decl_function __builtin___memset_chk 'function (pointer to void,
  signed int, size_t, size_t) returning pointer to void' EMPTY
  | |-Decl_function __builtin___memcpy_chk 'function (pointer to void,
  pointer to {const} void, size_t, size_t) returning pointer to void' EMPTY
  | |-Decl_function __builtin___memmove_chk 'function (pointer to void,
  pointer to {const} void, size_t, size_t) returning pointer to void' EMPTY
  | |-Decl_function __builtin___strcpy_chk 'function (pointer to char,
  pointer to {const} char, size_t) returning pointer to char' EMPTY
  | |-Decl_function __builtin___snprintf_chk 'variadic function (pointer to char,
  size_t, signed int, size_t) returning signed int' EMPTY
  | |-Decl_function __builtin___strncpy_chk 'function (pointer to char,
  pointer to {const} char, size_t, size_t) returning pointer to char' EMPTY
  | |-Decl_function __builtin_object_size 'function (pointer to {const} void,
  signed int) returning size_t' EMPTY
  | |-Decl_function __builtin_bswap32 'function (uint32_t) returning uint32_t' EMPTY
  | |-Decl_function __builtin_bswap64 'function (uint64_t) returning uint64_t' EMPTY
  | |-Decl_function __atomic_load_n 'function (pointer to {volatile} signed int,
  signed int) returning signed int' EMPTY
  | |-Decl_function __atomic_add_fetch 'function (pointer to {volatile} signed int,
  signed int, signed int) returning signed int' EMPTY
  | |-Decl_function __atomic_sub_fetch 'function (pointer to {volatile} signed int,
  signed int, signed int) returning signed int' EMPTY
  | |-Decl_function __atomic_exchange 'function (pointer to {volatile} signed int,
  pointer to signed int,
  pointer to signed int, signed int) returning void' EMPTY
  | |-Decl_function get_xx 'function () returning signed int' EMPTY
  | |-Decl_object x 'static signed int' EMPTY
  | `-Decl_function main 'function () returning signed int' EMPTY
  |-AilTagDefinitions EMPTY
  |-AilObjectDefinitions
  | |-Def_object x
  | | `-[§6.5.1#3; §6.7.9#11, sentence 3] AilEconst <line:3:8 - 10> 13
  | `-Def_object x
  |   `-[§6.5.1#3; §6.7.9#11, sentence 3] AilEconst <file1.c:3:15, col:17> 12
  |-AilFunctionDefinitions
  | |-FunctionDecl <file1.c:10:0, line:13:1> col:4 - line:10:10 get_xx
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSreturn
  | |     `- AilEcall <line:12:9 - 16>
  | |       `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:9 - 14>
  | |         `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEident <col:9 - 14> get_x
  | |-FunctionDecl <line:5:0, line:8:1> col:4 - line:5:9 get_x
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSreturn
  | |     `-[§6.5.16.1#1, bullet 1] AilErvalue <line:7:9 - 10>
  | |       `-[§6.5.16.1#1, bullet 1] AilEident <col:9 - 10> x
  | `-FunctionDecl <file2.c:5:0, line:9:1> col:4 - line:5:8 main
  |   `-AilSblock
  |     |-Bindings
  |     | `-ret automatic 'signed int'
  |     |-AilSdeclaration
  |     | `-Symbol ret
  |     |   `- AilEcall <line:7:12 - 20>
  |     |     `-[§6.5.2.2#1;
  §6.7.9#11, sentence 3;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:12 - 18>
  |     |       `-[§6.5.2.2#1;
  §6.7.9#11, sentence 3;
  §6.5.16.1#1, bullet 1] AilEident <col:12 - 18> get_xx
  |     `-AilSreturn
  |       `- AilEbinary <line:8:9 - 16> col:13 '+'
  |         |-[§6.5.16.1#1, bullet 1] AilErvalue <col:9 - 12>
  |         | `-[§6.5.16.1#1, bullet 1] AilEident <col:9 - 12> ret
  |         `- AilErvalue <col:15 - 16>
  |           `- AilEident <col:15 - 16> x
  |-AilStaticAssertions EMPTY
  `-AilCNpredicates EMPTY
  

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000019,
          Some
            [(Ser_heap
                (0x0000000000000001,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000d : signed int};
                   info = None }));
             (Ser_heap
                (0x0000000000000002,
                 { node =
                   MemVal {offset = 0x0000000000000000;
                     len = 0x0000000000000004; v = 0x0000000c : signed int};
                   info = None }));
             (Ser_globs (x_560, 0x0000000000000002));
             (Ser_globs (x_642, 0x0000000000000001))])]
  
  Executed 7 statements
  Verification Success!
