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
  | |-Decl_function funcA 'function (signed int) returning signed int' EMPTY
  | |-Decl_function funcB 'function (signed int) returning signed int' EMPTY
  | |-Decl_function main 'function () returning signed int' EMPTY
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
  | |-Decl_function funcA 'function (signed int) returning signed int' EMPTY
  | `-Decl_function funcB 'function (signed int) returning signed int' EMPTY
  |-AilTagDefinitions EMPTY
  |-AilObjectDefinitions EMPTY
  |-AilFunctionDefinitions
  | |-FunctionDecl <file1.c:3:0, line:10:1> col:4 - line:3:9 funcB
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   |-AilSif
  | |   | |- AilEbinary <line:5:6 - 12> col:8 '<='
  | |   | | |- AilErvalue <col:6 - 7>
  | |   | | | `- AilEident <col:6 - 7> x
  | |   | | `-[§6.5.1#3] AilEconst <col:11 - 12> 0
  | |   | |-AilSblock
  | |   | | |-Bindings EMPTY
  | |   | | `-AilSreturn
  | |   | |   `-[§6.5.1#3; §6.8.4.1#1] AilEconst <line:7:11 - 12> 1
  | |   | `-AilSskip EMPTY
  | |   `-AilSreturn
  | |     `-[§6.5.16.1#1, bullet 1] AilEcall <line:9:9 - 21>
  | |       |-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:9 - 14>
  | |       | `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEident <col:9 - 14> funcA
  | |       `- AilEbinary <col:15 - 20> col:17 '-'
  | |         |- AilErvalue <col:15 - 16>
  | |         | `- AilEident <col:15 - 16> x
  | |         `-[§6.5.1#3] AilEconst <col:19 - 20> 1
  | |-FunctionDecl <line:12:0, line:16:1> col:4 - line:12:8 main
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   |-AilSexpr
  | |   | `-[§6.5.16.1#1, bullet 1] AilEcall <line:14:2 - 35>
  | |   |   |-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:2 - 20>
  | |   |   | `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEident <col:2 - 20> __soteria___assert
  | |   |   `- AilEbinary <col:21 - 34> col:30 '=='
  | |   |     |-[§6.5.16.1#1, bullet 1] AilEcall <col:21 - 29>
  | |   |     | |-[§6.5.2.2#1] AilEfunction_decay <col:21 - 26>
  | |   |     | | `-[§6.5.2.2#1] AilEident <col:21 - 26> funcA
  | |   |     | `-[§6.5.1#3] AilEconst <col:27 - 28> 5
  | |   |     `-[§6.5.1#3] AilEconst <col:33 - 34> 1
  | |   `-AilSreturn
  | |     `-[§6.5.1#3] AilEconst <line:15:9 - 10> 0
  | `-FunctionDecl <file2.c:3:0, line:10:1> col:4 - line:3:9 funcA
  |   `-AilSblock
  |     |-Bindings EMPTY
  |     |-AilSif
  |     | |- AilEbinary <line:5:6 - 12> col:8 '<='
  |     | | |- AilErvalue <col:6 - 7>
  |     | | | `- AilEident <col:6 - 7> x
  |     | | `-[§6.5.1#3] AilEconst <col:11 - 12> 0
  |     | |-AilSblock
  |     | | |-Bindings EMPTY
  |     | | `-AilSreturn
  |     | |   `-[§6.5.1#3; §6.8.4.1#1] AilEconst <line:7:11 - 12> 0
  |     | `-AilSskip EMPTY
  |     `-AilSreturn
  |       `-[§6.5.16.1#1, bullet 1] AilEcall <line:9:9 - 21>
  |         |-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:9 - 14>
  |         | `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEident <col:9 - 14> funcB
  |         `- AilEbinary <col:15 - 20> col:17 '-'
  |           |- AilErvalue <col:15 - 16>
  |           | `- AilEident <col:15 - 16> x
  |           `-[§6.5.1#3] AilEconst <col:19 - 20> 1
  |-AilStaticAssertions EMPTY
  `-AilCNpredicates EMPTY
  

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  
  Executed 27 statements
  Verification Success!
