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
  | |-Decl_function compute 'function () returning signed int' EMPTY
  | |-Decl_function helper 'function () returning signed int' EMPTY
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
  | |-Decl_function compute 'function () returning signed int' EMPTY
  | `-Decl_function main 'function () returning signed int' EMPTY
  |-AilTagDefinitions EMPTY
  |-AilObjectDefinitions EMPTY
  |-AilFunctionDefinitions
  | |-FunctionDecl <file1.c:8:0, line:11:1> col:4 - line:8:11 compute
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSreturn
  | |     `- AilEbinary <line:10:9 - 21> col:11 '+'
  | |       |-[§6.5.1#3] AilEconst <col:9 - 10> 5
  | |       `- AilEcall <col:13 - 21>
  | |         `-[§6.5.2.2#1] AilEfunction_decay <col:13 - 19>
  | |           `-[§6.5.2.2#1] AilEident <col:13 - 19> helper
  | |-FunctionDecl <line:3:0, line:6:1> col:11 - line:3:17 helper
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSreturn
  | |     `-[§6.5.1#3;
  §6.5.16.1#1, bullet 1] AilEconst <line:5:9 - 10> 7
  | `-FunctionDecl <file2.c:3:0, line:8:1> col:4 - line:3:8 main
  |   `-AilSblock
  |     |-Bindings
  |     | `-ret automatic 'signed int'
  |     |-AilSdeclaration
  |     | `-Symbol ret
  |     |   `- AilEcall <line:5:12 - 21>
  |     |     `-[§6.5.2.2#1;
  §6.7.9#11, sentence 3] AilEfunction_decay <col:12 - 19>
  |     |       `-[§6.5.2.2#1;
  §6.7.9#11, sentence 3] AilEident <col:12 - 19> compute
  |     |-AilSexpr
  |     | `-[§6.5.16.1#1, bullet 1] AilEcall <line:6:2 - 31>
  |     |   |-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:2 - 20>
  |     |   | `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEident <col:2 - 20> __soteria___assert
  |     |   `- AilEbinary <col:21 - 30> col:25 '=='
  |     |     |- AilErvalue <col:21 - 24>
  |     |     | `- AilEident <col:21 - 24> ret
  |     |     `-[§6.5.1#3] AilEconst <col:28 - 30> 12
  |     `-AilSreturn
  |       `-[§6.5.1#3] AilEconst <line:7:9 - 10> 0
  |-AilStaticAssertions EMPTY
  `-AilCNpredicates EMPTY
  

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, None)]
  
  Executed 8 statements
  Verification Success!
