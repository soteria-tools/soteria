  $ soteria-c show-ail file1.c file2.c -I .
  AilSigma
  |-AilTypedefAttributes
  | |-__builtin_va_list EMPTY
  | |-operation EMPTY
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
  | |-Decl_function add 'function (signed int,
  signed int) returning signed int' EMPTY
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
  | |-Decl_function add 'function (signed int,
  signed int) returning signed int' EMPTY
  | |-Decl_function operate 'function (pointer to function (signed int,
  signed int) returning signed int (typedef: operation),
  signed int, signed int) returning signed int' EMPTY
  | `-Decl_function main 'function () returning signed int' EMPTY
  |-AilTagDefinitions EMPTY
  |-AilObjectDefinitions EMPTY
  |-AilFunctionDefinitions
  | |-FunctionDecl <file1.c:3:0, line:6:1> col:4 - line:3:7 add
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSreturn
  | |     `- AilEbinary <line:5:11 - 16> col:13 '+'
  | |       |- AilErvalue <col:11 - 12>
  | |       | `- AilEident <col:11 - 12> x
  | |       `- AilErvalue <col:15 - 16>
  | |         `- AilEident <col:15 - 16> y
  | |-FunctionDecl <file2.c:7:0, line:10:1> col:4 - line:7:11 operate
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSreturn
  | |     `-[§6.5.16.1#1, bullet 1] AilEcall <line:9:11 - 19>
  | |       |-[§6.5.2.2#1] AilErvalue <col:11 - 13>
  | |       | `-[§6.5.2.2#1] AilEident <col:11 - 13> op
  | |       |- AilErvalue <col:14 - 15>
  | |       | `- AilEident <col:14 - 15> a
  | |       `-[§6.5.16.1#1, bullet 1] AilErvalue <col:17 - 18>
  | |         `-[§6.5.16.1#1, bullet 1] AilEident <col:17 - 18> b
  | `-FunctionDecl <line:12:0, line:17:1> col:4 - line:12:8 main
  |   `-AilSblock
  |     |-Bindings
  |     | `-ret automatic 'signed int'
  |     |-AilSdeclaration
  |     | `-Symbol ret
  |     |   `-[§6.5.16.1#1, bullet 1] AilEcall <line:14:14 - 32>
  |     |     |-[§6.5.2.2#1;
  §6.7.9#11, sentence 3;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:14 - 21>
  |     |     | `-[§6.5.2.2#1;
  §6.7.9#11, sentence 3;
  §6.5.16.1#1, bullet 1] AilEident <col:14 - 21> operate
  |     |     |- AilEfunction_decay <col:22 - 25>
  |     |     | `- AilEident <col:22 - 25> add
  |     |     |-[§6.5.1#3;
  §6.5.16.1#1, bullet 3] AilEconst <col:27 - 28> 5
  |     |     `-[§6.5.1#3;
  §6.5.16.1#1, bullet 1] AilEconst <col:30 - 31> 3
  |     |-AilSexpr
  |     | `-[§6.5.16.1#1, bullet 1] AilEcall <line:15:4 - 32>
  |     |   |-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:4 - 22>
  |     |   | `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEident <col:4 - 22> __soteria___assert
  |     |   `- AilEbinary <col:23 - 31> col:27 '=='
  |     |     |- AilErvalue <col:23 - 26>
  |     |     | `- AilEident <col:23 - 26> ret
  |     |     `-[§6.5.1#3] AilEconst <col:30 - 31> 8
  |     `-AilSreturn
  |       `-[§6.5.1#3] AilEconst <line:16:11 - 12> 0
  |-AilStaticAssertions EMPTY
  `-AilCNpredicates EMPTY
  

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000, { heap = []; globs = [] })]
  
  Executed 8 statements
  Verification Success!
