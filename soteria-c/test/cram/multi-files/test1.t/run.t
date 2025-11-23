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
  | |-Decl_object glob 'static signed int' EMPTY
  | |-Decl_function exposed 'function () returning signed int' EMPTY
  | |-Decl_function test 'function () returning signed int' EMPTY
  | |-Decl_function update 'function () returning void' EMPTY
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
  | |-Decl_object glob 'static signed int' EMPTY
  | |-Decl_object ref 'static pointer to signed int' EMPTY
  | `-Decl_function exposed 'function () returning signed int' EMPTY
  |-AilTagDefinitions EMPTY
  |-AilObjectDefinitions
  | |-Def_object ref
  | | `-[§6.5.3.2#3, 2nd sentence] AilEunary <line:4:11 - 16> col:11 '&'
  | |   `-[§6.5.3.2#1;
  §6.7.9#11, sentence 3;
  §6.5.16.1#1, bullet 1] AilEident <col:12 - 16> glob
  | `-Def_object glob
  |   `-[§6.5.1#3; §6.7.9#11, sentence 3] AilEconst <line:3:11 - 13> 42
  |-AilFunctionDefinitions
  | |-FunctionDecl <file1.c:3:0, line:6:1> col:4 - line:3:8 test
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSreturn
  | |     `- AilEcall <line:5:11 - 20>
  | |       `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEfunction_decay <col:11 - 18>
  | |         `-[§6.5.2.2#1;
  §6.5.16.1#1, bullet 1] AilEident <col:11 - 18> exposed
  | |-FunctionDecl <line:8:0, line:11:1> col:5 - line:8:11 update
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   `-AilSexpr
  | |     `-[§6.5.16.1#1, bullet 1] AilEassign <line:10:4 - 15> col:9
  | |       |-[§6.5.16.1#1, bullet 1] AilEident <col:4 - 8> glob
  | |       `-[§6.5.1#3;
  §6.5.16#3, sentence 3] AilEconst <col:11 - 15> 1024
  | |-FunctionDecl <line:13:0, line:19:1> col:4 - line:13:8 main
  | | `-AilSblock
  | |   |-Bindings EMPTY
  | |   |-AilSexpr
  | |   | `-[§6.5.16.1#1, bullet 1] AilEcall <line:15:4 - 36>
  | |   |   |-[§6.5.2.2#1] AilEfunction_decay <col:4 - 22>
  | |   |   | `-[§6.5.2.2#1] AilEident <col:4 - 22> __soteria___assert
  | |   |   `- AilEbinary <col:23 - 35> col:30 '=='
  | |   |     |- AilEcall <col:23 - 29>
  | |   |     | `-[§6.5.2.2#1] AilEfunction_decay <col:23 - 27>
  | |   |     |   `-[§6.5.2.2#1] AilEident <col:23 - 27> test
  | |   |     `-[§6.5.1#3] AilEconst <col:33 - 35> 42
  | |   |-AilSexpr
  | |   | `- AilEcall <line:16:4 - 12>
  | |   |   `-[§6.5.2.2#1] AilEfunction_decay <col:4 - 10>
  | |   |     `-[§6.5.2.2#1] AilEident <col:4 - 10> update
  | |   |-AilSexpr
  | |   | `-[§6.5.16.1#1, bullet 1] AilEcall <line:17:4 - 38>
  | |   |   |-[§6.5.2.2#1] AilEfunction_decay <col:4 - 22>
  | |   |   | `-[§6.5.2.2#1] AilEident <col:4 - 22> __soteria___assert
  | |   |   `- AilEbinary <col:23 - 37> col:30 '=='
  | |   |     |- AilEcall <col:23 - 29>
  | |   |     | `-[§6.5.2.2#1] AilEfunction_decay <col:23 - 27>
  | |   |     |   `-[§6.5.2.2#1] AilEident <col:23 - 27> test
  | |   |     `-[§6.5.1#3] AilEconst <col:33 - 37> 1024
  | |   `-AilSreturn
  | |     `-[§6.5.1#3] AilEconst <line:18:11 - 12> 0
  | `-FunctionDecl <file2.c:6:0, line:9:1> col:4 - line:6:11 exposed
  |   `-AilSblock
  |     |-Bindings EMPTY
  |     `-AilSreturn
  |       `-[§6.5.3.2#4, sentence 3;
  §6.5.3.2#4, sentence 2] AilErvalue <line:8:9 - 13> col:9
  |         `-[§6.5.3.2#4, sentence 3;
  §6.5.3.2#4, sentence 2] AilEunary <col:9 - 13> col:9 '*'
  |           `-[§6.5.16.1#1, bullet 3] AilErvalue <col:10 - 13>
  |             `-[§6.5.16.1#1, bullet 3] AilEident <col:10 - 13> ref
  |-AilStaticAssertions EMPTY
  `-AilCNpredicates EMPTY
  

  $ soteria-c exec file1.c file2.c -I . --no-ignore-parse-failures --no-ignore-duplicate-symbols --print-states
  Symex terminated with the following outcomes:
    [Ok: (0x00000000,
          { heap =
            [(V|1|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000008;
                   v = &(V|2|, 0x0000000000000000) : signed int*}];
                info = None });
             (V|2|,
              { node =
                [MemVal {offset = 0x0000000000000000; len = 0x0000000000000004;
                   v = 0x00000400 : signed int}];
                info = None })];
            globs = [(glob_644, V|2|); (ref_645, V|1|)] })]
  
  Executed 15 statements
  Verification Success!
