Warning and unspported with CBMC enabled 
  $ ../exec_test.sh test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  warning: CBMC support is not enabled, but detected use of the __CPROVER API. Soteria will consider the function as missing a body.
  error: Analysis gave up: Unsupported: Cannot call external function: __CPROVER_assume_<id> in main
  
  Verification Failure! (Unsupported features)
  
  Executed 2 statements
  Exit code: 2

Behaves as expected with the API declared.
  $ ../exec_test.sh test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --cbmc-compat
  error: Failed assertion in main
      --> test.c:7:3
    7 |    __CPROVER_assert(0, "");
      |    ^^^^^^^^^^^^^^^^^^^^^^^
      |    |
      |    Triggering operation
      |    1: Called from here
  
  Verification Failure!
  
  Executed 3 statements
  Exit code: 13
