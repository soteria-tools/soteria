Warning and unspported with CBMC enabled 
  $ soteria-c exec test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  [WARN] CBMC support is not enabled, but detected use of the __CPROVER API. Soteria will consider the function as missing a body.
  error: Analysis gave up: Unsupported: Cannot call external function: __CPROVER_assume_563 in main
  Executed 2 statements
  Verification Failure! (Unsupported features)
  [2]

Behaves as expected with the API declared.
  $ soteria-c exec test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --cbmc-compat
  error: Failed assertion in main
      ┌─ test.c:7:3
    7 │    __CPROVER_assert(0, "");
      │    ^^^^^^^^^^^^^^^^^^^^^^^
      │    │
      │    Triggering operation
      │    1: Called from here
  Executed 3 statements
  Verification Failure!
  [13]
