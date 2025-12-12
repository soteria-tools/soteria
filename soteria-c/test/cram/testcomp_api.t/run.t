Warning and unspported with __VERIFIER enabled 
  $ soteria-c exec test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  [WARN] Test-Comp support is not enabled, but detected use of the __VERIFIER API. Soteria will consider the function as missing a body.
  error: Analysis gave up: Unsupported: Cannot call external function: __VERIFIER_nondet_int_559 in main
  Executed 2 statements
  Verification Failure! (Unsupported features)
  [2]

Behaves as expected with the API declared.
  $ soteria-c exec test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --testcomp-compat
  [WARN] Test-Comp compatibility mode enabled. Note that Soteria-C is *not* optimised for this test suite, and does not aim to be performant on it.
  error: Failed assertion in main
      ┌─ test.c:7:3
    7 │    __assert_fail();
      │    ^^^^^^^^^^^^^^^
      │    │
      │    Triggering operation
      │    1: Called from here
  Executed 3 statements
  Verification Failure!
  [13]

