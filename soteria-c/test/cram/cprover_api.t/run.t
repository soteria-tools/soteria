Warning and unspported with CBMC enabled 
  $ soteria-c exec test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  [WARN] CBMC support is not enabled, but detected use of the __CPROVER API. Soteria will consider the function as missing a body.
  Symex terminated with the following outcomes:
    [Error: Gave up: MISSING FEATURE, VANISHING: Cannot call external function: __CPROVER_assume_563]
  Executed 2 statements

Behaves as expected with the API declared.
  $ soteria-c exec test.c --no-ignore-parse-failures --no-ignore-duplicate-symbols --cbmc-compat
  Symex terminated with the following outcomes:
    [Error: Failed assertion with trace
            [• Called from here: test.c:7:3-26;
             • Triggering operation: test.c:7:3-26]]
  Executed 3 statements
