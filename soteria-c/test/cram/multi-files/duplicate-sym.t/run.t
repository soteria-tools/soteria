  $ soteria-c gen-summaries file1.c file2.c -I .
  Summaries for test_485:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 0); memory_leak = false }
      manifest bugs: []
  
We avoid printing the backtrace in the test output to reduce flakiness of the test
  $ OCAMLRUNPARAM="b=0" soteria-c gen-summaries file1.c file2.c -I . --no-ignore-duplicate-symbols
  LinkError: Duplicate external name duplicate_sym with trace []
  soteria-c: internal error, uncaught exception:
             Failure("Failed to parse AIL")
             
  [125]

