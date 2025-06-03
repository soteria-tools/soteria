  $ soteria-c gen-summaries file1.c file2.c -I . --no-progress-bar --dump-summaries "out.summaries" && cat out.summaries
  Summaries for test_486:
    { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
      ret = (Ok 0); memory_leak = false }
      manifest bugs: []
  
We avoid printing the backtrace in the test output to reduce flakiness of the test
  $ OCAMLRUNPARAM="b=0" soteria-c gen-summaries file1.c file2.c -I . --no-ignore-duplicate-symbols --no-progress-bar --dump-summaries "out.summaries" && cat out.summaries
  LinkError: Duplicate external name duplicate_sym with trace []
  soteria-c: internal error, uncaught exception:
             Failure("Failed to parse AIL")
             
  [125]

