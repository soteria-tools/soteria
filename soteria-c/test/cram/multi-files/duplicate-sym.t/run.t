  $ soteria-c gen-summaries file1.c file2.c -I . --dump-summaries "out.summaries" && cat out.summaries
  Summaries for test_486:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 0) };
      manifest_bugs = []}
  
We avoid printing the backtrace in the test output to reduce flakiness of the test
  $ OCAMLRUNPARAM="b=0" soteria-c gen-summaries file1.c file2.c -I . --no-ignore-duplicate-symbols --dump-summaries "out.summaries" && cat out.summaries
  LinkError: Duplicate external name duplicate_sym with trace []
  soteria-c: internal error, uncaught exception:
             Failure("Failed to parse AIL")
             
  [125]

