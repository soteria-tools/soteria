  $ soteria-c gen-summaries file1.c file2.c --dump-summaries "out.summaries" && cat out.summaries
  [WARN] Ignoring file that did not parse correctly: file1.c
  Failed to parse AIL: file1.c:4:1: error: unexpected token after '?' and before '}'
  parsing "conditional_expression": seen "logical_OR_expression QUESTION", expecting "expression COLON conditional_expression"
  }
  ^ 
  
  Summaries for test_484:
    Analysed {
      raw =
      { args = []; pre = []; pc = []; post = { heap = []; globs = [] };
        ret = (Ok 0) };
      manifest_bugs = []}
  
We do not want to print the backtrace in the test output to reduce flakiness of the test
  $ OCAMLRUNPARAM="b=0" soteria-c gen-summaries file1.c file2.c -I . --no-ignore-parse-failures  --dump-summaries "out.summaries" && cat out.summaries
  Parsing Error: Failed to parse AIL: file1.c:4:1: error: unexpected token after '?' and before '}'
  parsing "conditional_expression": seen "logical_OR_expression QUESTION", expecting "expression COLON conditional_expression"
  }
  ^  with trace
  [• file1.c:4:1-2]
  soteria-c: internal error, uncaught exception:
             Failure("Failed to parse AIL")
             
  [125]
