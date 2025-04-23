Test memory leaks
  $ soteria-rust exec-main leak.rs --clean
  soteria-rust: [ERROR] Error: Errors: [Memory leak with trace [( (virtual):0:0-0,
                                    Triggering memory operation)]]
  [1]

Test reading the max char, and then incrementing it (invalid)
  $ soteria-rust exec-main char_min_max.rs --clean
  Done. - Ran 1 branches
  PC: empty
