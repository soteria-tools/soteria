Test running Rusteria on a crate
  $ soteria-rust cargo . --clean
     Compiling my_crate v0.1.0 ($TESTCASE_ROOT)
      Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.08s
  note: Done, no errors found
  
  my_crate::tests::tests::my_test: ran 2 branches
  PC 1: (0 == V|1|)
  PC 2: (1 == V|1|)
