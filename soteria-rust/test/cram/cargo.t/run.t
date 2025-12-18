Test running Rusteria on a crate
  $ soteria-rust cargo . --clean --no-timing
  Compiling... done in <time>
  note: tests::tests::my_test: done in <time>, ran 2 branches
  PC 1: (0x01 == V|1|) /\ (0x01 == V|1|)
  PC 2: (0x00 == V|1|) /\ (0x00 == V|1|)
  
