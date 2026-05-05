  $ soteria-rust exec array_init.rs
  Compiling... errored
  error: Fatal (Frontend): Failed compilation to ULLBC:
  ERROR in serialize_to_file at /Users/sacha/.cargo/git/checkouts/charon-f3d627c546fc4861/fa7ff71/charon/src/export.rs:99:
  Could not open: "$TESTCASE_ROOT/array_init.rs.llbc.json"
  
  thread 'main' (135341209) panicked at src/bin/obol-driver/main.rs:103:14:
  Failed to write smir.json to output: ()
  note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
  ERROR Compilation panicked
  [3]
