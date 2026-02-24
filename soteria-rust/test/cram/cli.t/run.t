  $ soteria-rust --help
  NAME
         soteria-rust
  
  SYNOPSIS
         soteria-rust COMMAND …
  
  COMMANDS
         auto [OPTION]… PATH
             Run bi-abduction
  
         build-plugins [OPTION]…
             Build plugins
  
         exec [OPTION]… PATH
             Run symbolic execution
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-rust exits with:
  
         0   on success
  
         1   on failure (bug or error found)
  
         2   on crash caused by Soteria Rust
  
         3   on crash caused by Charon
  

  $ soteria-rust exec --help
  NAME
         soteria-rust-exec - Run symbolic execution
  
  SYNOPSIS
         soteria-rust exec [OPTION]… PATH
  
         Run Soteria Rust on the specified file or crate; this will either use
         Rustc to compile that file only, or use Cargo to compile the whole
         crate if it's a directory. It will then look for all entrypoints and
         execute them symbolically.
  
  ARGUMENTS
         PATH (required)
             The .rs file or the directory of the crate to analyse
  
  OPTIONS
         --approx-floating-ops=ENUM (absent=warn)
             Whether to allow complex floating-point operations to be
             over-approximated. Applies to e.g. sqrt, exp, pow and
             trigonometric functions. If deny, will vanish execution when
             encountering them.
  
         --branch-fuel=INT (absent BRANCH_FUEL env)
             The default branch fuel for each entrypoint -- every symbolic
             execution branching point counts as one fuel. Defaults to infinite
             fuel.
  
         --charon-path=VAL (absent=charon or SOTERIA_CHARON_PATH env)
             Path to the charon binary. Defaults to "charon", i.e. looked up in
             PATH.
  
         --clean (absent SOTERIA_RUST_CLEANUP env)
             Clean up compiled files after execution
  
         --compact (absent SOTERIA_COMPACT_DIAGNOSTICS env)
             Make diagnostic outputs compact.
  
         --dump-smt-to=VAL, --dump-smt=VAL
             Dump the SMT queries to the given file
  
         --exclude=[,…]
             Filter the entrypoints to exclude, by name. If empty, no
             entrypoints are excluded. Multiple filters can be provided,
             comma-separated; tests matching any will be excluded. The filters
             are treated as regexes. Opposite of --filter.
  
         --fail-fast (absent FAIL_FAST env)
             Stop symbolic execution upon the first error encountered.
  
         --filter=[,…]
             Filter the entrypoints to run, by name. If empty, all entrypoints
             are run. Multiple filters can be provided, comma-separated; tests
             matching any will be selected. The filters are treated as regexes.
             Opposite of --exclude.
  
         --frontend=ENUM (absent=obol)
             Choose the frontend to use: Charon or Obol
  
         --hide-unstable, --diffable (absent HIDE_UNSTABLE env)
             Do not display unstable values like durations (e.g. for diffing
             purposes).
  
         --html
             HTML logging, clashes with --log-kind
  
         --ignore-aliasing
             Ignore pointer aliasing rules (tree borrows)
  
         --ignore-leaks
             Ignore memory leaks
  
         --kani
             Use the Kani library
  
         -l ENUM, --log_kind=ENUM
             Log kind, clashes with --html
  
         --log-compilation
             Log the compilation process
  
         --log-smt
             Always log SMT queries, even in silent mode
  
         --miri
             Use the Miri library
  
         --no-colour, --no-color (absent NO_COLOR env)
             Disables coloured output.
  
         --no-compile
             Do not compile the Rust code, as it is already compiled
  
         --no-compile-plugins
             Do not compile the plugins, as they are already compiled
  
         --obol-path=VAL (absent=obol or SOTERIA_OBOL_PATH env)
             Path to the obol binary. Defaults to "obol", i.e. looked up in
             PATH.
  
         --output-crate
             Pretty-print the compiled crate to a file
  
         --plugins=VAL (absent SOTERIA_RUST_PLUGINS env)
             The directory in which plugins are and should be compiled;
             defaults to the current dune-managed site.
  
         --polymorphic, --poly
             Whether compilation (and thus analysis) should be done on
             polymorphic code (experimental), rather than on monomorphic code
             (with generics substituted).
  
         --provenance=ENUM (absent=permissive)
             The provenance model to use for pointers. If not provided, the
             default is permissive.
  
         --recursive-validity=ENUM (absent=warn)
             Whether to check the validity of the addressed memory when
             obtaining a reference to it. We only go one level deep.
  
         --rustc=[,…] (absent RUSTC_FLAGS env)
             Additional flags to pass to the Rustc compiler
  
         --show-pcs, --pcs (absent SHOW_PCS env)
             Whether to show the path conditions for outcomes at the end of
             execution.
  
         --solver-timeout=INT (absent SOTERIA_SOLVER_TIMEOUT env)
             Set the solver timeout in miliseconds
  
         --stats=VAL, --output-stats=VAL, --dump-stats=VAL (absent
         SOTERIA_OUTPUT_STATS env)
             If stats should be output. If the value is "stdout", prints the
             stats to stdout; otherwise, stores them as JSON in the specified
             file.
  
         --step-fuel=INT (absent STEP_FUEL env)
             The default step fuel for each entrypoint -- every control flow
             jump counts as one fuel. Defaults to infinite fuel.
  
         --summary
             If a summary of all test cases should be printed at the end of
             execution
  
         --sysroot=VAL (absent RUST_SYSROOT env)
             The sysroot to use for compilation. If not provided, the default
             sysroot is used.
  
         --target=VAL (absent TARGET env)
             The compilation target triple to use, e.g.
             x86_64-unknown-linux-gnu. If not provided, the default target for
             the current machine is used.
  
         -v, --verbose
             Verbosity level, clashes with -q
  
         --z3-path=VAL (absent=z3 or SOTERIA_Z3_PATH env)
             Path to the Z3 executable
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-rust exec exits with:
  
         0   on success
  
         1   on failure (bug or error found)
  
         2   on crash caused by Soteria Rust
  
         3   on crash caused by Charon
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-rust exec:
  
         BRANCH_FUEL
             See option --branch-fuel.
  
         FAIL_FAST
             See option --fail-fast.
  
         HIDE_UNSTABLE
             See option --hide-unstable.
  
         NO_COLOR
             See option --no-color.
  
         RUSTC_FLAGS
             See option --rustc.
  
         RUST_SYSROOT
             See option --sysroot.
  
         SHOW_PCS
             See option --show-pcs.
  
         SOTERIA_CHARON_PATH
             See option --charon-path.
  
         SOTERIA_COMPACT_DIAGNOSTICS
             See option --compact.
  
         SOTERIA_OBOL_PATH
             See option --obol-path.
  
         SOTERIA_OUTPUT_STATS
             See option --output-stats.
  
         SOTERIA_RUST_CLEANUP
             See option --clean.
  
         SOTERIA_RUST_PLUGINS
             See option --plugins.
  
         SOTERIA_SOLVER_TIMEOUT
             See option --solver-timeout.
  
         SOTERIA_Z3_PATH
             See option --z3-path.
  
         STEP_FUEL
             See option --step-fuel.
  
         TARGET
             See option --target.
  
  SEE ALSO
         soteria-rust(1)
  
  $ soteria-rust auto --help
  NAME
         soteria-rust-auto - Run bi-abduction
  
  SYNOPSIS
         soteria-rust auto [OPTION]… PATH
  
         Perform bi-abduction on the specified file or crate; will analyse all
         functions compositionally, and look for reachable bugs.
  
  ARGUMENTS
         PATH (required)
             The .rs file or the directory of the crate to analyse
  
  OPTIONS
         --approx-floating-ops=ENUM (absent=warn)
             Whether to allow complex floating-point operations to be
             over-approximated. Applies to e.g. sqrt, exp, pow and
             trigonometric functions. If deny, will vanish execution when
             encountering them.
  
         --branch-fuel=INT (absent BRANCH_FUEL env)
             The default branch fuel for each entrypoint -- every symbolic
             execution branching point counts as one fuel. Defaults to infinite
             fuel.
  
         --charon-path=VAL (absent=charon or SOTERIA_CHARON_PATH env)
             Path to the charon binary. Defaults to "charon", i.e. looked up in
             PATH.
  
         --clean (absent SOTERIA_RUST_CLEANUP env)
             Clean up compiled files after execution
  
         --compact (absent SOTERIA_COMPACT_DIAGNOSTICS env)
             Make diagnostic outputs compact.
  
         --dump-smt-to=VAL, --dump-smt=VAL
             Dump the SMT queries to the given file
  
         --exclude=[,…]
             Filter the entrypoints to exclude, by name. If empty, no
             entrypoints are excluded. Multiple filters can be provided,
             comma-separated; tests matching any will be excluded. The filters
             are treated as regexes. Opposite of --filter.
  
         --fail-fast (absent FAIL_FAST env)
             Stop symbolic execution upon the first error encountered.
  
         --filter=[,…]
             Filter the entrypoints to run, by name. If empty, all entrypoints
             are run. Multiple filters can be provided, comma-separated; tests
             matching any will be selected. The filters are treated as regexes.
             Opposite of --exclude.
  
         --frontend=ENUM (absent=obol)
             Choose the frontend to use: Charon or Obol
  
         --hide-unstable, --diffable (absent HIDE_UNSTABLE env)
             Do not display unstable values like durations (e.g. for diffing
             purposes).
  
         --html
             HTML logging, clashes with --log-kind
  
         --ignore-aliasing
             Ignore pointer aliasing rules (tree borrows)
  
         --ignore-leaks
             Ignore memory leaks
  
         --kani
             Use the Kani library
  
         -l ENUM, --log_kind=ENUM
             Log kind, clashes with --html
  
         --log-compilation
             Log the compilation process
  
         --log-smt
             Always log SMT queries, even in silent mode
  
         --miri
             Use the Miri library
  
         --no-colour, --no-color (absent NO_COLOR env)
             Disables coloured output.
  
         --no-compile
             Do not compile the Rust code, as it is already compiled
  
         --no-compile-plugins
             Do not compile the plugins, as they are already compiled
  
         --obol-path=VAL (absent=obol or SOTERIA_OBOL_PATH env)
             Path to the obol binary. Defaults to "obol", i.e. looked up in
             PATH.
  
         --output-crate
             Pretty-print the compiled crate to a file
  
         --plugins=VAL (absent SOTERIA_RUST_PLUGINS env)
             The directory in which plugins are and should be compiled;
             defaults to the current dune-managed site.
  
         --polymorphic, --poly
             Whether compilation (and thus analysis) should be done on
             polymorphic code (experimental), rather than on monomorphic code
             (with generics substituted).
  
         --provenance=ENUM (absent=permissive)
             The provenance model to use for pointers. If not provided, the
             default is permissive.
  
         --recursive-validity=ENUM (absent=warn)
             Whether to check the validity of the addressed memory when
             obtaining a reference to it. We only go one level deep.
  
         --rustc=[,…] (absent RUSTC_FLAGS env)
             Additional flags to pass to the Rustc compiler
  
         --show-pcs, --pcs (absent SHOW_PCS env)
             Whether to show the path conditions for outcomes at the end of
             execution.
  
         --solver-timeout=INT (absent SOTERIA_SOLVER_TIMEOUT env)
             Set the solver timeout in miliseconds
  
         --stats=VAL, --output-stats=VAL, --dump-stats=VAL (absent
         SOTERIA_OUTPUT_STATS env)
             If stats should be output. If the value is "stdout", prints the
             stats to stdout; otherwise, stores them as JSON in the specified
             file.
  
         --step-fuel=INT (absent STEP_FUEL env)
             The default step fuel for each entrypoint -- every control flow
             jump counts as one fuel. Defaults to infinite fuel.
  
         --summary
             If a summary of all test cases should be printed at the end of
             execution
  
         --sysroot=VAL (absent RUST_SYSROOT env)
             The sysroot to use for compilation. If not provided, the default
             sysroot is used.
  
         --target=VAL (absent TARGET env)
             The compilation target triple to use, e.g.
             x86_64-unknown-linux-gnu. If not provided, the default target for
             the current machine is used.
  
         -v, --verbose
             Verbosity level, clashes with -q
  
         --z3-path=VAL (absent=z3 or SOTERIA_Z3_PATH env)
             Path to the Z3 executable
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-rust auto exits with:
  
         0   on success
  
         1   on failure (bug or error found)
  
         2   on crash caused by Soteria Rust
  
         3   on crash caused by Charon
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-rust auto:
  
         BRANCH_FUEL
             See option --branch-fuel.
  
         FAIL_FAST
             See option --fail-fast.
  
         HIDE_UNSTABLE
             See option --hide-unstable.
  
         NO_COLOR
             See option --no-color.
  
         RUSTC_FLAGS
             See option --rustc.
  
         RUST_SYSROOT
             See option --sysroot.
  
         SHOW_PCS
             See option --show-pcs.
  
         SOTERIA_CHARON_PATH
             See option --charon-path.
  
         SOTERIA_COMPACT_DIAGNOSTICS
             See option --compact.
  
         SOTERIA_OBOL_PATH
             See option --obol-path.
  
         SOTERIA_OUTPUT_STATS
             See option --output-stats.
  
         SOTERIA_RUST_CLEANUP
             See option --clean.
  
         SOTERIA_RUST_PLUGINS
             See option --plugins.
  
         SOTERIA_SOLVER_TIMEOUT
             See option --solver-timeout.
  
         SOTERIA_Z3_PATH
             See option --z3-path.
  
         STEP_FUEL
             See option --step-fuel.
  
         TARGET
             See option --target.
  
  SEE ALSO
         soteria-rust(1)
  

  $ soteria-rust build-plugins --help
  NAME
         soteria-rust-build-plugins - Build plugins
  
  SYNOPSIS
         soteria-rust build-plugins [OPTION]…
  
         Build the plugins for Soteria Rust; this is done automatically when
         running Soteria Rust except when --no-compile-plugins is used, so you
         should only need to run this command if you want to build the plugins
         separately.
  
  OPTIONS
         --approx-floating-ops=ENUM (absent=warn)
             Whether to allow complex floating-point operations to be
             over-approximated. Applies to e.g. sqrt, exp, pow and
             trigonometric functions. If deny, will vanish execution when
             encountering them.
  
         --branch-fuel=INT (absent BRANCH_FUEL env)
             The default branch fuel for each entrypoint -- every symbolic
             execution branching point counts as one fuel. Defaults to infinite
             fuel.
  
         --charon-path=VAL (absent=charon or SOTERIA_CHARON_PATH env)
             Path to the charon binary. Defaults to "charon", i.e. looked up in
             PATH.
  
         --clean (absent SOTERIA_RUST_CLEANUP env)
             Clean up compiled files after execution
  
         --compact (absent SOTERIA_COMPACT_DIAGNOSTICS env)
             Make diagnostic outputs compact.
  
         --dump-smt-to=VAL, --dump-smt=VAL
             Dump the SMT queries to the given file
  
         --exclude=[,…]
             Filter the entrypoints to exclude, by name. If empty, no
             entrypoints are excluded. Multiple filters can be provided,
             comma-separated; tests matching any will be excluded. The filters
             are treated as regexes. Opposite of --filter.
  
         --fail-fast (absent FAIL_FAST env)
             Stop symbolic execution upon the first error encountered.
  
         --filter=[,…]
             Filter the entrypoints to run, by name. If empty, all entrypoints
             are run. Multiple filters can be provided, comma-separated; tests
             matching any will be selected. The filters are treated as regexes.
             Opposite of --exclude.
  
         --frontend=ENUM (absent=obol)
             Choose the frontend to use: Charon or Obol
  
         --hide-unstable, --diffable (absent HIDE_UNSTABLE env)
             Do not display unstable values like durations (e.g. for diffing
             purposes).
  
         --html
             HTML logging, clashes with --log-kind
  
         --ignore-aliasing
             Ignore pointer aliasing rules (tree borrows)
  
         --ignore-leaks
             Ignore memory leaks
  
         --kani
             Use the Kani library
  
         -l ENUM, --log_kind=ENUM
             Log kind, clashes with --html
  
         --log-compilation
             Log the compilation process
  
         --log-smt
             Always log SMT queries, even in silent mode
  
         --miri
             Use the Miri library
  
         --no-colour, --no-color (absent NO_COLOR env)
             Disables coloured output.
  
         --no-compile
             Do not compile the Rust code, as it is already compiled
  
         --no-compile-plugins
             Do not compile the plugins, as they are already compiled
  
         --obol-path=VAL (absent=obol or SOTERIA_OBOL_PATH env)
             Path to the obol binary. Defaults to "obol", i.e. looked up in
             PATH.
  
         --output-crate
             Pretty-print the compiled crate to a file
  
         --plugins=VAL (absent SOTERIA_RUST_PLUGINS env)
             The directory in which plugins are and should be compiled;
             defaults to the current dune-managed site.
  
         --polymorphic, --poly
             Whether compilation (and thus analysis) should be done on
             polymorphic code (experimental), rather than on monomorphic code
             (with generics substituted).
  
         --provenance=ENUM (absent=permissive)
             The provenance model to use for pointers. If not provided, the
             default is permissive.
  
         --recursive-validity=ENUM (absent=warn)
             Whether to check the validity of the addressed memory when
             obtaining a reference to it. We only go one level deep.
  
         --rustc=[,…] (absent RUSTC_FLAGS env)
             Additional flags to pass to the Rustc compiler
  
         --show-pcs, --pcs (absent SHOW_PCS env)
             Whether to show the path conditions for outcomes at the end of
             execution.
  
         --solver-timeout=INT (absent SOTERIA_SOLVER_TIMEOUT env)
             Set the solver timeout in miliseconds
  
         --stats=VAL, --output-stats=VAL, --dump-stats=VAL (absent
         SOTERIA_OUTPUT_STATS env)
             If stats should be output. If the value is "stdout", prints the
             stats to stdout; otherwise, stores them as JSON in the specified
             file.
  
         --step-fuel=INT (absent STEP_FUEL env)
             The default step fuel for each entrypoint -- every control flow
             jump counts as one fuel. Defaults to infinite fuel.
  
         --summary
             If a summary of all test cases should be printed at the end of
             execution
  
         --sysroot=VAL (absent RUST_SYSROOT env)
             The sysroot to use for compilation. If not provided, the default
             sysroot is used.
  
         --target=VAL (absent TARGET env)
             The compilation target triple to use, e.g.
             x86_64-unknown-linux-gnu. If not provided, the default target for
             the current machine is used.
  
         -v, --verbose
             Verbosity level, clashes with -q
  
         --z3-path=VAL (absent=z3 or SOTERIA_Z3_PATH env)
             Path to the Z3 executable
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-rust build-plugins exits with:
  
         0   on success
  
         1   on failure (bug or error found)
  
         2   on crash caused by Soteria Rust
  
         3   on crash caused by Charon
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-rust
         build-plugins:
  
         BRANCH_FUEL
             See option --branch-fuel.
  
         FAIL_FAST
             See option --fail-fast.
  
         HIDE_UNSTABLE
             See option --hide-unstable.
  
         NO_COLOR
             See option --no-color.
  
         RUSTC_FLAGS
             See option --rustc.
  
         RUST_SYSROOT
             See option --sysroot.
  
         SHOW_PCS
             See option --show-pcs.
  
         SOTERIA_CHARON_PATH
             See option --charon-path.
  
         SOTERIA_COMPACT_DIAGNOSTICS
             See option --compact.
  
         SOTERIA_OBOL_PATH
             See option --obol-path.
  
         SOTERIA_OUTPUT_STATS
             See option --output-stats.
  
         SOTERIA_RUST_CLEANUP
             See option --clean.
  
         SOTERIA_RUST_PLUGINS
             See option --plugins.
  
         SOTERIA_SOLVER_TIMEOUT
             See option --solver-timeout.
  
         SOTERIA_Z3_PATH
             See option --z3-path.
  
         STEP_FUEL
             See option --step-fuel.
  
         TARGET
             See option --target.
  
  SEE ALSO
         soteria-rust(1)
  
