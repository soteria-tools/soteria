  $ soteria-c --help
  NAME
         soteria-c
  
  SYNOPSIS
         soteria-c COMMAND …
  
  COMMANDS
         capture-db [OPTION]… COMPILE_COMMANDS.JSON
             Same as gen-summaries but runs on a compilation database.
  
         exec [OPTION]… FILES…
             Symbolically execute a program starting from the main function.
  
         gen-summaries [OPTION]… FILES…
             Run soteria-c in bug-finding mode. Soteria will perform
             bi-abduction and generate summaries for all functions in the code
             base. It will then analyse these summaries and report any bugs it
             finds.
  
         lsp [OPTION]…
             Run soteria-c in LSP mode for bug finding (experimental)
  
         show-ail [OPTION]… FILES…
             Parse and link the ail program and print its AST (for debugging
             purposes)
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-c exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  $ soteria-c capture-db --help
  NAME
         soteria-c-capture-db - Same as gen-summaries but runs on a compilation
         database.
  
  SYNOPSIS
         soteria-c capture-db [OPTION]… COMPILE_COMMANDS.JSON
  
  SOLVER OPTIONS
         --dump-smt-to=VAL, --dump-smt=VAL
             Dump the SMT queries to the given file
  
         --solver-timeout=INT (absent SOTERIA_SOLVER_TIMEOUT env)
             Set the solver timeout in miliseconds
  
         --z3-path=VAL (absent=z3 or SOTERIA_Z3_PATH env)
             Path to the Z3 executable
  
  PROFILING OPTIONS
         --flamegraphs=VAL (absent SOTERIA_FLAMEGRAPHS env)
             Specify the folder in which the flamegraphs will be saved.
  
  OUTPUT OPTIONS
         --compact (absent SOTERIA_COMPACT_DIAGNOSTICS env)
             Make diagnostic outputs compact.
  
         --dump-report=FILE (absent SOTERIA_DUMP_REPORT env)
             Write a JSON report of all diagnostics (bugs and errors) to the
             specified file
  
         --dump-summaries-to=FILE, --dump-summaries=FILE (absent
         SOTERIA_DUMP_SUMMARIES_FILE env)
             Dump the generated summaries to a file
  
         --print-states (absent SOTERIA_PRINT_STATES env)
             Print final program states after whole-program symbolic testing
  
         --show-manifest-summaries (absent SOTERIA_SHOW_MANIFEST_SUMMARIES env)
             Print a corresponding manifest summary after the bug report if a
             bug is found
  
         --stats=VAL, --output-stats=VAL, --dump-stats=VAL (absent
         SOTERIA_OUTPUT_STATS env)
             If stats should be output. If the value is "stdout", prints the
             stats to stdout; otherwise, stores them as JSON in the specified
             file.
  
         -v, --verbose
             Verbosity level, clashes with -q
  
  LOGS OPTIONS
         --hide-unstable, --diffable (absent HIDE_UNSTABLE env)
             Do not display unstable values like durations (e.g. for diffing
             purposes).
  
         --html
             HTML logging, clashes with --log-kind
  
         -l ENUM, --log_kind=ENUM
             Log kind, clashes with --html
  
         --log-smt
             Always log SMT queries, even in silent mode
  
         --no-colour, --no-color (absent NO_COLOR env)
             Disables coloured output.
  
  FRONTEND OPTIONS
         --auto-include-path=PATH (absent SOTERIA_AUTO_INCLUDE_PATH env)
             Path to the directory that contains the soteria-c.h
  
         -f FUNCTION_NAME
             List of functions to analyse
  
         --no-c23 (absent SOTERIA_NO_C23 env)
             Disable C23 support (even if the underlying Cerberus library
             supports it).
  
         --no-ignore-duplicate-symbols (absent
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS env)
             Programs that contain duplicate symbols are ignored by default,
             this flag deactivates that behaviour.
  
         --no-ignore-parse-failures (absent SOTERIA_NO_IGNORE_PARSE_FAILURES
         env)
             Files that cannot be parsed correctly are ignored by default, this
             flag deactivates that behaviour.
  
         --parse-only (absent SOTERIA_PARSE_ONLY env)
             Only parse and link the C program, do not perform analysis
  
         --use-cerb-headers (absent SOTERIA_USE_CERB_HEADERS env)
             Use the Cerberus-provided standard headers instead of the system
             headers.
  
         --write-parsed-db=FILE, --write-parsed-compilation-db=FILE (absent
         SOTERIA_WRITE_PARSED_DB env)
             When using a compilation database, write a filtered version
             containing only successfully parsed files to the specified path
  
  ANALYSIS OPTIONS
         --alloc-cannot-fail (absent SOTERIA_ALLOC_CANNOT_FAIL env)
             Assume allocations cannot fail
  
         --cbmc-compat, --cbmc (absent SOTERIA_CBMC_COMPAT env)
             Enable support for a subset of the __CPROVER_ API.
  
         --havoc-undefined-funs, --havoc-undef
             Assume that all undefined functions can return any value. Warning:
             this can lead to unsoundnesses in analyses.
  
         --ignore-ub (absent SOTERIA_IGNORE_UB env)
             Ignores undefined behaviour branches (mostly for Test-Comp's weird
             requirements). Branches reaching UB will be dismissed. Only has
             effect in symbolic testing mode.
  
         --testcomp-compat, --testcomp (absent SOTERIA_TESTCOMP_COMPAT env)
             Enable support for a subset of the testcomp API (e.g.,
             __VERIFIER_nondet_
  
  ARGUMENTS
         COMPILE_COMMANDS.JSON (required)
             JSON file following the Clang compilation database format
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-c capture-db exits with:
  
         0   on successful analysis (no bugs found).
  
         2   when Soteria-C did not complete the analysis because of a missing
             feature or internal error.
  
         13  when a bug or error was found in the analysed program.
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-c
         capture-db:
  
         HIDE_UNSTABLE
             See option --hide-unstable.
  
         NO_COLOR
             See option --no-color.
  
         SOTERIA_ALLOC_CANNOT_FAIL
             See option --alloc-cannot-fail.
  
         SOTERIA_AUTO_INCLUDE_PATH
             See option --auto-include-path.
  
         SOTERIA_CBMC_COMPAT
             See option --cbmc.
  
         SOTERIA_COMPACT_DIAGNOSTICS
             See option --compact.
  
         SOTERIA_DUMP_REPORT
             See option --dump-report.
  
         SOTERIA_DUMP_SUMMARIES_FILE
             See option --dump-summaries.
  
         SOTERIA_FLAMEGRAPHS
             See option --flamegraphs.
  
         SOTERIA_IGNORE_UB
             See option --ignore-ub.
  
         SOTERIA_NO_C23
             See option --no-c23.
  
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS
             See option --no-ignore-duplicate-symbols.
  
         SOTERIA_NO_IGNORE_PARSE_FAILURES
             See option --no-ignore-parse-failures.
  
         SOTERIA_OUTPUT_STATS
             See option --output-stats.
  
         SOTERIA_PARSE_ONLY
             See option --parse-only.
  
         SOTERIA_PRINT_STATES
             See option --print-states.
  
         SOTERIA_SHOW_MANIFEST_SUMMARIES
             See option --show-manifest-summaries.
  
         SOTERIA_SOLVER_TIMEOUT
             See option --solver-timeout.
  
         SOTERIA_TESTCOMP_COMPAT
             See option --testcomp.
  
         SOTERIA_USE_CERB_HEADERS
             See option --use-cerb-headers.
  
         SOTERIA_WRITE_PARSED_DB
             See option --write-parsed-db.
  
         SOTERIA_Z3_PATH
             See option --z3-path.
  
  SEE ALSO
         soteria-c(1)
  
  $ soteria-c exec --help
  NAME
         soteria-c-exec - Symbolically execute a program starting from the main
         function.
  
  SYNOPSIS
         soteria-c exec [OPTION]… FILES…
  
  SOLVER OPTIONS
         --dump-smt-to=VAL, --dump-smt=VAL
             Dump the SMT queries to the given file
  
         --solver-timeout=INT (absent SOTERIA_SOLVER_TIMEOUT env)
             Set the solver timeout in miliseconds
  
         --z3-path=VAL (absent=z3 or SOTERIA_Z3_PATH env)
             Path to the Z3 executable
  
  PROFILING OPTIONS
         --flamegraphs=VAL (absent SOTERIA_FLAMEGRAPHS env)
             Specify the folder in which the flamegraphs will be saved.
  
  OUTPUT OPTIONS
         --compact (absent SOTERIA_COMPACT_DIAGNOSTICS env)
             Make diagnostic outputs compact.
  
         --dump-report=FILE (absent SOTERIA_DUMP_REPORT env)
             Write a JSON report of all diagnostics (bugs and errors) to the
             specified file
  
         --dump-summaries-to=FILE, --dump-summaries=FILE (absent
         SOTERIA_DUMP_SUMMARIES_FILE env)
             Dump the generated summaries to a file
  
         --print-states (absent SOTERIA_PRINT_STATES env)
             Print final program states after whole-program symbolic testing
  
         --show-manifest-summaries (absent SOTERIA_SHOW_MANIFEST_SUMMARIES env)
             Print a corresponding manifest summary after the bug report if a
             bug is found
  
         --stats=VAL, --output-stats=VAL, --dump-stats=VAL (absent
         SOTERIA_OUTPUT_STATS env)
             If stats should be output. If the value is "stdout", prints the
             stats to stdout; otherwise, stores them as JSON in the specified
             file.
  
         -v, --verbose
             Verbosity level, clashes with -q
  
  LOGS OPTIONS
         --hide-unstable, --diffable (absent HIDE_UNSTABLE env)
             Do not display unstable values like durations (e.g. for diffing
             purposes).
  
         --html
             HTML logging, clashes with --log-kind
  
         -l ENUM, --log_kind=ENUM
             Log kind, clashes with --html
  
         --log-smt
             Always log SMT queries, even in silent mode
  
         --no-colour, --no-color (absent NO_COLOR env)
             Disables coloured output.
  
  FRONTEND OPTIONS
         --auto-include-path=PATH (absent SOTERIA_AUTO_INCLUDE_PATH env)
             Path to the directory that contains the soteria-c.h
  
         --harness=ENTRYPOINT, --entry-point=ENTRYPOINT, --entry=ENTRYPOINT
         (absent=main)
             Entry point of the program to execute
  
         -I DIR
             Add a directory to the include path
  
         --no-c23 (absent SOTERIA_NO_C23 env)
             Disable C23 support (even if the underlying Cerberus library
             supports it).
  
         --no-ignore-duplicate-symbols (absent
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS env)
             Programs that contain duplicate symbols are ignored by default,
             this flag deactivates that behaviour.
  
         --no-ignore-parse-failures (absent SOTERIA_NO_IGNORE_PARSE_FAILURES
         env)
             Files that cannot be parsed correctly are ignored by default, this
             flag deactivates that behaviour.
  
         --parse-only (absent SOTERIA_PARSE_ONLY env)
             Only parse and link the C program, do not perform analysis
  
         --use-cerb-headers (absent SOTERIA_USE_CERB_HEADERS env)
             Use the Cerberus-provided standard headers instead of the system
             headers.
  
         --write-parsed-db=FILE, --write-parsed-compilation-db=FILE (absent
         SOTERIA_WRITE_PARSED_DB env)
             When using a compilation database, write a filtered version
             containing only successfully parsed files to the specified path
  
  ANALYSIS OPTIONS
         --alloc-cannot-fail (absent SOTERIA_ALLOC_CANNOT_FAIL env)
             Assume allocations cannot fail
  
         --branching-fuel=N, --branch-fuel=N (absent SOTERIA_BRANCHING_FUEL
         env)
             How many times symbolic execution may branch, may yield ~2^N
             branches!, clashes with --infinite-fuel if finite. Default: 4
  
         --cbmc-compat, --cbmc (absent SOTERIA_CBMC_COMPAT env)
             Enable support for a subset of the __CPROVER_ API.
  
         --havoc-undefined-funs, --havoc-undef
             Assume that all undefined functions can return any value. Warning:
             this can lead to unsoundnesses in analyses.
  
         --ignore-ub (absent SOTERIA_IGNORE_UB env)
             Ignores undefined behaviour branches (mostly for Test-Comp's weird
             requirements). Branches reaching UB will be dismissed. Only has
             effect in symbolic testing mode.
  
         --infinite-fuel (absent SOTERIA_INFINITE_FUEL env)
             Use infinite fuel (may not terminate). Default: false
  
         --step-fuel=VALUE (absent SOTERIA_STEP_FUEL env)
             How many symbolic execution steps (~1 statement) are executed per
             branch, clashes with --infinite-fuel if finite. Default: 150
  
         --testcomp-compat, --testcomp (absent SOTERIA_TESTCOMP_COMPAT env)
             Enable support for a subset of the testcomp API (e.g.,
             __VERIFIER_nondet_
  
  ARGUMENTS
         FILES (required)
             FILES
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-c exec exits with:
  
         0   on successful analysis (no bugs found).
  
         2   when Soteria-C did not complete the analysis because of a missing
             feature or internal error.
  
         13  when a bug or error was found in the analysed program.
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-c exec:
  
         HIDE_UNSTABLE
             See option --hide-unstable.
  
         NO_COLOR
             See option --no-color.
  
         SOTERIA_ALLOC_CANNOT_FAIL
             See option --alloc-cannot-fail.
  
         SOTERIA_AUTO_INCLUDE_PATH
             See option --auto-include-path.
  
         SOTERIA_BRANCHING_FUEL
             See option --branching-fuel.
  
         SOTERIA_CBMC_COMPAT
             See option --cbmc.
  
         SOTERIA_COMPACT_DIAGNOSTICS
             See option --compact.
  
         SOTERIA_DUMP_REPORT
             See option --dump-report.
  
         SOTERIA_DUMP_SUMMARIES_FILE
             See option --dump-summaries.
  
         SOTERIA_FLAMEGRAPHS
             See option --flamegraphs.
  
         SOTERIA_IGNORE_UB
             See option --ignore-ub.
  
         SOTERIA_INFINITE_FUEL
             See option --infinite-fuel.
  
         SOTERIA_NO_C23
             See option --no-c23.
  
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS
             See option --no-ignore-duplicate-symbols.
  
         SOTERIA_NO_IGNORE_PARSE_FAILURES
             See option --no-ignore-parse-failures.
  
         SOTERIA_OUTPUT_STATS
             See option --output-stats.
  
         SOTERIA_PARSE_ONLY
             See option --parse-only.
  
         SOTERIA_PRINT_STATES
             See option --print-states.
  
         SOTERIA_SHOW_MANIFEST_SUMMARIES
             See option --show-manifest-summaries.
  
         SOTERIA_SOLVER_TIMEOUT
             See option --solver-timeout.
  
         SOTERIA_STEP_FUEL
             See option --step-fuel.
  
         SOTERIA_TESTCOMP_COMPAT
             See option --testcomp.
  
         SOTERIA_USE_CERB_HEADERS
             See option --use-cerb-headers.
  
         SOTERIA_WRITE_PARSED_DB
             See option --write-parsed-db.
  
         SOTERIA_Z3_PATH
             See option --z3-path.
  
  SEE ALSO
         soteria-c(1)
  
  $ soteria-c gen-summaries --help
  NAME
         soteria-c-gen-summaries - Run soteria-c in bug-finding mode. Soteria
         will perform bi-abduction and generate summaries for all functions in
         the code base. It will then analyse these summaries and report any
         bugs it finds.
  
  SYNOPSIS
         soteria-c gen-summaries [OPTION]… FILES…
  
  SOLVER OPTIONS
         --dump-smt-to=VAL, --dump-smt=VAL
             Dump the SMT queries to the given file
  
         --solver-timeout=INT (absent SOTERIA_SOLVER_TIMEOUT env)
             Set the solver timeout in miliseconds
  
         --z3-path=VAL (absent=z3 or SOTERIA_Z3_PATH env)
             Path to the Z3 executable
  
  PROFILING OPTIONS
         --flamegraphs=VAL (absent SOTERIA_FLAMEGRAPHS env)
             Specify the folder in which the flamegraphs will be saved.
  
  OUTPUT OPTIONS
         --compact (absent SOTERIA_COMPACT_DIAGNOSTICS env)
             Make diagnostic outputs compact.
  
         --dump-report=FILE (absent SOTERIA_DUMP_REPORT env)
             Write a JSON report of all diagnostics (bugs and errors) to the
             specified file
  
         --dump-summaries-to=FILE, --dump-summaries=FILE (absent
         SOTERIA_DUMP_SUMMARIES_FILE env)
             Dump the generated summaries to a file
  
         --print-states (absent SOTERIA_PRINT_STATES env)
             Print final program states after whole-program symbolic testing
  
         --show-manifest-summaries (absent SOTERIA_SHOW_MANIFEST_SUMMARIES env)
             Print a corresponding manifest summary after the bug report if a
             bug is found
  
         --stats=VAL, --output-stats=VAL, --dump-stats=VAL (absent
         SOTERIA_OUTPUT_STATS env)
             If stats should be output. If the value is "stdout", prints the
             stats to stdout; otherwise, stores them as JSON in the specified
             file.
  
         -v, --verbose
             Verbosity level, clashes with -q
  
  LOGS OPTIONS
         --hide-unstable, --diffable (absent HIDE_UNSTABLE env)
             Do not display unstable values like durations (e.g. for diffing
             purposes).
  
         --html
             HTML logging, clashes with --log-kind
  
         -l ENUM, --log_kind=ENUM
             Log kind, clashes with --html
  
         --log-smt
             Always log SMT queries, even in silent mode
  
         --no-colour, --no-color (absent NO_COLOR env)
             Disables coloured output.
  
  FRONTEND OPTIONS
         --auto-include-path=PATH (absent SOTERIA_AUTO_INCLUDE_PATH env)
             Path to the directory that contains the soteria-c.h
  
         -f FUNCTION_NAME
             List of functions to analyse
  
         -I DIR
             Add a directory to the include path
  
         --no-c23 (absent SOTERIA_NO_C23 env)
             Disable C23 support (even if the underlying Cerberus library
             supports it).
  
         --no-ignore-duplicate-symbols (absent
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS env)
             Programs that contain duplicate symbols are ignored by default,
             this flag deactivates that behaviour.
  
         --no-ignore-parse-failures (absent SOTERIA_NO_IGNORE_PARSE_FAILURES
         env)
             Files that cannot be parsed correctly are ignored by default, this
             flag deactivates that behaviour.
  
         --parse-only (absent SOTERIA_PARSE_ONLY env)
             Only parse and link the C program, do not perform analysis
  
         --use-cerb-headers (absent SOTERIA_USE_CERB_HEADERS env)
             Use the Cerberus-provided standard headers instead of the system
             headers.
  
         --write-parsed-db=FILE, --write-parsed-compilation-db=FILE (absent
         SOTERIA_WRITE_PARSED_DB env)
             When using a compilation database, write a filtered version
             containing only successfully parsed files to the specified path
  
  ANALYSIS OPTIONS
         --alloc-cannot-fail (absent SOTERIA_ALLOC_CANNOT_FAIL env)
             Assume allocations cannot fail
  
         --cbmc-compat, --cbmc (absent SOTERIA_CBMC_COMPAT env)
             Enable support for a subset of the __CPROVER_ API.
  
         --havoc-undefined-funs, --havoc-undef
             Assume that all undefined functions can return any value. Warning:
             this can lead to unsoundnesses in analyses.
  
         --ignore-ub (absent SOTERIA_IGNORE_UB env)
             Ignores undefined behaviour branches (mostly for Test-Comp's weird
             requirements). Branches reaching UB will be dismissed. Only has
             effect in symbolic testing mode.
  
         --testcomp-compat, --testcomp (absent SOTERIA_TESTCOMP_COMPAT env)
             Enable support for a subset of the testcomp API (e.g.,
             __VERIFIER_nondet_
  
  ARGUMENTS
         FILES (required)
             FILES
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-c gen-summaries exits with:
  
         0   on successful analysis (no bugs found).
  
         2   when Soteria-C did not complete the analysis because of a missing
             feature or internal error.
  
         13  when a bug or error was found in the analysed program.
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-c
         gen-summaries:
  
         HIDE_UNSTABLE
             See option --hide-unstable.
  
         NO_COLOR
             See option --no-color.
  
         SOTERIA_ALLOC_CANNOT_FAIL
             See option --alloc-cannot-fail.
  
         SOTERIA_AUTO_INCLUDE_PATH
             See option --auto-include-path.
  
         SOTERIA_CBMC_COMPAT
             See option --cbmc.
  
         SOTERIA_COMPACT_DIAGNOSTICS
             See option --compact.
  
         SOTERIA_DUMP_REPORT
             See option --dump-report.
  
         SOTERIA_DUMP_SUMMARIES_FILE
             See option --dump-summaries.
  
         SOTERIA_FLAMEGRAPHS
             See option --flamegraphs.
  
         SOTERIA_IGNORE_UB
             See option --ignore-ub.
  
         SOTERIA_NO_C23
             See option --no-c23.
  
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS
             See option --no-ignore-duplicate-symbols.
  
         SOTERIA_NO_IGNORE_PARSE_FAILURES
             See option --no-ignore-parse-failures.
  
         SOTERIA_OUTPUT_STATS
             See option --output-stats.
  
         SOTERIA_PARSE_ONLY
             See option --parse-only.
  
         SOTERIA_PRINT_STATES
             See option --print-states.
  
         SOTERIA_SHOW_MANIFEST_SUMMARIES
             See option --show-manifest-summaries.
  
         SOTERIA_SOLVER_TIMEOUT
             See option --solver-timeout.
  
         SOTERIA_TESTCOMP_COMPAT
             See option --testcomp.
  
         SOTERIA_USE_CERB_HEADERS
             See option --use-cerb-headers.
  
         SOTERIA_WRITE_PARSED_DB
             See option --write-parsed-db.
  
         SOTERIA_Z3_PATH
             See option --z3-path.
  
  SEE ALSO
         soteria-c(1)
  
  $ soteria-c lsp --help
  NAME
         soteria-c-lsp - Run soteria-c in LSP mode for bug finding
         (experimental)
  
  SYNOPSIS
         soteria-c lsp [OPTION]…
  
  OUTPUT OPTIONS
         --dump-report=FILE (absent SOTERIA_DUMP_REPORT env)
             Write a JSON report of all diagnostics (bugs and errors) to the
             specified file
  
         --dump-summaries-to=FILE, --dump-summaries=FILE (absent
         SOTERIA_DUMP_SUMMARIES_FILE env)
             Dump the generated summaries to a file
  
         --print-states (absent SOTERIA_PRINT_STATES env)
             Print final program states after whole-program symbolic testing
  
         --show-manifest-summaries (absent SOTERIA_SHOW_MANIFEST_SUMMARIES env)
             Print a corresponding manifest summary after the bug report if a
             bug is found
  
  FRONTEND OPTIONS
         --auto-include-path=PATH (absent SOTERIA_AUTO_INCLUDE_PATH env)
             Path to the directory that contains the soteria-c.h
  
         --no-c23 (absent SOTERIA_NO_C23 env)
             Disable C23 support (even if the underlying Cerberus library
             supports it).
  
         --no-ignore-duplicate-symbols (absent
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS env)
             Programs that contain duplicate symbols are ignored by default,
             this flag deactivates that behaviour.
  
         --no-ignore-parse-failures (absent SOTERIA_NO_IGNORE_PARSE_FAILURES
         env)
             Files that cannot be parsed correctly are ignored by default, this
             flag deactivates that behaviour.
  
         --parse-only (absent SOTERIA_PARSE_ONLY env)
             Only parse and link the C program, do not perform analysis
  
         --use-cerb-headers (absent SOTERIA_USE_CERB_HEADERS env)
             Use the Cerberus-provided standard headers instead of the system
             headers.
  
         --write-parsed-db=FILE, --write-parsed-compilation-db=FILE (absent
         SOTERIA_WRITE_PARSED_DB env)
             When using a compilation database, write a filtered version
             containing only successfully parsed files to the specified path
  
  ANALYSIS OPTIONS
         --alloc-cannot-fail (absent SOTERIA_ALLOC_CANNOT_FAIL env)
             Assume allocations cannot fail
  
         --cbmc-compat, --cbmc (absent SOTERIA_CBMC_COMPAT env)
             Enable support for a subset of the __CPROVER_ API.
  
         --havoc-undefined-funs, --havoc-undef
             Assume that all undefined functions can return any value. Warning:
             this can lead to unsoundnesses in analyses.
  
         --ignore-ub (absent SOTERIA_IGNORE_UB env)
             Ignores undefined behaviour branches (mostly for Test-Comp's weird
             requirements). Branches reaching UB will be dismissed. Only has
             effect in symbolic testing mode.
  
         --testcomp-compat, --testcomp (absent SOTERIA_TESTCOMP_COMPAT env)
             Enable support for a subset of the testcomp API (e.g.,
             __VERIFIER_nondet_
  
  OPTIONS
         --version
             Print version information
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-c lsp exits with:
  
         0   on successful analysis (no bugs found).
  
         2   when Soteria-C did not complete the analysis because of a missing
             feature or internal error.
  
         13  when a bug or error was found in the analysed program.
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-c lsp:
  
         SOTERIA_ALLOC_CANNOT_FAIL
             See option --alloc-cannot-fail.
  
         SOTERIA_AUTO_INCLUDE_PATH
             See option --auto-include-path.
  
         SOTERIA_CBMC_COMPAT
             See option --cbmc.
  
         SOTERIA_DUMP_REPORT
             See option --dump-report.
  
         SOTERIA_DUMP_SUMMARIES_FILE
             See option --dump-summaries.
  
         SOTERIA_IGNORE_UB
             See option --ignore-ub.
  
         SOTERIA_NO_C23
             See option --no-c23.
  
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS
             See option --no-ignore-duplicate-symbols.
  
         SOTERIA_NO_IGNORE_PARSE_FAILURES
             See option --no-ignore-parse-failures.
  
         SOTERIA_PARSE_ONLY
             See option --parse-only.
  
         SOTERIA_PRINT_STATES
             See option --print-states.
  
         SOTERIA_SHOW_MANIFEST_SUMMARIES
             See option --show-manifest-summaries.
  
         SOTERIA_TESTCOMP_COMPAT
             See option --testcomp.
  
         SOTERIA_USE_CERB_HEADERS
             See option --use-cerb-headers.
  
         SOTERIA_WRITE_PARSED_DB
             See option --write-parsed-db.
  
  SEE ALSO
         soteria-c(1)
  
  $ soteria-c show-ail --help
  NAME
         soteria-c-show-ail - Parse and link the ail program and print its AST
         (for debugging purposes)
  
  SYNOPSIS
         soteria-c show-ail [OPTION]… FILES…
  
  SOLVER OPTIONS
         --dump-smt-to=VAL, --dump-smt=VAL
             Dump the SMT queries to the given file
  
         --solver-timeout=INT (absent SOTERIA_SOLVER_TIMEOUT env)
             Set the solver timeout in miliseconds
  
         --z3-path=VAL (absent=z3 or SOTERIA_Z3_PATH env)
             Path to the Z3 executable
  
  PROFILING OPTIONS
         --flamegraphs=VAL (absent SOTERIA_FLAMEGRAPHS env)
             Specify the folder in which the flamegraphs will be saved.
  
  OUTPUT OPTIONS
         --compact (absent SOTERIA_COMPACT_DIAGNOSTICS env)
             Make diagnostic outputs compact.
  
         --dump-report=FILE (absent SOTERIA_DUMP_REPORT env)
             Write a JSON report of all diagnostics (bugs and errors) to the
             specified file
  
         --dump-summaries-to=FILE, --dump-summaries=FILE (absent
         SOTERIA_DUMP_SUMMARIES_FILE env)
             Dump the generated summaries to a file
  
         --print-states (absent SOTERIA_PRINT_STATES env)
             Print final program states after whole-program symbolic testing
  
         --show-manifest-summaries (absent SOTERIA_SHOW_MANIFEST_SUMMARIES env)
             Print a corresponding manifest summary after the bug report if a
             bug is found
  
         --stats=VAL, --output-stats=VAL, --dump-stats=VAL (absent
         SOTERIA_OUTPUT_STATS env)
             If stats should be output. If the value is "stdout", prints the
             stats to stdout; otherwise, stores them as JSON in the specified
             file.
  
         -v, --verbose
             Verbosity level, clashes with -q
  
  LOGS OPTIONS
         --hide-unstable, --diffable (absent HIDE_UNSTABLE env)
             Do not display unstable values like durations (e.g. for diffing
             purposes).
  
         --html
             HTML logging, clashes with --log-kind
  
         -l ENUM, --log_kind=ENUM
             Log kind, clashes with --html
  
         --log-smt
             Always log SMT queries, even in silent mode
  
         --no-colour, --no-color (absent NO_COLOR env)
             Disables coloured output.
  
  FRONTEND OPTIONS
         --auto-include-path=PATH (absent SOTERIA_AUTO_INCLUDE_PATH env)
             Path to the directory that contains the soteria-c.h
  
         -I DIR
             Add a directory to the include path
  
         --no-c23 (absent SOTERIA_NO_C23 env)
             Disable C23 support (even if the underlying Cerberus library
             supports it).
  
         --no-ignore-duplicate-symbols (absent
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS env)
             Programs that contain duplicate symbols are ignored by default,
             this flag deactivates that behaviour.
  
         --no-ignore-parse-failures (absent SOTERIA_NO_IGNORE_PARSE_FAILURES
         env)
             Files that cannot be parsed correctly are ignored by default, this
             flag deactivates that behaviour.
  
         --parse-only (absent SOTERIA_PARSE_ONLY env)
             Only parse and link the C program, do not perform analysis
  
         --use-cerb-headers (absent SOTERIA_USE_CERB_HEADERS env)
             Use the Cerberus-provided standard headers instead of the system
             headers.
  
         --write-parsed-db=FILE, --write-parsed-compilation-db=FILE (absent
         SOTERIA_WRITE_PARSED_DB env)
             When using a compilation database, write a filtered version
             containing only successfully parsed files to the specified path
  
  ANALYSIS OPTIONS
         --alloc-cannot-fail (absent SOTERIA_ALLOC_CANNOT_FAIL env)
             Assume allocations cannot fail
  
         --cbmc-compat, --cbmc (absent SOTERIA_CBMC_COMPAT env)
             Enable support for a subset of the __CPROVER_ API.
  
         --havoc-undefined-funs, --havoc-undef
             Assume that all undefined functions can return any value. Warning:
             this can lead to unsoundnesses in analyses.
  
         --ignore-ub (absent SOTERIA_IGNORE_UB env)
             Ignores undefined behaviour branches (mostly for Test-Comp's weird
             requirements). Branches reaching UB will be dismissed. Only has
             effect in symbolic testing mode.
  
         --testcomp-compat, --testcomp (absent SOTERIA_TESTCOMP_COMPAT env)
             Enable support for a subset of the testcomp API (e.g.,
             __VERIFIER_nondet_
  
  ARGUMENTS
         FILES (required)
             FILES
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         soteria-c show-ail exits with:
  
         0   on successful analysis (no bugs found).
  
         2   when Soteria-C did not complete the analysis because of a missing
             feature or internal error.
  
         13  when a bug or error was found in the analysed program.
  
  ENVIRONMENT
         These environment variables affect the execution of soteria-c
         show-ail:
  
         HIDE_UNSTABLE
             See option --hide-unstable.
  
         NO_COLOR
             See option --no-color.
  
         SOTERIA_ALLOC_CANNOT_FAIL
             See option --alloc-cannot-fail.
  
         SOTERIA_AUTO_INCLUDE_PATH
             See option --auto-include-path.
  
         SOTERIA_CBMC_COMPAT
             See option --cbmc.
  
         SOTERIA_COMPACT_DIAGNOSTICS
             See option --compact.
  
         SOTERIA_DUMP_REPORT
             See option --dump-report.
  
         SOTERIA_DUMP_SUMMARIES_FILE
             See option --dump-summaries.
  
         SOTERIA_FLAMEGRAPHS
             See option --flamegraphs.
  
         SOTERIA_IGNORE_UB
             See option --ignore-ub.
  
         SOTERIA_NO_C23
             See option --no-c23.
  
         SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS
             See option --no-ignore-duplicate-symbols.
  
         SOTERIA_NO_IGNORE_PARSE_FAILURES
             See option --no-ignore-parse-failures.
  
         SOTERIA_OUTPUT_STATS
             See option --output-stats.
  
         SOTERIA_PARSE_ONLY
             See option --parse-only.
  
         SOTERIA_PRINT_STATES
             See option --print-states.
  
         SOTERIA_SHOW_MANIFEST_SUMMARIES
             See option --show-manifest-summaries.
  
         SOTERIA_SOLVER_TIMEOUT
             See option --solver-timeout.
  
         SOTERIA_TESTCOMP_COMPAT
             See option --testcomp.
  
         SOTERIA_USE_CERB_HEADERS
             See option --use-cerb-headers.
  
         SOTERIA_WRITE_PARSED_DB
             See option --write-parsed-db.
  
         SOTERIA_Z3_PATH
             See option --z3-path.
  
  SEE ALSO
         soteria-c(1)
  
