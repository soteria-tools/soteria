type t = {
  auto_include_path : string;
      [@default
        match Auto_include_site.Sites.includes with
        | [] -> "."
        | [ x ] -> x
        | _ -> failwith "Multiple auto-include paths found"]
      [@docv "PATH"]
      [@names [ "auto-include-path" ]]
      [@env "SOTERIA_AUTO_INCLUDE_PATH"]
      (** Path to the directory that contains the soteria-c.h *)
  dump_stats_file : string option;
      [@docv "FILE"] [@names [ "dump-stats" ]] [@env "SOTERIA_DUMP_STATS"]
      (** Dump a json file with unsupported features and their number of
          occurences *)
  no_ignore_parse_failures : bool;
      [@make.default false]
      [@names [ "no-ignore-parse-failures" ]]
      [@env "SOTERIA_NO_IGNORE_PARSE_FAILURES"]
      (** Files that cannot be parsed correctly are ignored by default, this
          flag deactivates that behaviour. *)
  no_ignore_duplicate_symbols : bool;
      [@make.default false]
      [@names [ "no-ignore-duplicate-symbols" ]]
      [@env "SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS"]
      (** Programs that contain duplicate symbols are ignored by default, this
          flag deactivates that behaviour. *)
  parse_only : bool;
      [@make.default false]
      [@names [ "parse-only" ]]
      [@env "SOTERIA_PARSE_ONLY"]
      (** Only parse and link the C program, do not perform analysis *)
  dump_summaries_file : string option;
      [@docv "FILE"]
      [@names [ "dump-summaries"; "dump-summaries-to" ]]
      [@env "SOTERIA_DUMP_SUMMARIES_FILE"]
      (** Dump the generated summaries to a file *)
  show_manifest_summaries : bool;
      [@make.default false]
      [@names [ "show-manifest-summaries" ]]
      [@env "SOTERIA_SHOW_MANIFEST_SUMMARIES"]
      (** Print a corresponding manifest summary after the bug report if a bug
          is found *)
  alloc_cannot_fail : bool;
      [@make.default false]
      [@names [ "alloc-cannot-fail" ]]
      [@env "SOTERIA_ALLOC_CANNOT_FAIL"]
      (** Assume allocations cannot fail *)
  use_cerb_headers : bool;
      [@make.default false]
      [@names [ "use-cerb-headers" ]]
      [@env "SOTERIA_USE_CERB_HEADERS"]
      (** Use the Cerberus-provided standard headers instead of the system
          headers. *)
  cbmc_compat : bool;
      [@make.default false]
      [@names [ "cbmc"; "cbmc-compat" ]]
      [@env "SOTERIA_CBMC_COMPAT"]
      (** Enable support for a subset of the __CPROVER_ API. *)
  testcomp_compat : bool;
      [@make.default false]
      [@names [ "testcomp"; "testcomp-compat" ]]
      [@env "SOTERIA_TESTCOMP_COMPAT"]
      (** Enable support for a subset of the testcomp API (e.g.,
          __VERIFIER_nondet_*)
  havoc_undefined_funs : bool;
      [@make.default false]
      [@names
        [ "havoc-undef"; "havoc-undefined-funs" ]
        [@env "SOTERIA_HAVOC_UNDEFINED_FUNS"]]
      (** Assume that all undefined functions can return any value. Warning:
          this can lead to unsoundnesses in analyses. *)
  print_states : bool;
      [@make.default false]
      [@names [ "print-states" ]]
      [@env "SOTERIA_PRINT_STATES"]
      (** Print final program states after whole-program symbolic testing *)
}
[@@deriving make, subliner]

type _ Effect.t += GetConfig : t Effect.t

let default = make ()

let current () =
  try Effect.perform GetConfig with Effect.Unhandled GetConfig -> default

let with_config ~(config : t) f =
  try f () with effect GetConfig, k -> Effect.Deep.continue k config
