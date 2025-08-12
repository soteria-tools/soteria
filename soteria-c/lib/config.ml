type t = {
  auto_include_path : string;
      [@default
        match Auto_include_site.Sites.includes with
        | [] -> "."
        | [ x ] -> x
        | _ -> failwith "Multiple auto-include paths found"]
      [@docv "PATH"]
      [@names [ "auto-include-path" ] [@env "SOTERIA_AUTO_INCLUDE_PATH"]]
      (** Path to the directory that contains the soteria-c.h *)
  dump_unsupported_file : string option;
      [@docv "FILE"]
      [@names [ "dump-unsupported" ] [@env "SOTERIA_DUMP_UNSUPPORTED"]]
      (** Dump a json file with unsupported features and their number of
          occurences *)
  no_ignore_parse_failures : bool;
      [@make.default false]
      [@names
        [ "no-ignore-parse-failures" ] [@env "SOTERIA_NO_IGNORE_PARSE_FAILURES"]]
      (** Files that cannot be parsed correctly are ignored by default, this
          flag deactivates that behaviour. *)
  no_ignore_duplicate_symbols : bool;
      [@make.default false]
      [@names
        [ "no-ignore-duplicate-symbols" ]
        [@env "SOTERIA_NO_IGNORE_DUPLICATE_SYMBOLS"]]
      (** Programs that contain duplicate symbols are ignored by default, this
          flag deactivates that behaviour. *)
  parse_only : bool;
      [@make.default false]
      [@names [ "parse-only" ] [@env "SOTERIA_PARSE_ONLY"]]
      (** Only parse and link the C program, do not perform analysis *)
  dump_summaries_file : string option;
      [@docv "FILE"]
      [@names
        [ "dump-summaries"; "dump-summaries-to" ]
        [@env "SOTERIA_DUMP_SUMMARIES_FILE"]]
      (** Dump the generated summaries to a file *)
  show_manifest_summaries : bool;
      [@make.default false]
      [@names
        [ "show-manifest-summaries" ] [@env "SOTERIA_SHOW_MANIFEST_SUMMARIES"]]
      (** Print a corresponding manifest summary after the bug report if a bug
          is found *)
  alloc_cannot_fail : bool;
      [@make.default false]
      [@names [ "alloc-cannot-fail" ] [@env "SOTERIA_ALLOC_CANNOT_FAIL"]]
      (** Assume allocations cannot fail *)
  use_cerb_headers : bool;
      [@make.default false]
      [@names [ "use-cerb-headers" ] [@env "SOTERIA_USE_CERB_HEADERS"]]
      (** Use the Cerberus-provided standard headers instead of the system
          headers. *)
  infinite_fuel : bool;
      [@make.default false]
      [@names [ "infinite-fuel" ] [@env "SOTERIA_INFINITE_FUEL"]]
      (** Infinite fuel for the analysis. If there is an unbounded loop, the
          analysis will not stop. (Used only for whole-program analysis) *)
  cbmc_compat : bool;
      [@make.default false]
      [@names [ "cbmc"; "cbmc-compat" ] [@env "SOTERIA_CBMC_COMPAT"]]
      (** Enable support for a subset of the __CPROVER_ API. *)
  havoc_undefined_funs : bool;
      [@make.default false]
      [@names
        [ "havoc-undef"; "havoc-undefined-funs" ]
        [@env "SOTERIA_HAVOC_UNDEFINED_FUNS"]]
      (** Assume that all undefined functions can return any value. Warning:
          this can lead to unsoundnesses in analyses. *)
}
[@@deriving make, subliner]

type _ Effect.t += GetConfig : t Effect.t

let default = make ()

let current () =
  try Effect.perform GetConfig with Effect.Unhandled GetConfig -> default

let with_config ~(config : t) f =
  try f () with effect GetConfig, k -> Effect.Deep.continue k config
