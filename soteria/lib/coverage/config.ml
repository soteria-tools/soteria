open Soteria_std.Cmdliner_helpers

(* Cmdliner.deriving opens Cmdliner.Arg for the frontend argument, without using
   it. We ignore the warning here. *)
[@@@warning "-unused-open"]

type format = Json | Cobertura | Lcov
[@@deriving subliner_enum, show { with_path = false }]

type t = {
  output_coverage : string option;
      [@docs Sections.output]
      [@names [ "output-coverage"; "dump-coverage"; "coverage" ]]
      [@env "SOTERIA_OUTPUT_COVERAGE"]
      (** If coverage should be output. If the value is "stdout", prints the
          coverage report to stdout; otherwise, stores it in the specified file.
      *)
  coverage_format : (format[@conv format_cmdliner_conv ()]);
      [@docs Sections.output]
      [@default Json]
      [@names [ "coverage-format" ]]
      [@env "SOTERIA_COVERAGE_FORMAT"]
      (** Coverage output format. Options are "json", "cobertura", and "lcov".
      *)
}
[@@deriving make, subliner]

let default = make ~coverage_format:Json ()
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Coverage" ~default ()
