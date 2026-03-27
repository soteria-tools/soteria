type t = {
  output_coverage : string option;
      [@docs Soteria_std.Cmdliner_helpers.Sections.output]
      [@names [ "output-coverage"; "dump-coverage"; "coverage" ]]
      [@env "SOTERIA_OUTPUT_COVERAGE"]
      (** If coverage should be output. If the value is ["stdout"], prints the
          coverage report to stdout; otherwise, stores it in the specified file.
      *)
  coverage_format : string;
      [@docs Soteria_std.Cmdliner_helpers.Sections.output]
      [@names [ "coverage-format" ]]
      [@env "SOTERIA_COVERAGE_FORMAT"]
      (** Coverage output format. Built-in values are ["json"] and
          ["cobertura"]. *)
}
[@@deriving make, subliner]

let default = make ~coverage_format:"json" ()
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Coverage" ~default ()
