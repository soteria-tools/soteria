type t = {
  solver_timeout : int option;
      [@names [ "solver-timeout" ]] [@env "SOTERIA_SOLVER_TIMEOUT"]
      (** Set the solver timeout in miliseconds*)
  dump_smt_file : string option; [@names [ "dump-smt"; "dump-smt-to" ]]
      (** Dump the SMT queries to the given file*)
  z3_path : string;
      [@default "z3"] [@names [ "z3-path" ]] [@env "SOTERIA_Z3_PATH"]
      (** Path to the Z3 executable *)
  hide_response_times : bool;
      [@make.default false]
      [@names [ "hide-response-times" ]]
      [@env "SOTERIA_HIDE_RESPONSE_TIMES"]
      (** Hide the response times in the solver response logs, useful for
          reproducible output *)
}
[@@deriving make, subliner]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
