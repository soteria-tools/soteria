type t = {
  solver_timeout : int option;
      [@names [ "solver-timeout" ]] [@env "SOTERIA_SOLVER_TIMEOUT"]
      (** Set the solver timeout in miliseconds*)
  dump_smt_file : string option; [@names [ "dump-smt"; "dump-smt-to" ]]
      (** Dump the SMT queries to the given file*)
  z3_path : string;
      [@default "z3"] [@names [ "z3-path" ]] [@env "SOTERIA_Z3_PATH"]
      (** Path to the Z3 executable *)
}
[@@deriving make, subliner]

let default = make ()
let get, set_and_lock = Soteria_std.Write_once.make ~name:"Solvers" ~default ()
