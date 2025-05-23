type t = {
  solver_timeout : int option; [@default None]
  dump_smt_file : string option; [@default None]
  z3_path : string; [@default "z3"]
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config

module Cli = struct
  open Cmdliner
  open Soteria_std.Cmdliner_helpers

  let dump_smt_arg =
    let doc = "Dump the SMT queries to the given file" in
    Arg.(
      value
      & opt (some string) default.dump_smt_file
      & info [ "dump-smt-to"; "dump-smt" ] ~docv:"SMT_FILE" ~doc)

  let solver_timeout_arg =
    let doc = "Set the solver timeout in miliseconds" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_SOLVER_TIMEOUT" in
    Arg.(
      value
      & opt (some int) default.solver_timeout
      & info [ "solver-timeout" ] ~doc ~docv:"TIMEOUT" ~env)

  let z3_path_arg =
    let doc = "Path to the Z3 executable" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_Z3_PATH" in
    Arg.(
      value
      & opt file_as_absolute default.z3_path
      & info [ "z3-path" ] ~env ~doc)

  let make_from_args z3_path solver_timeout dump_smt_file =
    make ~z3_path ~solver_timeout ~dump_smt_file ()

  let term =
    Cmdliner.Term.(
      const make_from_args $ z3_path_arg $ solver_timeout_arg $ dump_smt_arg)
end
