type t = {
  solver_timeout : int option;
      [@names [ "solver-timeout" ]] [@env "SOTERIA_SOLVER_TIMEOUT"]
      (** Set the solver timeout in miliseconds*)
  dump_smt_file : string option;
  z3_path : string; [@default "z3"]
  hide_response_times : bool; [@make.default false]
}
[@@deriving make, subliner]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config

(* module Cli = struct
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

  let hide_response_times_arg =
    let doc =
      "Hide the response times in the solver response logs, useful for \
       reproducible output"
    in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_HIDE_RESPONSE_TIMES" in
    Arg.(value & flag & info [ "hide-response-times" ] ~env ~doc)

  let make_from_args z3_path solver_timeout dump_smt_file hide_response_times =
    make ~z3_path ~solver_timeout ~dump_smt_file ~hide_response_times ()

  let term =
    Cmdliner.Term.(
      const make_from_args
      $ z3_path_arg
      $ solver_timeout_arg
      $ dump_smt_arg
      $ hide_response_times_arg)
end *)
