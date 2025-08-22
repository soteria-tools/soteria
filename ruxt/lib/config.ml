type t = {
  no_compile : bool; [@make.default false] [@names [ "no-compile" ]]
      (** Do not compile the Rust code, as it is already compiled *)
  no_timing : bool; [@make.default false] [@names [ "no-timing" ]]
      (** Do not display execution times *)
  cleanup : bool; [@make.default false] [@names [ "clean" ]]
      (** Clean up compiles files after execution *)
  ignore_leaks : bool; [@make.default false] [@names [ "ignore-leaks" ]]
      (** Ignore memory leaks *)
  only_public : bool; [@make.default false] [@names [ "only-public" ]]
      (** Only include public functions in the analysis *)
  with_kani : bool; [@make.default false] [@names [ "kani" ]]
      (** Use the Kani library *)
  with_miri : bool; [@make.default false] [@names [ "miri" ]]
      (** Use the Miri library *)
  log_compilation : bool; [@make.default false] [@names [ "log-compilation" ]]
      (** Log the compilation process *)
  step_fuel : int; [@default 1000] [@names [ "step-fuel" ]] [@env "STEP_FUEL"]
      (** The default step fuel for each entrypoint -- every control flow jump
          counts as one fuel *)
  branch_fuel : int;
      [@default 4] [@names [ "branch-fuel" ]] [@env "BRANCH_FUEL"]
      (** The default branch fuel for each entrypoint -- every symbolic
          execution branching point counts as one fuel *)
  pass_fuel : int; [@default 5] [@names [ "pass-fuel" ]] [@env "PASS_FUEL"]
      (** The default pass fuel for each library -- calling every function of
          the library with the current summaries counts as one fuel *)
}
[@@deriving make, subliner]

type global = {
  logs : (Soteria_logs.Config.t, string) result;
  terminal : Soteria_terminal.Config.t;
  solver : Soteria_c_values.Solver_config.t;
  ruxt : t;
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
