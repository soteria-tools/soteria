type t = {
  no_compile : bool; [@make.default false] [@names [ "no-compile" ]]
      (** Do not compile the Rust code, as it is already compiled *)
  no_timing : bool; [@make.default false] [@names [ "no-timing" ]]
      (** Do not display execution times *)
  cleanup : bool; [@make.default false] [@names [ "clean" ]]
      (** Clean up compiles files after execution *)
  ignore_leaks : bool; [@make.default false] [@names [ "ignore-leaks" ]]
      (** Ignore memory leaks *)
  ignore_aliasing : bool; [@make.default false] [@names [ "ignore-aliasing" ]]
      (** Ignore pointer aliasing rules (tree borrows) *)
  monomorphize_experimental : bool;
      [@make.default false] [@names [ "monomorphize-experimental" ]]
      (** Use Charon's new monomorphization, which may cause unexpected results
          but resolves drops. *)
  with_kani : bool; [@make.default false] [@names [ "kani" ]]
      (** Use the Kani library *)
  with_miri : bool; [@make.default false] [@names [ "miri" ]]
      (** Use the Miri library *)
  with_obol : bool; [@make.default false] [@names [ "obol" ]]
      (** Compile the code using Obol, rather than Charon *)
  log_compilation : bool; [@make.default false] [@names [ "log-compilation" ]]
      (** Log the compilation process *)
  step_fuel : int; [@default 1000] [@names [ "step-fuel" ]] [@env "STEP_FUEL"]
      (** The default step fuel for each entrypoint -- every control flow jump
          counts as one fuel *)
  branch_fuel : int;
      [@default 4] [@names [ "branch-fuel" ]] [@env "BRANCH_FUEL"]
      (** The default branch fuel for each entrypoint -- every symbolic
          execution branching point counts as one fuel *)
  no_fuel : bool; [@make.default false] [@names [ "no-fuel" ]]
      (** Disable fuel, for unbounded symbolic execution *)
  rustc_flags : string list;
      [@default []] [@names [ "rustc" ]] [@env "RUSTC_FLAGS"]
      (** Additional flags to pass to the Rustc compiler *)
  filter : string list; [@default []] [@names [ "filter" ]]
      (** Filter the entrypoints to run, by name. If empty, all entrypoints are
          run. Multiple filters can be provided; tests matching any will be
          selected. The filters are treated as regexes. *)
  print_summary : bool; [@make.default false] [@names [ "summary" ]]
      (** If a summary of all test cases should be printed at the end of
          execution *)
}
[@@deriving make, subliner]

let term = cmdliner_term ()

type global = {
  logs : (Soteria.Logs.Config.t, string) result; [@term Soteria.Logs.Cli.term]
  terminal : Soteria.Terminal.Config.t;
      [@term Soteria.Terminal.Config.cmdliner_term ()]
  solver : Soteria.Solvers.Config.t;
      [@term Soteria.Solvers.Config.cmdliner_term ()]
  rusteria : t; [@term term]
}
[@@deriving make, subliner]

let global_term = global_cmdliner_term ()
let default = make ()
let current : t ref = ref default

let set (config : global) =
  Soteria.Solvers.Config.set config.solver;
  Soteria.Logs.Config.check_set_and_lock config.logs;
  Soteria.Terminal.Config.set_and_lock config.terminal;
  current := config.rusteria
