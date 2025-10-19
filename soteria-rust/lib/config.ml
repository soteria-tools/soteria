type t = {
  (*
     Compilation flags
   *)
  cleanup : bool; [@make.default false] [@names [ "clean" ]]
      (** Clean up compiled files after execution *)
  log_compilation : bool; [@make.default false] [@names [ "log-compilation" ]]
      (** Log the compilation process *)
  monomorphize_old : bool; [@make.default false] [@names [ "monomorphize-old" ]]
      (** Use Charon's old monomorphization, which may be slower and less
          powerful. *)
  no_compile : bool; [@make.default false] [@names [ "no-compile" ]]
      (** Do not compile the Rust code, as it is already compiled *)
  no_compile_plugins : bool;
      [@make.default false] [@names [ "no-compile-plugins" ]]
      (** Do not compile the plugins, as they are already compiled *)
  target : string option; [@names [ "target" ]] [@env "TARGET"]
      (** The compilation target triple to use, e.g. x86_64-unknown-linux-gnu.
          If not provided, the default target for the current machine is used.
      *)
  output_crate : bool; [@make.default false] [@names [ "output-crate" ]]
      (** Pretty-print the compiled crate to a file *)
  rustc_flags : string list;
      [@default []] [@names [ "rustc" ]] [@env "RUSTC_FLAGS"]
      (** Additional flags to pass to the Rustc compiler *)
  with_obol : bool; [@make.default false] [@names [ "obol" ]]
      (** Compile the code using Obol, rather than Charon *)
  (*
     Plugins
   *)
  with_kani : bool; [@make.default false] [@names [ "kani" ]]
      (** Use the Kani library *)
  with_miri : bool; [@make.default false] [@names [ "miri" ]]
      (** Use the Miri library *)
  (*
     Printing settings
   *)
  filter : string list; [@default []] [@names [ "filter" ]]
      (** Filter the entrypoints to run, by name. If empty, all entrypoints are
          run. Multiple filters can be provided; tests matching any will be
          selected. The filters are treated as regexes. *)
  no_timing : bool; [@make.default false] [@names [ "no-timing" ]]
      (** Do not display execution times *)
  print_summary : bool; [@make.default false] [@names [ "summary" ]]
      (** If a summary of all test cases should be printed at the end of
          execution *)
  print_stats : bool; [@make.default false] [@names [ "stats" ]]
      (** If statistics about the execution should be printed at the end of each
          test *)
  (*
     Symbolic execution behaviour
   *)
  ignore_leaks : bool; [@make.default false] [@names [ "ignore-leaks" ]]
      (** Ignore memory leaks *)
  ignore_aliasing : bool; [@make.default false] [@names [ "ignore-aliasing" ]]
      (** Ignore pointer aliasing rules (tree borrows) *)
  step_fuel : int option; [@names [ "step-fuel" ]] [@env "STEP_FUEL"]
      (** The default step fuel for each entrypoint -- every control flow jump
          counts as one fuel. Defaults to infinite fuel. *)
  branch_fuel : int option; [@names [ "branch-fuel" ]] [@env "BRANCH_FUEL"]
      (** The default branch fuel for each entrypoint -- every symbolic
          execution branching point counts as one fuel. Defaults to infinite
          fuel. *)
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
