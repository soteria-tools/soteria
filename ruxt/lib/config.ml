(* Cmdliner.deriving opens Cmdliner.Arg for the frontend argument, without using it.
   We ignore the warning here. *)
[@@@warning "-unused-open"]

type t = {
  (*
     Compilation flags
   *)
  cleanup : bool; [@make.default false] [@names [ "clean" ]]
      (** Clean up compiled files after execution *)
  log_compilation : bool; [@make.default false] [@names [ "log-compilation" ]]
      (** Log the compilation process *)
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
      (** Choose the frontend to use: Charon or Obol *)
  sysroot : string option; [@names [ "sysroot" ]] [@env "RUST_SYSROOT"]
      (** The sysroot to use for compilation. If not provided, the default
          sysroot is used. *)
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
  only_public : bool; [@make.default false] [@names [ "only-public" ]]
      (** Only include public functions in the analysis *)
  provenance :
    (Soteria_rust_lib.Config.provenance
    [@conv Soteria_rust_lib.Config.provenance_cmdliner_conv ()]);
      [@default Soteria_rust_lib.Config.Permissive] [@names [ "provenance" ]]
      (** The provenance model to use for pointers. If not provided, the default
          is permissive. *)
  step_fuel : int option; [@names [ "step-fuel" ]] [@env "STEP_FUEL"]
      (** The default step fuel for each entrypoint -- every control flow jump
          counts as one fuel. Defaults to infinite fuel. *)
  branch_fuel : int option; [@names [ "branch-fuel" ]] [@env "BRANCH_FUEL"]
      (** The default branch fuel for each entrypoint -- every symbolic
          execution branching point counts as one fuel. Defaults to infinite
          fuel. *)
  pass_fuel : int; [@default 5] [@names [ "pass-fuel" ]] [@env "PASS_FUEL"]
      (** The default pass fuel for each library -- calling every function of
          the library with the current summaries counts as one fuel *)
}
[@@deriving make, subliner]

let term = cmdliner_term ()

type global = {
  logs : (Soteria.Logs.Config.t, string) result; [@term Soteria.Logs.Cli.term]
  terminal : Soteria.Terminal.Config.t;
      [@term Soteria.Terminal.Config.cmdliner_term ()]
  solver : Soteria.Solvers.Config.t;
      [@term Soteria.Solvers.Config.cmdliner_term ()]
  ruxt : t; [@term term]
}
[@@deriving make, subliner]

let global_term = global_cmdliner_term ()
let default = make ()
let current : t ref = ref default

let set (config : global) =
  current := config.ruxt;
  let (config : Soteria_rust_lib.Config.global) =
    {
      logs = config.logs;
      terminal = config.terminal;
      solver = config.solver;
      rusteria =
        {
          cleanup = config.ruxt.cleanup;
          log_compilation = config.ruxt.log_compilation;
          no_compile = config.ruxt.no_compile;
          no_compile_plugins = config.ruxt.no_compile_plugins;
          target = config.ruxt.target;
          output_crate = config.ruxt.output_crate;
          rustc_flags = config.ruxt.rustc_flags;
          (* Default to Charon for compositionality *)
          frontend = Soteria_rust_lib.Config.Charon;
          sysroot = config.ruxt.sysroot;
          with_kani = config.ruxt.with_kani;
          with_miri = config.ruxt.with_miri;
          (* No entry points needed as RUXt is compositional *)
          filter = [];
          no_timing = config.ruxt.no_timing;
          print_summary = config.ruxt.print_summary;
          print_stats = config.ruxt.print_stats;
          (* Ignore leaks in Soteria, we implement our own leak check *)
          ignore_leaks = true;
          (* Tree borrows are not supported *)
          ignore_aliasing = true;
          provenance = config.ruxt.provenance;
          step_fuel = config.ruxt.step_fuel;
          branch_fuel = config.ruxt.branch_fuel;
        };
    }
  in
  Soteria_rust_lib.Config.set config
