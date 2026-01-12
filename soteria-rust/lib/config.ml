(* Cmdliner.deriving opens Cmdliner.Arg for the frontend argument, without using it.
   We ignore the warning here. *)
[@@@warning "-unused-open"]

type frontend = Charon | Obol
[@@deriving subliner_enum, show { with_path = false }]

type provenance = Strict | Permissive [@@deriving subliner_enum]

type t = {
  (*
     Compilation flags
   *)
  cleanup : bool;
      [@make.default false] [@names [ "clean" ]] [@env "SOTERIA_RUST_CLEANUP"]
      (** Clean up compiled files after execution *)
  log_compilation : bool; [@make.default false] [@names [ "log-compilation" ]]
      (** Log the compilation process *)
  no_compile : bool; [@make.default false] [@names [ "no-compile" ]]
      (** Do not compile the Rust code, as it is already compiled *)
  no_compile_plugins : bool;
      [@make.default false] [@names [ "no-compile-plugins" ]]
      (** Do not compile the plugins, as they are already compiled *)
  plugin_directory : string option;
      [@names [ "plugins" ]] [@env "SOTERIA_RUST_PLUGINS"]
      (** The directory in which plugins are and should be compiled; defaults to
          the current dune-managed site. *)
  target : string option; [@names [ "target" ]] [@env "TARGET"]
      (** The compilation target triple to use, e.g. x86_64-unknown-linux-gnu.
          If not provided, the default target for the current machine is used.
      *)
  output_crate : bool; [@make.default false] [@names [ "output-crate" ]]
      (** Pretty-print the compiled crate to a file *)
  rustc_flags : string list;
      [@default []] [@names [ "rustc" ]] [@env "RUSTC_FLAGS"]
      (** Additional flags to pass to the Rustc compiler *)
  frontend : (frontend[@conv frontend_cmdliner_conv ()]);
      [@default Obol] [@make.default Obol] [@names [ "frontend" ]]
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
  filter : string list; [@default []] [@names [ "filter" ]]
      (** Filter the entrypoints to run, by name. If empty, all entrypoints are
          run. Multiple filters can be provided; tests matching any will be
          selected. The filters are treated as regexes. *)
  print_summary : bool; [@make.default false] [@names [ "summary" ]]
      (** If a summary of all test cases should be printed at the end of
          execution *)
  (*
     Symbolic execution behaviour
   *)
  ignore_leaks : bool; [@make.default false] [@names [ "ignore-leaks" ]]
      (** Ignore memory leaks *)
  ignore_aliasing : bool; [@make.default false] [@names [ "ignore-aliasing" ]]
      (** Ignore pointer aliasing rules (tree borrows) *)
  provenance : (provenance[@conv provenance_cmdliner_conv ()]);
      [@default Permissive] [@names [ "provenance" ]]
      (** The provenance model to use for pointers. If not provided, the default
          is permissive. *)
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
let default = make ()

let get, set_and_lock =
  Soteria.Soteria_std.Write_once.make ~name:"Soteria-Rust" ~default ()

type global = {
  logs : (Soteria.Logs.Config.t, string) result; [@term Soteria.Logs.Cli.term]
  terminal : Soteria.Terminal.Config.t;
      [@term Soteria.Terminal.Config.cmdliner_term ()]
  solver : Soteria.Solvers.Config.t;
      [@term Soteria.Solvers.Config.cmdliner_term ()]
  stats : Soteria.Stats.Config.t; [@term Soteria.Stats.Config.cmdliner_term ()]
  soteria_rust : t; [@term term]
}
[@@deriving make, subliner]

let global_term = global_cmdliner_term ()

let set_and_lock_global (config : global) =
  Soteria.Solvers.Config.set_and_lock config.solver;
  Soteria.Logs.Config.check_set_and_lock config.logs;
  Soteria.Terminal.Config.set_and_lock config.terminal;
  Soteria.Stats.Config.set_and_lock config.stats;
  set_and_lock config.soteria_rust
