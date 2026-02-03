(** Global configuration module *)

(** An object that captures configuration from all submodules of Soteria. *)
type t = {
  logs : Logs.Config.cli; [@term Logs.Config.cmdliner_term ()]
  stats : Stats.Config.t; [@term Stats.Config.cmdliner_term ()]
  terminal : Terminal.Config.t; [@term Terminal.Config.cmdliner_term ()]
  solver : Solvers.Config.t; [@term Solvers.Config.cmdliner_term ()]
}
[@@deriving make, subliner]

let set_and_lock (config : t) =
  Solvers.Config.set_and_lock config.solver;
  Logs.Config.check_set_and_lock config.logs;
  Terminal.Config.set_and_lock config.terminal;
  Stats.Config.set_and_lock config.stats
