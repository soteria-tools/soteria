type t = {
  no_compile : bool; [@default false]
  cleanup : bool; [@default false]
  ignore_leaks : bool; [@default false]
  ignore_aliasing : bool; [@default false]
  with_kani : bool; [@default false]
  with_miri : bool; [@default false]
  step_fuel : int; [@default 1000]
  branch_fuel : int; [@default 4]
}
[@@deriving make]

type global = {
  logs : (Soteria_logs.Config.t, string) result;
  terminal : Soteria_terminal.Config.t;
  solver : Soteria_c_values.Solver_config.t;
  rusteria : t;
}
[@@deriving make]

let default = make ()
let current : t ref = ref default
let set (config : t) = current := config
