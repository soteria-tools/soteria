module Value = Svalue

let log_src = Logs.Src.create "bfa_c.SOLVER"
let log_str s = Logs.debug ~src:log_src (fun m -> m "%s" s)
let solver_log = Simple_smt.{ send = log_str; receive = log_str; stop = Fun.id }
let solver_config = { Simple_smt.z3 with log = solver_log }
let solver = Simple_smt.new_solver solver_config

open Simple_smt

let cmd = ack_command solver
