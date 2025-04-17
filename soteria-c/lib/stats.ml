type stats = { mutable executed_statements : int }

let stats = { executed_statements = 0 }

let incr_executed_statements () =
  stats.executed_statements <- stats.executed_statements + 1

let get_executed_statements () = stats.executed_statements
