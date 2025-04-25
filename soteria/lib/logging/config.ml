open Level

let logs_enabled = ref false
let enable () = logs_enabled := true
let disable () = logs_enabled := false
let current_log_level = ref Error

let set_log_level level =
  enable ();
  current_log_level := level

let log_file = ref "soteria_logs.html"
let set_log_file file = log_file := file
