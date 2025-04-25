include Soteria_logs.Logs.L

let entry_point_section name =
  Fmt.kstr
    (Soteria_logs.Logs.with_section ~is_branch:false)
    "Executing entry point: %s" name
