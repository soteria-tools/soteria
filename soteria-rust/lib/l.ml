include Soteria.Logging.Logs.L

let entry_point_section name =
  Fmt.kstr
    (Soteria.Logging.Logs.with_section ~is_branch:false)
    "Executing entry point: %a" Crate.pp_name name
