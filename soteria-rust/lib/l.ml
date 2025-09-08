include Soteria.Logs.L

let entry_point_section name =
  Fmt.kstr
    (with_section ~is_branch:false)
    "Executing entry point: %a" Crate.pp_name name
