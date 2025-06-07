let process_args no_color compact : Config.t = { no_color; compact }

let term =
  let no_color =
    let doc = "Disable colored output" in
    let env = Cmdliner.Cmd.Env.info ~doc "NO_COLOR" in
    Cmdliner.Arg.(value & flag & info [ "no-color" ] ~doc ~env)
  in
  let compact =
    let doc = "Compact diagnostic output" in
    let env = Cmdliner.Cmd.Env.info ~doc "SOTERIA_COMPACT_DIAGNOSTICS" in
    Cmdliner.Arg.(value & flag & info [ "compact" ] ~doc ~env)
  in
  Cmdliner.Term.(const process_args $ no_color $ compact)
