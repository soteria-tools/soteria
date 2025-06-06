let process_args no_color compact : Config.t = { no_color; compact }

let term =
  let no_color =
    let doc = "Disable colored output" in
    let env = Cmdliner.Cmd.Env.info ~doc "NO_COLOR" in
    Cmdliner.Arg.(value & flag & info [ "no-color" ] ~docv:"" ~doc ~env)
  in
  let compact =
    Cmdliner.Arg.(
      value
      & flag
      & info [ "compact" ] ~docv:"" ~doc:"Compact diagnostic output")
  in
  Cmdliner.Term.(const process_args $ no_color $ compact)
