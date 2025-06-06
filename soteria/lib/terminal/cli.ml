let process_args no_color compact : Config.t = { no_color; compact }

let term =
  let no_color =
    Cmdliner.Arg.(
      value & flag & info [ "no-color" ] ~docv:"" ~doc:"Disable colored output")
  in
  let compact =
    Cmdliner.Arg.(
      value
      & flag
      & info [ "compact" ] ~docv:"" ~doc:"Compact diagnostic output")
  in
  Cmdliner.Term.(const process_args $ no_color $ compact)
