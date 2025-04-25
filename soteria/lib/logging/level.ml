type t = Trace | Debug | Info | Warn | App | Error

let to_string = function
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"
  | App -> "APP"

let ( <= ) l r =
  match (l, r) with
  | Trace, Trace -> true
  | Trace, _ -> false
  | Debug, (Trace | Debug) -> true
  | Debug, _ -> false
  | Info, (Trace | Debug | Info) -> true
  | Info, _ -> false
  | Warn, (Trace | Debug | Info | Warn) -> true
  | Warn, _ -> false
  | App, (Trace | Debug | Info | Warn) -> true
  | App, _ -> false
  | Error, _ -> true

let leq = ( <= )

let cmdliner_t =
  let to_level l =
    match List.length l with 0 -> Warn | 1 -> Info | 2 -> Debug | _ -> Trace
  in
  let arg =
    Cmdliner.Arg.(
      value
      & flag_all
      & info [ "v"; "verbose" ] ~docv:"v" ~doc:"Verbosity level")
  in
  Cmdliner.Term.(const to_level $ arg)
