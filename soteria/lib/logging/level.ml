type t = Trace | Debug | Info | Warn | App | Error | Smt

let to_string = function
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"
  | App -> "APP"
  | Smt -> "SMT"

let ( >= ) l r =
  match (l, r) with
  | _, _ when l = r -> true
  | Trace, _ -> false
  | _, Trace -> true
  | Smt, _ -> false
  | _, Smt -> true
  | Debug, _ -> false
  | _, Debug -> true
  | Info, _ -> false
  | _, Info -> true
  | Warn, _ -> false
  | _, Warn -> true
  | App, _ -> false
  | _, App -> true
  | Error, _ -> true

let geq = ( >= )

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
