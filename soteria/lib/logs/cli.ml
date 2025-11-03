open Soteria_std
open Level

let log_kind_conv =
  let pp = Config.pp_log_kind in
  let parse s =
    match String.lowercase_ascii s with
    | "sderr" | "raw" | "txt" | "text" -> Ok Config.Stderr
    | "html" -> Ok Config.Html
    | _ -> Error (Format.sprintf "Unknown log kind: %s" s)
  in
  Cmdliner.Arg.conv' ~docv:"LOG_KIND" (parse, pp)

let process_args v_list silent log_kind html always_log_smt =
  let open Syntaxes.Result in
  let* level =
    match (v_list, silent) with
    | [], true -> Ok None
    | _, true -> Error "Cannot use -v and --silent at the same time"
    | v_list, false ->
        let level =
          match List.length v_list with
          | 0 -> Some Warn
          | 1 -> Some Info
          | 2 -> Some Debug
          | 3 -> Some Trace
          | _ -> Some Smt
        in
        Ok level
  in
  let* kind =
    match (log_kind, html) with
    | Some k, false -> Ok k
    | None, true -> Ok Config.Html
    | Some _, true -> Error "Cannot use --html and --log-kind at the same time"
    | None, false -> Ok Config.Stderr
  in
  Ok Config.{ level; kind; always_log_smt }

let term =
  let v_list =
    Cmdliner.Arg.(
      value
      & flag_all
      & info [ "v"; "verbose" ] ~docv:"v"
          ~doc:"Verbosity level, clashes with -q")
  in
  let silent =
    Cmdliner.Arg.(
      value
      & flag
      & info [ "q"; "silent"; "quiet" ] ~docv:""
          ~doc:"Silent mode, clashes with -v")
  in
  let log_kind =
    Cmdliner.Arg.(
      value
      & opt (some log_kind_conv) None
      & info [ "l"; "log_kind" ] ~docv:"LOG_KIND"
          ~doc:"Log kind, clashes with --html")
  in
  let html =
    Cmdliner.Arg.(
      value
      & flag
      & info [ "html" ] ~docv:"" ~doc:"HTML logging, clashes with --log-kind")
  in
  let always_log_smt =
    Cmdliner.Arg.(
      value
      & flag
      & info [ "log-smt" ] ~docv:""
          ~doc:"Always log SMT queries, even in silent mode")
  in
  Cmdliner.Term.(
    const process_args $ v_list $ silent $ log_kind $ html $ always_log_smt)
