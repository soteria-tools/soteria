open Vscode

let output_channel = lazy (Window.createOutputChannel ~name:"Soteria-C")

let log_message kind fmt =
  let kind_s =
    match kind with
    | `Warn -> "warn"
    | `Info -> "info"
    | `Error -> "error"
    | `Debug -> "debug"
  in
  let (lazy oc) = output_channel in
  Printf.ksprintf
    (fun s ->
      let value = Printf.sprintf "[EXT: %s]: %s" kind_s s in
      OutputChannel.appendLine oc ~value)
    fmt

let info fmt = log_message `Info fmt
let warn fmt = log_message `Warn fmt
let error fmt = log_message `Error fmt
let debug fmt = log_message `Debug fmt
