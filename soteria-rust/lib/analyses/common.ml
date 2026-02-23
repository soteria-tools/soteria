open Frontend.Diagnostic

let fatal ?name ?(code = 2) err =
  let msg = Option.fold ~none:"Fatal: " ~some:(Fmt.str "Fatal (%s): ") name in
  print_diagnostic_simple ~severity:Error (msg ^ err);
  exit code
