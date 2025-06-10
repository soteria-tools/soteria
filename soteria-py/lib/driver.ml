let parse_py filename =
  let open PyreAst.Parser in
  let file = Channels.read_file filename in
  let res = with_context (fun context -> Concrete.parse_module ~context file) in
  match res with
  | Ok ast ->
      let sexp = PyreAst.Concrete.Module.sexp_of_t ast in
      let text = Sexplib.Sexp.to_string_hum sexp in
      Fmt.pr "Successfully parsed %s:\n%s\n" filename text
  | Error err -> Fmt.epr "Error parsing %s: %s\n" filename err.message
