open Soteria_linear_lib

let test_parse_file name =
  let file = Parser.parse_file name in
  Fmt.pr "Parsed AST from %s:@.%a@." name Lang.Program.pp file

let () =
  let examples_dir = "soteria-linear/test/examples" in
  let files = Sys.readdir examples_dir in
  Array.sort String.compare files;
  Array.iter
    (fun file ->
      if Filename.check_suffix file ".lin" then
        test_parse_file (Filename.concat examples_dir file))
    files
