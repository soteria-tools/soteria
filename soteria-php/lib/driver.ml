let parse filename =
  match Frontend.parse_file filename with
  | Ok program ->
      program
      |> Php_ir.to_yojson
      |> Yojson.Safe.pretty_to_string
      |> print_endline;
      0
  | Error error ->
      Format.eprintf "Frontend error: %a@." Frontend.Error.pp error;
      2
