let parse_program s =
  let lexbuf = Lexing.from_string s in
  try Parser.program Lexer.read lexbuf with
  | Parser.Error -> failwith ("Parser error (program): " ^ s)
  | Failure msg -> failwith ("Failure: " ^ msg ^ " in " ^ s)

let parse_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  let lexbuf = Lexing.from_string s in
  try Parser.program Lexer.read lexbuf with
  | Parser.Error -> Fmt.failwith "Parser error in %s\n" filename
  | Failure msg -> Fmt.failwith "Failure in %s: %s\n" filename msg

(* let parse_assertion s =
  let lexbuf = Lexing.from_string s in
  try Parser.assertion_eof Lexer.read lexbuf with
  | Parser.Error -> failwith ("Parser error (assertion): " ^ s)
  | Failure msg -> failwith ("Failure: " ^ msg ^ " in " ^ s) *)
