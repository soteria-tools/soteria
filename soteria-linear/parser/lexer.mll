{
open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = ['0'-'9']+

rule read = parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  (* | "take" { TAKE } *)
  (* | "requires" { REQUIRES } *)
  (* | "ensures" { ENSURES } *)
  (* | "RW" { RW } *)
  (* | "Freed" { FREED } *)
  | "true" { TRUE }
  | "false" { FALSE }
  | "nondet" { NONDET }
  | "alloc" { ALLOC }
  | "free" { FREE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "&&" { AND }
  | "||" { OR }
  | "==" { EQEQ }
  | "=" { EQ }
  | "<" { LT }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "<-" { ASSIGN }
  | ";" { SEMI }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (Failure ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
