(** Hand-written lexer for the [%%svalue] DSL. *)

type token =
  | IDENT of string  (** possibly dotted, e.g. [Z.add] *)
  | HASH_IDENT of string  (** [#x] literal-level variable *)
  | INT of int
  | OCAML of string  (** raw text captured between [{{ ]and[ }}] *)
  | KW of string  (** reserved word *)
  | SYM of string  (** symbolic operator / punctuation *)
  | EOF

type lexeme = { tok : token; pos : int }
(** [pos] is the byte offset of the lexeme in the source string. *)

let keywords =
  [ "ty"; "leaf"; "literal"; "op"; "nop"; "sort"; "with"; "rule"; "when"; "as";
    "print";
    "commutative"; "idempotent"; "involutive"; "identity"; "absorbing";
    "fold"; "true"; "false" ]

(* Multi-char symbols are matched before single-char ones. *)
let symbols =
  [ "~>"; "->"; "<="; ">="; "=="; "!="; "&&"; "||"; "<>";
    "|"; ":"; "="; "("; ")"; ","; "["; "]"; "{"; "}";
    "+"; "-"; "*"; "/"; "%"; "<"; ">"; "!" ]

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_cont c =
  is_ident_start c || (c >= '0' && c <= '9') || c = '\'' || c = '.'

exception Error of int * string

let tokenize (src : string) : lexeme list =
  let n = String.length src in
  let acc = ref [] in
  let i = ref 0 in
  let push pos tok = acc := { tok; pos } :: !acc in
  while !i < n do
    let c = src.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then incr i
    else if c = '/' && !i + 1 < n && src.[!i + 1] = '/' then (
      (* line comment *)
      while !i < n && src.[!i] <> '\n' do
        incr i
      done)
    else if c = '(' && !i + 1 < n && src.[!i + 1] = '*' then (
      (* nested OCaml-style block comment *)
      let depth = ref 1 in
      i := !i + 2;
      while !i < n && !depth > 0 do
        if !i + 1 < n && src.[!i] = '(' && src.[!i + 1] = '*' then (
          incr depth;
          i := !i + 2)
        else if !i + 1 < n && src.[!i] = '*' && src.[!i + 1] = ')' then (
          decr depth;
          i := !i + 2)
        else incr i
      done)
    else if c = '{' && !i + 1 < n && src.[!i + 1] = '{' then (
      (* {{ raw ocaml }} escape, supports nested braces *)
      let start = !i in
      i := !i + 2;
      let buf = Buffer.create 32 in
      let depth = ref 0 in
      let stop = ref false in
      while !i < n && not !stop do
        if !i + 1 < n && src.[!i] = '}' && src.[!i + 1] = '}' && !depth = 0 then (
          i := !i + 2;
          stop := true)
        else (
          (if src.[!i] = '{' then incr depth
           else if src.[!i] = '}' then decr depth);
          Buffer.add_char buf src.[!i];
          incr i)
      done;
      if not !stop then raise (Error (start, "unterminated {{ ... }} block"));
      push start (OCAML (Buffer.contents buf)))
    else if c = '#' then (
      let start = !i in
      incr i;
      let j = ref !i in
      while !j < n && is_ident_cont src.[!j] do
        incr j
      done;
      if !j = !i then raise (Error (start, "expected identifier after '#'"));
      let s = String.sub src !i (!j - !i) in
      i := !j;
      push start (HASH_IDENT s))
    else if is_ident_start c then (
      let start = !i in
      let j = ref !i in
      while !j < n && is_ident_cont src.[!j] do
        incr j
      done;
      let s = String.sub src start (!j - start) in
      i := !j;
      if s = "_" then push start (SYM "_")
      else if List.mem s keywords then push start (KW s)
      else push start (IDENT s))
    else if (c >= '0' && c <= '9') || (c = '-' && !i + 1 < n && src.[!i + 1] >= '0' && src.[!i + 1] <= '9') then (
      let start = !i in
      if c = '-' then incr i;
      while !i < n && src.[!i] >= '0' && src.[!i] <= '9' do
        incr i
      done;
      push start (INT (int_of_string (String.sub src start (!i - start)))))
    else (
      match
        List.find_opt
          (fun s ->
            let l = String.length s in
            !i + l <= n && String.sub src !i l = s)
          symbols
      with
      | Some s ->
          push !i (SYM s);
          i := !i + String.length s
      | None -> raise (Error (!i, Printf.sprintf "unexpected character %C" c)))
  done;
  push n EOF;
  List.rev !acc
