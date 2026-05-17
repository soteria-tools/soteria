(** Recursive-descent parser for the [%%svalue] DSL. *)

open Svalue_dsl_ast
open Svalue_dsl_lex

type st = {
  mutable toks : lexeme list;
  base : Ppxlib.location;
  src : string;
}

(* All generated/error locations use the payload expression location; the byte
   offset is surfaced as a human-readable line:col in the message. *)
let line_col src pos =
  let line = ref 1 and col = ref 1 in
  for k = 0 to min pos (String.length src) - 1 do
    if src.[k] = '\n' then (
      incr line;
      col := 1)
    else incr col
  done;
  (!line, !col)

let fail st pos fmt =
  let line, col = line_col st.src pos in
  Ppxlib.Location.raise_errorf ~loc:st.base
    ("svalue DSL (line %d, col %d): " ^^ fmt)
    line col

let peek st = match st.toks with l :: _ -> l | [] -> assert false
let loc st = st.base

let advance st =
  match st.toks with
  | l :: tl ->
      st.toks <- tl;
      l
  | [] -> assert false

let expect_sym st s =
  let l = advance st in
  match l.tok with
  | SYM x when x = s -> ()
  | _ -> fail st l.pos "expected '%s'" s

let expect_kw st s =
  let l = advance st in
  match l.tok with
  | KW x when x = s -> ()
  | _ -> fail st l.pos "expected keyword '%s'" s

let eat_sym st s =
  match (peek st).tok with
  | SYM x when x = s ->
      ignore (advance st);
      true
  | _ -> false

let eat_kw st s =
  match (peek st).tok with
  | KW x when x = s ->
      ignore (advance st);
      true
  | _ -> false

let ident st =
  let l = advance st in
  match l.tok with
  | IDENT s -> s
  | _ -> fail st l.pos "expected an identifier"

(* Parse a chunk of raw OCaml (an [{{ ... }}] block) into an expression. *)
let ocaml_expr st text =
  let lb = Lexing.from_string text in
  try Ppxlib.Parse.expression lb
  with _ -> fail st (peek st).pos "could not parse OCaml in {{ ... }}"

(* {2 Rewrite-expression grammar} *)

let rec parse_expr st = parse_or st

and parse_or st =
  let l = parse_and st in
  if eat_sym st "||" then EApp ("||", [], [ l; parse_or st ], loc st) else l

and parse_and st =
  let l = parse_cmp st in
  if eat_sym st "&&" then EApp ("&&", [], [ l; parse_and st ], loc st) else l

and parse_cmp st =
  let l = parse_add st in
  match (peek st).tok with
  | SYM (("==" | "!=" | "<" | "<=" | ">" | ">=") as s) ->
      ignore (advance st);
      let r = parse_add st in
      let lc = loc st in
      (match s with
      | "==" -> EApp ("==", [], [ l; r ], lc)
      | "!=" -> EApp ("not", [], [ EApp ("==", [], [ l; r ], lc) ], lc)
      | ">" -> EApp ("<", [], [ r; l ], lc)
      | ">=" -> EApp ("<=", [], [ r; l ], lc)
      | s -> EApp (s, [], [ l; r ], lc))
  | _ -> l

and parse_add st =
  let rec go l =
    match (peek st).tok with
    | SYM (("+" | "-") as s) ->
        ignore (advance st);
        go (EApp (s, [], [ l; parse_mul st ], loc st))
    | _ -> l
  in
  go (parse_mul st)

and parse_mul st =
  let rec go l =
    match (peek st).tok with
    | SYM (("*" | "/" | "%") as s) ->
        ignore (advance st);
        go (EApp (s, [], [ l; parse_unary st ], loc st))
    | _ -> l
  in
  go (parse_unary st)

and parse_unary st =
  match (peek st).tok with
  | SYM "-" ->
      ignore (advance st);
      EApp ("~-", [], [ parse_unary st ], loc st)
  | SYM "!" ->
      ignore (advance st);
      EApp ("not", [], [ parse_unary st ], loc st)
  | _ -> parse_app st

and parse_app st =
  (* an op/ctor name may be followed by [p1 p2 ...] constructor params *)
  let head = parse_atom st in
  match head with
  | EVar (name, lc) ->
      let params =
        if eat_sym st "[" then (
          let rec ps acc =
            if eat_sym st "]" then List.rev acc
            else ps (parse_atom st :: acc)
          in
          ps [])
        else []
      in
      let rec args acc =
        match (peek st).tok with
        | IDENT _ | HASH_IDENT _ | INT _ | KW ("true" | "false")
        | SYM "(" | SYM "_" | OCAML _ ->
            args (parse_atom st :: acc)
        | _ -> List.rev acc
      in
      let a = args [] in
      if a = [] && params = [] then head else EApp (name, params, a, lc)
  | _ -> head

and parse_atom st =
  let l = advance st in
  match l.tok with
  | SYM "_" -> EWild (loc st)
  | SYM "(" ->
      let e = parse_expr st in
      expect_sym st ")";
      e
  | IDENT s -> EVar (s, loc st)
  | HASH_IDENT s ->
      if eat_sym st ":" then
        let k = ident st in
        ELit (s, Some k, loc st)
      else ELit (s, None, loc st)
  | INT n -> EInt (n, loc st)
  | KW "true" -> EBool (true, loc st)
  | KW "false" -> EBool (false, loc st)
  | OCAML txt -> EOcaml (ocaml_expr st txt)
  | _ -> fail st l.pos "expected an expression"

let rec parse_guard st =
  let g = parse_guard_atom st in
  if eat_sym st "&&" then GAnd (g, parse_guard st) else g

and parse_guard_atom st =
  match (peek st).tok with
  | OCAML txt ->
      ignore (advance st);
      GOcaml (ocaml_expr st txt)
  | _ ->
      let a = parse_add st in
      let l = advance st in
      let cmp =
        match l.tok with
        | SYM "=" | SYM "==" -> `Eq
        | SYM "<>" | SYM "!=" -> `Neq
        | _ -> fail st l.pos "expected '=' or '<>' in guard"
      in
      GCmp (cmp, a, parse_add st)

(* {2 Declarations} *)

let parse_rule st =
  expect_kw st "rule";
  let lhs = parse_expr st in
  expect_sym st "~>";
  let rhs = parse_expr st in
  let guard = if eat_kw st "when" then Some (parse_guard st) else None in
  { r_lhs = lhs; r_rhs = rhs; r_guard = guard; r_loc = loc st }

(* [( cty , cty , ... )] -> core_type list (used by [ty] and [op] ctors) *)
let parse_paren_types st =
  if not (eat_sym st "(") then []
  else
    let rec go acc =
      let start = (peek st).pos in
      let rec stop () =
        match (peek st).tok with
        | SYM ("," | ")") -> (peek st).pos
        | EOF -> fail st start "unterminated type list"
        | _ ->
            ignore (advance st);
            stop ()
      in
      let endp = stop () in
      let txt = String.trim (String.sub st.src start (endp - start)) in
      let cty =
        try Ppxlib.Parse.core_type (Lexing.from_string txt)
        with _ -> fail st start "could not parse type %S" txt
      in
      if eat_sym st "," then go (cty :: acc)
      else (
        expect_sym st ")";
        List.rev (cty :: acc))
    in
    go []

let parse_ty st =
  expect_kw st "ty";
  let sorts = ref [] and args = ref [] in
  let rec go acc =
    let v = ident st in
    let tys = parse_paren_types st in
    if tys <> [] then args := (v, tys) :: !args;
    if eat_kw st "sort" then sorts := (v, ident st) :: !sorts;
    if eat_sym st "|" then go (v :: acc) else List.rev (v :: acc)
  in
  let vs = go [] in
  DTy
    {
      ty_variants = vs;
      ty_args = List.rev !args;
      ty_sorts = List.rev !sorts;
      ty_loc = loc st;
    }

let parse_sort st =
  expect_kw st "sort";
  let name = ident st in
  let base = if eat_sym st ":" then Some (ident st) else None in
  ignore (eat_sym st "=");
  let l = advance st in
  match l.tok with
  | OCAML txt -> (
      try
        DSort
          {
            sort_name = name;
            sort_base = base;
            sort_row = Ppxlib.Parse.core_type (Lexing.from_string txt);
            sort_loc = loc st;
          }
      with _ -> fail st l.pos "could not parse sort row type")
  | _ -> fail st l.pos "'sort' expects a {{ [ `... ] }} row type"

(* Payload OCaml type: capture the raw source span up to the next
   keyword/'as'/'print'/declaration/EOF, then parse it as a [core_type]. *)
let parse_payload_type st =
  let start = (peek st).pos in
  let rec stop () =
    match (peek st).tok with
    | KW ("as" | "print" | "op" | "nop" | "leaf" | "literal" | "ty") | EOF ->
        (peek st).pos
    | _ ->
        ignore (advance st);
        stop ()
  in
  let endp = stop () in
  let txt = String.trim (String.sub st.src start (endp - start)) in
  if txt = "" then fail st start "expected an OCaml payload type";
  try Ppxlib.Parse.core_type (Lexing.from_string txt)
  with _ -> fail st start "could not parse OCaml type %S" txt

let parse_leaf st =
  expect_kw st "leaf";
  let name = ident st in
  expect_sym st ":";
  let ty =
    match (peek st).tok with
    | IDENT "poly" ->
        ignore (advance st);
        None
    | IDENT s ->
        ignore (advance st);
        Some s
    | _ -> fail st (peek st).pos "expected a ty name or 'poly'"
  in
  expect_sym st "=";
  let payload = parse_payload_type st in
  DLeaf { leaf_name = name; leaf_ty = ty; leaf_payload = payload; leaf_loc = loc st }

let parse_literal st =
  expect_kw st "literal";
  let name = ident st in
  expect_sym st ":";
  let ty = ident st in
  expect_sym st "=";
  let payload = parse_payload_type st in
  let ctor = if eat_kw st "as" then Some (ident st) else None in
  let print =
    if eat_kw st "print" then (
      let l = advance st in
      match l.tok with
      | IDENT s -> Some (ocaml_expr st s)
      | OCAML t -> Some (ocaml_expr st t)
      | _ -> fail st l.pos "expected a printer function after 'print'")
    else None
  in
  DLiteral
    {
      lit_name = name;
      lit_ty = ty;
      lit_payload = payload;
      lit_ctor = ctor;
      lit_print = print;
      lit_loc = loc st;
    }

let parse_aux st =
  expect_kw st "with";
  let l = advance st in
  match l.tok with
  | OCAML txt -> (
      try DAux (Ppxlib.Parse.implementation (Lexing.from_string txt))
      with _ -> fail st l.pos "could not parse OCaml in 'with {{ ... }}'")
  | _ -> fail st l.pos "'with' expects a {{ ... }} block"

let parse_nop st =
  expect_kw st "nop";
  let name = ident st in
  let symbol =
    match (peek st).tok with
    | OCAML s ->
        ignore (advance st);
        Some s
    | SYM s when s <> "=" ->
        ignore (advance st);
        Some s
    | _ -> None
  in
  let ctor = if eat_sym st "=" then ident st else String.capitalize_ascii name in
  let body =
    match (peek st).tok with
    | OCAML t ->
        ignore (advance st);
        Some (ocaml_expr st t)
    | _ -> None
  in
  DNop
    {
      nop_name = name;
      nop_ctor = ctor;
      nop_symbol = symbol;
      nop_body = body;
      nop_loc = loc st;
    }

let parse_op st =
  expect_kw st "op";
  let name = ident st in
  expect_sym st ":";
  let rec arrows acc =
    let t = ident st in
    if eat_sym st "->" then arrows (t :: acc) else List.rev (t :: acc)
  in
  let tys = arrows [] in
  let args, ret =
    match List.rev tys with
    | ret :: rargs -> (List.rev rargs, ret)
    | [] -> fail st (peek st).pos "operator needs a type"
  in
  let symbol =
    match (peek st).tok with
    | OCAML s ->
        ignore (advance st);
        Some s
    | SYM s when s <> "=" && s <> "{" ->
        ignore (advance st);
        Some s
    | _ -> None
  in
  let ctor = if eat_sym st "=" then ident st else String.capitalize_ascii name in
  (* optional constructor payload: [= Ctor(name : ty, name : ty, ...)] *)
  let params =
    if eat_sym st "(" then (
      let rec ps acc =
        let pn = ident st in
        expect_sym st ":";
        let start = (peek st).pos in
        let rec stop () =
          match (peek st).tok with
          | SYM ("," | ")") -> (peek st).pos
          | EOF -> fail st start "unterminated constructor payload"
          | _ ->
              ignore (advance st);
              stop ()
        in
        let endp = stop () in
        let txt = String.trim (String.sub st.src start (endp - start)) in
        let cty =
          try Ppxlib.Parse.core_type (Lexing.from_string txt)
          with _ -> fail st start "could not parse param type %S" txt
        in
        let acc = (pn, cty) :: acc in
        if eat_sym st "," then ps acc
        else (
          expect_sym st ")";
          List.rev acc)
      in
      ps [])
    else []
  in
  expect_sym st "{";
  let commutative = ref false
  and idempotent = ref false
  and involutive = ref false
  and identity = ref None
  and absorbing = ref None
  and fold = ref None
  and rules = ref [] in
  let rec body () =
    match (peek st).tok with
    | SYM "}" -> ignore (advance st)
    | KW "commutative" ->
        ignore (advance st);
        commutative := true;
        ignore (eat_sym st ";");
        body ()
    | KW "idempotent" ->
        ignore (advance st);
        idempotent := true;
        ignore (eat_sym st ";");
        body ()
    | KW "involutive" ->
        ignore (advance st);
        involutive := true;
        ignore (eat_sym st ";");
        body ()
    | KW "identity" ->
        ignore (advance st);
        (match (advance st).tok with
        | INT n -> identity := Some n
        | _ -> fail st (peek st).pos "identity expects an int literal");
        ignore (eat_sym st ";");
        body ()
    | KW "absorbing" ->
        ignore (advance st);
        (match (advance st).tok with
        | INT n -> absorbing := Some n
        | _ -> fail st (peek st).pos "absorbing expects an int literal");
        ignore (eat_sym st ";");
        body ()
    | KW "fold" ->
        ignore (advance st);
        (match (advance st).tok with
        | IDENT s -> fold := Some (ocaml_expr st s)
        | OCAML t -> fold := Some (ocaml_expr st t)
        | _ -> fail st (peek st).pos "fold expects a function");
        ignore (eat_sym st ";");
        body ()
    | KW "rule" ->
        rules := parse_rule st :: !rules;
        ignore (eat_sym st ";");
        body ()
    | _ -> fail st (peek st).pos "expected a property, 'rule' or '}'"
  in
  body ();
  DOp
    {
      op_name = name;
      op_args = args;
      op_ret = ret;
      op_symbol = symbol;
      op_ctor = ctor;
      op_params = params;
      op_commutative = !commutative;
      op_idempotent = !idempotent;
      op_involutive = !involutive;
      op_identity = !identity;
      op_absorbing = !absorbing;
      op_fold = !fold;
      op_rules = List.rev !rules;
      op_loc = loc st;
    }

let parse_program ~(base : Ppxlib.location) (src : string) : program =
  let st =
    try { toks = Svalue_dsl_lex.tokenize src; base; src }
    with Svalue_dsl_lex.Error (pos, msg) ->
      let line, col = line_col src pos in
      Ppxlib.Location.raise_errorf ~loc:base
        "svalue DSL (line %d, col %d): %s" line col msg
  in
  let rec go acc =
    match (peek st).tok with
    | EOF -> List.rev acc
    | KW "ty" -> go (parse_ty st :: acc)
    | KW "leaf" -> go (parse_leaf st :: acc)
    | KW "literal" -> go (parse_literal st :: acc)
    | KW "op" -> go (parse_op st :: acc)
    | KW "nop" -> go (parse_nop st :: acc)
    | KW "with" -> go (parse_aux st :: acc)
    | KW "sort" -> go (parse_sort st :: acc)
    | _ -> fail st (peek st).pos "expected a top-level declaration"
  in
  go []
