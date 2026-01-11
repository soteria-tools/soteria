%{
  open Soteria_linear_ast
  open Lang
%}

%token <int> INT
%token <string> ID
%token PLUS MINUS TIMES DIV AND OR EQ EQEQ LT
%token LBRACK RBRACK ASSIGN SEMI LPAREN RPAREN COMMA
%token LET IN IF THEN ELSE
// %token TAKE RW FREED
%token TRUE FALSE NONDET ALLOC FREE
// %token REQUIRES ENSURES
%token EOF

%start <Program.t> program
// %start <Asrt.t> assertion_eof

%nonassoc IN
%right SEMI
%nonassoc ELSE
%left OR
%left AND
%left EQEQ LT
%left PLUS MINUS
%left TIMES DIV

%%

program:
  | fns = list(fun_def) EOF {
      List.fold_left (fun acc f -> String_map.add f.Fun_def.name f acc) String_map.empty fns
    }

fun_def:
  | LET
    name = ID
    args = list(ID)
    // spec = option(spec)
    EQ
    body = expr {
      {
        Fun_def.name;
        args;
        body;
        (* spec *)
      }
    }

// spec:
//   | REQUIRES pre = assertion ENSURES post = assertion { { Spec.pre; post } }
//   | REQUIRES pre = assertion { { Spec.pre; post = [Asrt.Pure (Pure_expr.Bool true)] } }
//   | ENSURES post = assertion { { Spec.pre = [Asrt.Pure (Pure_expr.Bool true)]; post } }

pure_expr:
  | i = INT { Pure_expr.Int i }
  | TRUE { Pure_expr.Bool true }
  | FALSE { Pure_expr.Bool false }
  | x = ID { Pure_expr.Var x }
  | NONDET { Pure_expr.NondetInt }
  | e1 = pure_expr PLUS e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.Add, e2) }
  | e1 = pure_expr MINUS e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.Sub, e2) }
  | e1 = pure_expr TIMES e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.Mul, e2) }
  | e1 = pure_expr DIV e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.Div, e2) }
  | e1 = pure_expr AND e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.And, e2) }
  | e1 = pure_expr OR e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.Or, e2) }
  | e1 = pure_expr EQEQ e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.Eq, e2) }
  | e1 = pure_expr LT e2 = pure_expr { Pure_expr.BinOp(e1, BinOp.Lt, e2) }
  | LPAREN e = pure_expr RPAREN { e }

expr:
  | p = pure_expr { Expr.Pure_expr p }
  | np = non_pure_expr { np }

non_pure_expr:
  | LET x = ID EQ e1 = expr IN e2 = expr { Expr.Let(Some x, e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { Expr.If(e1, e2, e3) }
  | e1 = expr SEMI e2 = expr { Expr.Let(None, e1, e2) }
  | e = non_pure_simple_expr { e }

non_pure_simple_expr:
  | LBRACK p = pure_expr RBRACK { Expr.Load p }
  | LBRACK p1 = pure_expr RBRACK ASSIGN p2 = pure_expr { Expr.Store(p1, p2) }
  | ALLOC { Expr.Alloc }
  | FREE LPAREN p = pure_expr RPAREN { Expr.Free p }
  | f = ID LPAREN args = separated_list(COMMA, pure_expr) RPAREN { Expr.Call(f, args) }
  | LPAREN e = non_pure_expr RPAREN { e }

// assertion:
//   | atoms = separated_list(SEMI, atom) { atoms }

// atom:
//   | TAKE x = ID EQ RW LPAREN p = pure_expr RPAREN { Asrt.TakePointsTo(x, p) }
//   | TAKE FREED LPAREN p = pure_expr RPAREN { Asrt.TakePointsToFreed(p) }
//   | p = pure_expr { Asrt.Pure p }

// assertion_eof:
//   | a = assertion EOF { a }
