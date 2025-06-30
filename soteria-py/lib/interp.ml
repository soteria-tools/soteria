module InterpM = struct end
open PyreAst.Concrete

let value_of_constant (c : Constant.t) =
  match c with
  | True -> Typed.v_true
  | False -> Typed.v_false
  | None | Ellipsis | Integer _ | BigInteger _ | Float _ | Complex _ | String _
  | ByteString _ ->
      failwith "Unsupported constant type"

let eval_expr (expr : Expression.t) =
  match expr with
  | Constant { location = _; kind = _; value = c } -> value_of_constant c
  | BoolOp _ | NamedExpr _ | BinOp _ | UnaryOp _ | Lambda _ | IfExp _ | Dict _
  | Set _ | ListComp _ | SetComp _ | DictComp _ | GeneratorExp _ | Await _
  | Yield _ | YieldFrom _ | Compare _ | Call _ | FormattedValue _ | JoinedStr _
  | Attribute _ | Subscript _ | Starred _ | Name _ | List _ | Tuple _ | Slice _
    ->
      failwith "Unsupported expression type"

let exec_stmt (stmt : Statement.t) =
  match stmt with
  | Assert { location = _; test; msg = _ } ->
      let _ = eval_expr test in
      ()
  | _ -> failwith "Unsupported statement"

let exec_module (module_ : Module.t) =
  let statements = module_.body in
  List.fold_left
    (fun _ stmt ->
      exec_stmt stmt;
      ())
    () statements
