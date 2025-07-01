open PyreAst.Concrete
open Pysymex
open Soteria_symex.Compo_res
module T = Typed.T
open Pysymex.Syntax

module InterpM = struct
  (* There are no real errors in python, everything is successful module exceptions. *)
  type error = |
  type 'a t = State.t -> ('a * State.t, error, State.serialized list) Result.t

  let ok x = fun state -> Result.ok (x, state)
  let return x = ok x
  let error err = fun _ -> Result.error err
  let not_impl str = fun _ -> Pysymex.not_impl str

  let bind x f =
   fun state ->
    let** y, state = x state in
    (f y) state

  let map f x =
   fun state ->
    let++ y, state = x state in
    (f y, state)

  let lift_symex (s : 'a Pysymex.t) : 'a t =
   fun state ->
    let+ s = s in
    Ok (s, state)

  let fold_list x ~init ~f =
    Monad.foldM ~bind ~return:ok ~fold:Foldable.List.fold x ~init ~f

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let^ ) x f = bind (lift_symex x) f

    module Symex_syntax = struct
      let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
       fun state ->
        Pysymex.branch_on ?left_branch_name ?right_branch_name guard
          ~then_:(fun () -> then_ () state)
          ~else_:(fun () -> else_ () state)
    end
  end
end

open InterpM.Syntax

(* let truthiness (v : Pyval.t) : T.sbool InterpM.t =
  match v with
  |  *)

let value_of_constant (c : Constant.t) : Pyval.t Pysymex.t =
  match c with
  | True -> Pysymex.return Pyval.v_true
  | False -> Pysymex.return Pyval.v_false
  | Integer i -> Pysymex.return (Pyval.int i)
  | BigInteger z_str ->
      let z = Z.of_string z_str in
      Pysymex.return (Pyval.int_z z)
  | None -> Pysymex.return Pyval.none
  (* | NotImplemented -> Pysymex.return Pyval.not_implemented *)
  | Ellipsis -> Pysymex.return Pyval.ellipsis
  | Float f -> Pysymex.return (Pyval.float f) (* | Complex (re, im) -> *)
  | Complex _ | String _ | ByteString _ ->
      Fmt.kstr Pysymex.not_impl "Unsupported constant type: %a"
        Fmt_ast.pp_constant c

let eval_expr (expr : Expression.t) : Pyval.t InterpM.t =
  match expr with
  | Constant { location = _; kind = _; value = c } ->
      InterpM.lift_symex (value_of_constant c)
  | BoolOp _ | NamedExpr _ | BinOp _ | UnaryOp _ | Lambda _ | IfExp _ | Dict _
  | Set _ | ListComp _ | SetComp _ | DictComp _ | GeneratorExp _ | Await _
  | Yield _ | YieldFrom _ | Compare _ | Call _ | FormattedValue _ | JoinedStr _
  | Attribute _ | Subscript _ | Starred _ | Name _ | List _ | Tuple _ | Slice _
    ->
      Fmt.kstr InterpM.not_impl "Unsupported expression type: %a"
        Fmt_ast.pp_expr expr

let exec_stmt (stmt : Statement.t) : unit InterpM.t =
  match stmt with
  | Assert { location = _; test; msg = _ } ->
      let* _v = eval_expr test in
      failwith "aaa"
  | _ ->
      Fmt.kstr InterpM.not_impl "Unsupported statement: %a" Fmt_ast.pp_stmt stmt

let exec_module (module_ : Module.t) =
  let statements = module_.body in
  InterpM.fold_list statements ~init:() ~f:(fun _ stmt -> exec_stmt stmt)
