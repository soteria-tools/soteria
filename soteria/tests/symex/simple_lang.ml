open Soteria
open Soteria.Logs.Import

module Symex =
  Symex.Make (Soteria.Symex.Meta.Dummy) (Soteria.C_values.C_solver.Z3_solver)

open C_values

module Ast = struct
  module Const = struct
    type t = Int of Z.t

    let pp ft = function Int i -> Fmt.pf ft "%a" Z.pp_print i
  end

  module Binop = struct
    type t = Sub | Geq

    let to_str = function Sub -> "-" | Geq -> ">="
    let pp ft op = Fmt.pf ft "%s" (to_str op)
  end

  module Expr = struct
    type t =
      | Const of Const.t
      | Var of string
      | BinOp of Binop.t * t * t
      | Let of string * t * t
      | If of t * t * t
      | NondetInt
      | Assert of t

    let rec pp ft t =
      match t with
      | Var v -> Fmt.pf ft "%s" v
      | Const c -> Const.pp ft c
      | Let (v, e1, e2) -> Fmt.pf ft "@[<v 2>let %s = %a in@ %a@]" v pp e1 pp e2
      | If (cond, then_branch, else_branch) ->
          Fmt.pf ft "@[<v 0>if %a@ then %a@ else %a@]" pp cond pp then_branch pp
            else_branch
      | NondetInt -> Fmt.pf ft "nondet_int"
      | Assert e -> Fmt.pf ft "assert %a" pp e
      | BinOp (op, e1, e2) -> Fmt.pf ft "(%a %a %a)" pp e1 Binop.pp op pp e2
  end
end

module Eval = struct
  open Ast
  open Symex
  open Syntax
  module T = Typed.T

  type value = [ T.sbool | T.sint ] Typed.t

  module Subst = Map.Make (String)

  type subst = value Subst.t
  type error = AssertError | TypeError

  let pp_error ft = function
    | AssertError -> Fmt.pf ft "assertion failed"
    | TypeError -> Fmt.pf ft "type error"

  let cast_checked (x : value) (t : 'a Typed.ty) :
      ('a Typed.t, error, _) Result.t =
    match Typed.cast_checked x t with
    | Some x -> Result.ok x
    | None -> Result.error TypeError

  let value_of_const (c : Const.t) : value =
    match c with Int z -> Typed.int_z z

  let eval_binop (op : Binop.t) (v1 : value) (v2 : value) :
      (value, error, _) Result.t =
    let open Typed.Infix in
    match op with
    | Geq ->
        let** v1 = cast_checked v1 Typed.t_int in
        let++ v2 = cast_checked v2 Typed.t_int in
        v1 >=@ v2
    | Sub ->
        let** v1 = cast_checked v1 Typed.t_int in
        let++ v2 = cast_checked v2 Typed.t_int in
        v1 -@ v2

  let rec eval (subst : subst) (e : Expr.t) : (value, error, _) Result.t =
    L.debug (fun m -> m "Evaluating expression: %a" Expr.pp e);
    match e with
    | Const c -> Result.ok (value_of_const c)
    | Var x -> Result.ok (Subst.find x subst)
    | BinOp (op, e1, e2) ->
        let** v1 = eval subst e1 in
        let** v2 = eval subst e2 in
        eval_binop op v1 v2
    | Let (x, e1, e2) ->
        let** v1 = eval subst e1 in
        let subst' = Subst.add x v1 subst in
        eval subst' e2
    | If (cond, then_e, else_e) ->
        let** cond_v = eval subst cond in
        let** cond_v = cast_checked cond_v Typed.t_bool in
        if%sat cond_v then eval subst then_e else eval subst else_e
    | NondetInt ->
        let+ v = Symex.nondet Typed.t_int in
        Soteria.Symex.Compo_res.Ok v
    | Assert e ->
        let** v = eval subst e in
        let** v = cast_checked v Typed.t_bool in
        let++ () = Symex.assert_or_error v AssertError in
        Typed.v_true
end

let program =
  let open Ast.Expr in
  let v = "v" in
  let y = "y" in
  let vx = Ast.Expr.Var v in
  let yx = Ast.Expr.Var y in
  let assert_e = Assert (BinOp (Geq, vx, Const (Int Z.zero))) in
  let abs_e =
    If
      ( BinOp (Geq, yx, Const (Int Z.zero)),
        yx,
        BinOp (Sub, Const (Int Z.zero), yx) )
  in
  Let (y, NondetInt, Let (v, abs_e, assert_e))

let pp_result =
  Fmt.Dump.pair
    (Soteria.Symex.Compo_res.pp ~ok:Typed.ppa ~err:Eval.pp_error ~miss:Fmt.nop)
    (Fmt.Dump.list Typed.ppa)

let () =
  Soteria.Logs.Config.check_set_and_lock (Ok { level = Some Smt; kind = Html });
  let results = Symex.run ~mode:OX Eval.(eval Subst.empty program) in
  Fmt.pr "Results: %a\n" (Fmt.Dump.list pp_result) results
