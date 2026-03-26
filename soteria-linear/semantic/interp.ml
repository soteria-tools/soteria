include Aux
open Syntaxes.FunctionWrap
open Soteria_linear_ast.Lang
open Symex
open Symex.Syntax
open S_val.Infix
open Soteria.Logs
module Compo_res = Soteria.Symex.Compo_res

let cast_both (ty : [< S_val.T.any ] S_val.ty) v1 v2 =
  let v1 = S_val.cast_checked v1 ty in
  let v2 = S_val.cast_checked v2 ty in
  match (v1, v2) with
  | Some v1, Some v2 -> Result.ok (v1, v2)
  | _ -> Result.error "Type error"

let cast_to_bool v =
  match S_val.cast_checked v S_val.t_bool with
  | Some v -> Result.ok v
  | None -> Result.error "Type error"

let cast_to_int v =
  match S_val.cast_checked v S_val.t_int with
  | Some v -> Result.ok v
  | None -> Result.error "Type error"

let rec eval_pure_expr (subst : subst) expr : (S_val.t, 'err, 'a) Symex.Result.t
    =
  match expr with
  | Pure_expr.Int n -> Result.ok (S_val.int n)
  | Bool b -> Result.ok (S_val.bool b)
  | Var x -> Result.ok (String_map.find x subst)
  | NondetInt ->
      let* v = Symex.nondet S_val.t_int in
      Result.ok v
  | BinOp (e1, op, e2) -> (
      let** v1 = eval_pure_expr subst e1 in
      let** v2 = eval_pure_expr subst e2 in
      match op with
      | BinOp.Eq -> Symex.Result.ok (v1 ==?@ v2)
      | BinOp.And ->
          let++ v1, v2 = cast_both S_val.t_bool v1 v2 in
          v1 &&@ v2
      | BinOp.Or ->
          let++ v1, v2 = cast_both S_val.t_bool v1 v2 in
          v1 ||@ v2
      | BinOp.Lt ->
          let++ v1, v2 = cast_both S_val.t_int v1 v2 in
          v1 <@ v2
      | BinOp.Add ->
          let++ v1, v2 = cast_both S_val.t_int v1 v2 in
          v1 +@ v2
      | BinOp.Sub ->
          let++ v1, v2 = cast_both S_val.t_int v1 v2 in
          v1 -@ v2
      | BinOp.Mul ->
          let++ v1, v2 = cast_both S_val.t_int v1 v2 in
          v1 *@ v2
      | BinOp.Div ->
          let** v1, v2 = cast_both S_val.t_int v1 v2 in
          let++ v2 = S_val.check_nonzero v2 in
          v1 /@ v2)

module Make (State : State_intf.S) = struct
  module SM = State.SM
  open SM.Syntax

  let lift_to_state f =
    let+- err = SM.lift f in
    `Interp err

  let eval_pure_expr subst expr = lift_to_state (eval_pure_expr subst expr)
  let cast_to_bool v = lift_to_state (cast_to_bool v)
  let cast_to_int v = lift_to_state (cast_to_int v)

  module Asrt_executor = Logic.Asrt.Execute (State)

  let subst_res subst res =
    match res with
    | Ok v -> Compo_res.Ok (S_val.subst subst v)
    | Error e -> Compo_res.Error e

  let exec_spec args spec st =
    (* Bit of a gymnastic here, I wonder if things can be improved to have a
       better interface. *)
    let open Symex in
    let open Syntax in
    L.debug (fun m ->
        m "@[<v 2>Executing specification:@ %a@]" Context.pp_spec spec);
    let Context.{ args = params; pre; post; pc; ret } = spec in
    let asrt = Logic.Asrt.make ~spatial:pre ~pure:[] in
    let* frame =
      let@@ () = Consumer.run_consumer ~subst:Value.Expr.Subst.empty in
      let open Consumer in
      let open Syntax in
      let pairs = List.combine params args in
      let* () =
        iter_list pairs ~f:(fun (param, arg) -> S_val.learn_eq param arg)
      in
      Asrt_executor.consume asrt st
    in
    match frame with
    | Missing m -> return (Compo_res.Missing m, st)
    | Error e -> return (Compo_res.Error (e :> Error.t), st)
    | Ok (frame, subst) ->
        let+ (v, st), _ =
          let open Producer in
          let@@ () = run_producer ~subst in
          let open Syntax in
          let post = Logic.Asrt.make ~spatial:post ~pure:pc in
          let* st = Asrt_executor.produce post frame in
          let+ v = apply_subst subst_res ret in
          (v, st)
        in
        (v, st)

  let rec eval_expr (subst : subst) expr : (S_val.t, 'err, 'fix) SM.Result.t =
    let* () = SM.consume_fuel_steps 1 in
    L.debug (fun m ->
        m "@[<v 0>@[<v 2>Interp expr:@ %a@]@.@[<v 2>In subst:@ %a@]@]" Expr.pp
          expr pp_subst subst);
    match expr with
    | Expr.Pure_expr e -> eval_pure_expr subst e
    | Let (x, e1, e2) ->
        let** v1 = eval_expr subst e1 in
        let subst =
          Option.fold ~none:subst ~some:(fun x -> String_map.add x v1 subst) x
        in
        eval_expr subst e2
    | If (guard, then_, else_) ->
        let** v_guard = eval_expr subst guard in
        let** v_guard = cast_to_bool v_guard in
        if%sat v_guard then eval_expr subst then_ else eval_expr subst else_
    | Call (fname, arg_exprs) -> (
        let** arg_values =
          SM.Result.map_list arg_exprs ~f:(eval_pure_expr subst)
        in
        let func_interp = Context.get_interp fname in
        match func_interp with
        | Inline ->
            let func = Context.get_function fname in
            eval_function func arg_values
        | Use_specs specs ->
            let spec_runs =
              List.map (fun specs () -> exec_spec arg_values specs) specs
            in
            SM.branches spec_runs)
    | Load addr ->
        let** addr = eval_pure_expr subst addr in
        let** addr = cast_to_int addr in
        State.load addr
    | Store (addr, value) ->
        let** addr = eval_pure_expr subst addr in
        let** addr = cast_to_int addr in
        let** value = eval_pure_expr subst value in
        let++ () = State.store addr value in
        (S_val.v_false :> S_val.t)
    | Alloc ->
        let++ addr = State.alloc () in
        (addr :> S_val.t)
    | Free addr ->
        let** addr = eval_pure_expr subst addr in
        let** addr = cast_to_int addr in
        let++ () = State.free addr in
        (S_val.v_false :> S_val.t)

  and eval_function func args =
    let subst = List.combine func.Fun_def.args args |> String_map.of_list in
    L.debug (fun m ->
        m "@[<v 2>Running function %s with args:@ %a@]" func.Fun_def.name
          pp_subst subst);
    let++ r = eval_expr subst func.Fun_def.body in
    L.debug (fun m ->
        m "@[<v 2>Function %s returned:@ %a@]" func.Fun_def.name S_val.pp r);
    r
end
