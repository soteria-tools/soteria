include Aux
open Soteria_linear_ast.Lang
open Symex
open Symex.Syntax
open S_val.Infix
open Soteria.Logs

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

let rec interp_pure_expr (subst : subst) expr :
    (S_val.t, 'err, 'a) Symex.Result.t =
  match expr with
  | Pure_expr.Int n -> Result.ok (S_val.int n)
  | Bool b -> Result.ok (S_val.bool b)
  | Var x -> Result.ok (String_map.find x subst)
  | BinOp (e1, op, e2) -> (
      let** v1 = interp_pure_expr subst e1 in
      let** v2 = interp_pure_expr subst e2 in
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
          (* TODO: soundness danger! *)
          give_up ~loc:() "Division not implemented yet")
  | NondetInt ->
      let* v = Symex.nondet S_val.t_int in
      Result.ok v

module Make (State : State_intf.S) = struct
  let interp_pure_expr (subst : subst) (state : State.t) expr =
    let+- msg = interp_pure_expr subst expr in
    State.error msg state

  let cast_to_bool v state =
    let+- msg = cast_to_bool v in
    State.error msg state

  let cast_to_int v state =
    let+- msg = cast_to_int v in
    State.error msg state

  let rec interp_expr (subst : subst) (state : State.t) expr =
    L.debug (fun m ->
        m "@[<v 0>@[<v 2>Interp expr:@ %a@]@.@[<v 2>In subst:@ %a@]@]" Expr.pp
          expr pp_subst subst);
    match expr with
    | Expr.Pure_expr e ->
        let++ v = interp_pure_expr subst state e in
        (v, state)
    | Let (x, e1, e2) ->
        let** v1, state = interp_expr subst state e1 in
        let subst =
          Option.fold ~none:subst ~some:(fun x -> String_map.add x v1 subst) x
        in
        interp_expr subst state e2
    | If (guard, then_, else_) ->
        let** v_guard, state = interp_expr subst state guard in
        let** v_guard = cast_to_bool v_guard state in
        if%sat v_guard then interp_expr subst state then_
        else interp_expr subst state else_
    | Call (fname, arg_exprs) ->
        let** arg_values =
          Symex.Result.fold_list arg_exprs ~init:[] ~f:(fun acc e ->
              let++ res = interp_pure_expr subst state e in
              res :: acc)
        in
        let arg_values = List.rev arg_values in
        let func = get_function fname in
        run_function func state arg_values
    | Load addr ->
        let** addr = interp_pure_expr subst state addr in
        let** addr = cast_to_int addr state in
        State.load addr state
    | Store (addr, value) ->
        let** addr = interp_pure_expr subst state addr in
        let** addr = cast_to_int addr state in
        let** value = interp_pure_expr subst state value in
        let++ (), state = State.store addr value state in
        ((S_val.v_false :> S_val.t), state)
    | Alloc ->
        let++ addr, state = State.alloc state in
        ((addr :> S_val.t), state)
    | Free addr ->
        let** addr = interp_pure_expr subst state addr in
        let** addr = cast_to_int addr state in
        let++ (), state = State.free addr state in
        ((S_val.v_false :> S_val.t), state)

  and run_function func state args =
    let subst = List.combine func.Fun_def.args args |> String_map.of_list in
    L.debug (fun m ->
        m "@[<v 2>Running function %s with args:@ %a@]" func.Fun_def.name
          pp_subst subst);
    let++ r = interp_expr subst state func.Fun_def.body in
    L.debug (fun m ->
        m "@[<v 2>Function %s returned:@ %a@]" func.Fun_def.name
          (Fmt.Dump.pair S_val.pp State.pp)
          r);
    r
end
