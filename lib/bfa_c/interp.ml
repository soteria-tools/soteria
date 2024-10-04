open Csymex
open Csymex.Syntax
open Ail_tys

type state = unit
type store = Svalue.t Store.t

let not_impl source_loc loc what =
  L.info (fun m ->
      m "%s@\n%s not supported (%s)"
        (Cerb_location.location_to_string source_loc)
        what loc);
  Csymex.vanish ()

let value_of_constant (c : constant) =
  match c with
  | ConstantInteger (IConstant (z, _basis, _suff)) ->
      Csymex.return (Svalue.int_z z)
  | _ -> Csymex.vanish ()

let rec eval_expr_list ~(prog : sigma) ~(store : store) (state : state)
    (el : expr list) =
  let++ vs, state =
    Csymex.Result.fold_left el ~init:([], state) ~f:(fun (acc, state) e ->
        let++ new_res, state = eval_expr ~prog ~store state e in
        (new_res :: acc, state))
  in
  (List.rev vs, state)

and eval_expr ~(prog : sigma) ~(store : store) (state : state) (aexpr : expr) =
  let (AnnotatedExpression (_, _, loc, expr)) = aexpr in
  match expr with
  | AilEconst c ->
      let+ v = value_of_constant c in
      Ok (v, state)
  (* | AilEfunction_decay k -> (
      match k with
      | AnnotatedExpression (_, _, _, AilEident _) ->
          not_impl loc __LOC__ "Unsupported function call expr"
      | _ -> not_impl loc __LOC__ "AilEfunction_decay is not an ident") *)
  | AilEcall (f, args) ->
      let* fname =
        match f with
        | AnnotatedExpression
            ( _,
              _,
              _,
              AilEfunction_decay
                (AnnotatedExpression (_, _, _, AilEident fname)) ) ->
            Csymex.return fname
        | _ ->
            Fmt.kstr (not_impl loc __LOC__)
              "Function expression isn't a simple identifier: %a"
              Fmt_ail.pp_expr f
      in
      let fundef =
        List.find prog.function_definitions ~f:(fun (id, _) ->
            Cerb_frontend.Symbol.equal_sym id fname)
      in
      let* fundef =
        match fundef with
        | Some fundef -> Csymex.return fundef
        | None ->
            Fmt.kstr (not_impl loc __LOC__) "Cannot call external function: %a"
              Fmt_ail.pp_expr f
      in
      let** args, state = eval_expr_list ~prog ~store state args in
      exec_fun ~prog ~args state fundef
  | AilEbinary (e1, op, e2) -> (
      let** v1, state = eval_expr ~prog ~store state e1 in
      let** v2, state = eval_expr ~prog ~store state e2 in
      match op with
      | Ge -> Result.ok (Svalue.geq v1 v2, state)
      | Gt -> Result.ok (Svalue.gt v1 v2, state)
      | _ ->
          Fmt.kstr (not_impl loc __LOC__) "Unsupported binary operator: %a"
            Fmt_ail.pp_binop op)
  | AilErvalue e ->
      L.warn (fun m ->
          m "Rvalue expression: %a, will change when we put things in heap!!"
            Fmt_ail.pp_expr e);
      eval_expr ~prog ~store state e
  | AilEident id -> (
      match Store.find_opt id store with
      | Some v -> Result.ok (v, state)
      | None ->
          Fmt.kstr (not_impl loc __LOC__) "Variable %a not found in store"
            Fmt_ail.pp_sym id)
  | _ ->
      Fmt.kstr (not_impl loc __LOC__) "Unsupported expr: %a" Fmt_ail.pp_expr
        aexpr

(** Executing a statement returns an optional value outcome (if a return statement was hit), or  *)
and exec_stmt ~prog (store : store) (state : state) (astmt : stmt) :
    (Svalue.t option * state, string) Csymex.Result.t =
  let (AnnotatedStatement (loc, _, stmt)) = astmt in
  match stmt with
  | AilSskip -> Result.ok (None, state)
  | AilSreturn e ->
      let** v, state = eval_expr ~prog ~store state e in
      L.info (fun m -> m "Returning: %a" Svalue.pp v);
      Result.ok (Some v, state)
  | AilSblock (_, stmtl) ->
      Csymex.Result.fold_left stmtl ~init:(None, state)
        ~f:(fun (res, state) stmt ->
          match res with
          | Some _ -> Csymex.Result.ok (res, state)
          | None -> exec_stmt ~prog store state stmt)
  | AilSif (cond, then_stmt, else_stmt) ->
      let** v, state = eval_expr ~prog ~store state cond in
      if%sat v then exec_stmt ~prog store state then_stmt
      else exec_stmt ~prog store state else_stmt
  | _ ->
      Fmt.kstr (not_impl loc __LOC__) "Unsupported statement: %a"
        Fmt_ail.pp_stmt astmt

and exec_fun ~prog ~args state (fundef : fundef) =
  (* Put arguments in store *)
  let name, (_, _, _, params, stmt) = fundef in
  L.info (fun m ->
      m "Executing function %s" (Cerb_frontend.Pp_symbol.to_string name));
  let store = Store.of_list (List.zip_exn params args) in
  L.debug (fun m -> m "Store: %a" Store.pp store);
  let++ val_opt, state = exec_stmt ~prog store state stmt in
  let value = Option.value ~default:Svalue.void val_opt in
  (value, state)
