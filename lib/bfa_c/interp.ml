open Csymex
open Csymex.Syntax
open Ail_tys

type state = unit

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

let rec eval_expr ~(prog : sigma) (state : state) (aexpr : expr) =
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
      let** args, state =
        Csymex.Result.fold_left args ~init:([], state)
          ~f:(fun (acc, state) arg ->
            let++ new_res, state = eval_expr ~prog state arg in
            (new_res :: acc, state))
      in
      exec_fun ~prog ~args state fundef
  | _ ->
      Fmt.kstr (not_impl loc __LOC__) "Unsupported expr: %a" Fmt_ail.pp_expr
        aexpr

(** Executing a statement returns an optional value outcome (if a return statement was hit), or  *)
and exec_stmt ~prog (state : state) (astmt : stmt) :
    (Svalue.t option * state, string) Csymex.Result.t =
  let (AnnotatedStatement (loc, _, stmt)) = astmt in
  match stmt with
  | AilSskip -> Result.ok (None, state)
  | AilSreturn e ->
      let** v, state = eval_expr ~prog state e in
      L.info (fun m -> m "Returning: %a" Svalue.pp v);
      Result.ok (Some v, state)
  | AilSblock (_, stmtl) ->
      Csymex.Result.fold_left stmtl ~init:(None, state)
        ~f:(fun (res, state) stmt ->
          match res with
          | Some _ -> Csymex.Result.ok (res, state)
          | None -> exec_stmt ~prog state stmt)
  | _ ->
      Fmt.kstr (not_impl loc __LOC__) "Unsupported statement: %a"
        Fmt_ail.pp_stmt astmt

and exec_fun ~prog ~args state (fundef : fundef) =
  L.debug (fun m -> m "Args: %a" Fmt.(Dump.list Svalue.pp) args);
  if not @@ List.is_empty args then Fmt.failwith "function with arguments";
  L.info (fun m -> m "Executing a function");
  let _, (_, _, _, _, stmt) = fundef in
  let++ val_opt, state = exec_stmt ~prog state stmt in
  let value = Option.value ~default:Svalue.void val_opt in
  (value, state)
