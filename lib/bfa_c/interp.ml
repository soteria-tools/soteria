open Csymex
open Csymex.Syntax
open Ail_tys

let type_of expr = Cerb_frontend.Translation_aux.ctype_of expr

type state = Heap.t
type store = Svalue.t Store.t

let not_impl source_loc loc what = not_impl ~source_loc ~loc what

(* TODO: handle qualifiers! *)
let get_param_tys ~prog fid =
  let ptys =
    List.find_map
      (fun (id, (_, _, decl)) ->
        if Cerb_frontend.Symbol.equal_sym id fid then
          match decl with
          | Cerb_frontend.AilSyntax.Decl_function
              (_proto, _ret_qual, ptys, _is_variadic, _is_inline, _is_noreturn)
            ->
              Some (List.map (fun (_, ty, _) -> ty) ptys)
          | _ -> None
        else None)
      prog.Cerb_frontend.AilSyntax.declarations
  in
  Csymex.of_opt_not_impl ~msg:"Couldn't find function prototype" ptys

let alloc_params params st =
  let store = Store.empty in
  Csymex.Result.fold_left params ~init:(store, st)
    ~f:(fun (store, st) (pname, ty, value) ->
      let** ptr, st = Heap.alloc_ty ty st in
      let++ (), st = Heap.store ptr ty value st in
      (Store.add pname ptr store, st))

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
        prog.function_definitions
        |> List.find_opt (fun (id, _) ->
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
      let** lvalue, state = eval_expr ~prog ~store state e in
      let ty = type_of e in
      Heap.load lvalue ty state
      (* let chunk = chunk_of_lvalue lvalue in
         let loc, ofs = Svalue.to_loc_ofs lvalue in
         Heap.load loc ofs chunk state *)
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
    (Svalue.t option * state, 'err) Csymex.Result.t =
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
  let* ptys = get_param_tys ~prog name in
  let ps = Utils.List_ex.combine3 params ptys args in
  let** store, state = alloc_params ps state in
  (* TODO: local optimisation to put values in store directly when no address is taken. *)
  L.debug (fun m -> m "Store: %a" Store.pp store);
  let++ val_opt, state = exec_stmt ~prog store state stmt in
  let value = Option.value ~default:Svalue.void val_opt in
  (value, state)
