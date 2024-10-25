open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
open Ail_tys
module Ctype = Cerb_frontend.Ctype
module AilSyntax = Cerb_frontend.AilSyntax
module T = Typed.T

exception Unsupported of (string * Cerb_location.t)

let type_of expr = Cerb_frontend.Translation_aux.ctype_of expr

type cval = [ T.sint | T.sptr ]
type state = Heap.t
type store = (T.sptr Typed.t option * Ctype.ctype) Store.t

type 'err fun_exec =
  prog:sigma ->
  args:T.cval Typed.t list ->
  state:state ->
  (cval Typed.t * state, 'err) Result.t

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

let attach_bindings store (bindings : AilSyntax.bindings) =
  ListLabels.fold_left bindings ~init:store
    ~f:(fun store (pname, ((loc, duration, _is_register), align, _quals, ty)) ->
      let@ () = with_loc_immediate ~loc in
      (match duration with
      | AilSyntax.Static | Thread ->
          raise (Unsupported ("static/tread", get_loc ()))
      | _ -> ());
      if Option.is_some align then raise (Unsupported ("align", get_loc ()));
      Store.add pname (None, ty) store)

let attach_bindings store bindings =
  try
    let store = attach_bindings store bindings in
    Csymex.return store
  with Unsupported (msg, loc) ->
    let@ () = with_loc ~loc in
    Csymex.not_impl msg

let alloc_params params st =
  Csymex.Result.fold_left params ~init:(Store.empty, st)
    ~f:(fun (store, st) (pname, ty, value) ->
      let** ptr, st = Heap.alloc_ty ty st in
      let store = Store.add pname (Some ptr, ty) store in
      let++ (), st = Heap.store ptr ty value st in
      (store, st))

let value_of_constant (c : constant) : T.cval Typed.t Csymex.t =
  match c with
  | ConstantInteger (IConstant (z, _basis, _suff)) ->
      Csymex.return (Typed.int_z z)
  | _ -> Csymex.not_impl "value of constant?"

let nondet_int_fun ~prog:_ ~args:_ ~state :
    ([> Typed.T.sint ] Typed.t * state, 'err) Result.t =
  let constrs = Layout.int_constraints (Ctype.Signed Int_) |> Option.get in
  let+ v = Typed.nondet ~constrs Typed.t_int in
  let v = (v :> cval Typed.t) in
  Ok (v, state)

let find_stub ~prog:_ fname : 'err fun_exec option =
  let name = Cerb_frontend.Pp_symbol.to_string fname in
  if String.starts_with ~prefix:"__nondet__" name then Some nondet_int_fun
  else if String.starts_with ~prefix:"malloc" name then Some C_std.malloc
  else None

let rec resolve_function ~(prog : sigma) fexpr : 'err fun_exec Csymex.t =
  let* loc, fname =
    match fexpr with
    | AilSyntax.AnnotatedExpression
        ( _,
          _,
          loc,
          AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname)) )
      ->
        Csymex.return (loc, fname)
    | _ ->
        Fmt.kstr not_impl "Function expression isn't a simple identifier: %a"
          Fmt_ail.pp_expr fexpr
  in
  let@ () = with_loc ~loc in
  let fundef_opt =
    prog.function_definitions
    |> List.find_opt (fun (id, _) -> Cerb_frontend.Symbol.equal_sym id fname)
  in
  match fundef_opt with
  | Some fundef -> Csymex.return (exec_fun fundef)
  | None -> (
      match find_stub ~prog fname with
      | Some stub -> Csymex.return stub
      | None ->
          Fmt.kstr not_impl "Cannot call external function: %a" Fmt_ail.pp_sym
            fname)

and eval_expr_list ~(prog : sigma) ~(store : store) (state : state)
    (el : expr list) =
  let++ vs, state =
    Csymex.Result.fold_left el ~init:([], state) ~f:(fun (acc, state) e ->
        let++ new_res, state = eval_expr ~prog ~store state e in
        (new_res :: acc, state))
  in
  (List.rev vs, state)

and eval_expr ~(prog : sigma) ~(store : store) ?(lvalue = false) (state : state)
    (aexpr : expr) =
  let eval_expr = eval_expr ~prog ~store in
  let (AnnotatedExpression (_, _, loc, expr)) = aexpr in
  let@ () = with_loc ~loc in
  match expr with
  | AilEconst c ->
      let+ v = value_of_constant c in
      Ok (v, state)
  | AilEcall (f, args) ->
      let* exec_fun = resolve_function ~prog f in
      let** args, state = eval_expr_list ~prog ~store state args in
      exec_fun ~prog ~args ~state
  | AilEunary (op, e) -> (
      let** v, state = eval_expr state e in
      match op with
      | Indirection ->
          if lvalue then Result.ok (v, state)
          else
            let ty = type_of aexpr in
            let v = Typed.cast v in
            Heap.load v ty state
      | _ ->
          Fmt.kstr not_impl "Unsupported unary operator %a" Fmt_ail.pp_unop op)
  | AilEbinary (e1, op, e2) -> (
      (* TODO: Binary operators should return a cval, right now this is not right, I need to model integers *)
      let** v1, state = eval_expr state e1 in
      let** v2, state = eval_expr state e2 in
      let v1 = Typed.cast v1 in
      let v2 = Typed.cast v2 in
      (* FIXME: the semantics of value comparison is a lot more complex than this! *)
      match op with
      | Ge -> Result.ok (v1 #>= v2 |> Typed.int_of_bool, state)
      | Gt -> Result.ok (v1 #> v2 |> Typed.int_of_bool, state)
      | Lt -> Result.ok (v1 #< v2 |> Typed.int_of_bool, state)
      | Le -> Result.ok (v1 #<= v2 |> Typed.int_of_bool, state)
      | Arithmetic Div ->
          if%sat v2 #== 0s then Result.error (`DivisionByZero, loc)
          else Result.ok (v1 #/ v2, state)
      | _ ->
          Fmt.kstr not_impl "Unsupported binary operator: %a" Fmt_ail.pp_binop
            op)
  | AilErvalue e ->
      let** lvalue, state = eval_expr state e in
      let ty = type_of e in
      (* At this point, lvalue must be a pointer (including to the stack) *)
      let lvalue = Typed.cast lvalue in
      Heap.load lvalue ty state
  | AilEident id -> (
      match Store.find_value id store with
      | Some v ->
          (* A pointer is a value *)
          let v = (v :> cval Typed.t) in
          Result.ok (v, state)
      | None ->
          Fmt.kstr not_impl "Variable %a not found in store" Fmt_ail.pp_sym id)
  | AilEassign (lvalue, rvalue) ->
      (* Evaluate rvalue first *)
      let** rval, state = eval_expr state rvalue in
      let** ptr, state = eval_expr ~lvalue:true state lvalue in
      (* [ptr] is a necessarily a pointer, and [rval] is a memory value.
         I don't support pointer fragments for now, so let's say it's an *)
      let ptr = Typed.cast ptr in
      let rval = Typed.cast rval in
      let ty = type_of lvalue in
      let++ (), state = Heap.store ptr ty rval state in
      (rval, state)
  | _ -> Fmt.kstr not_impl "Unsupported expr: %a" Fmt_ail.pp_expr aexpr

(** Executing a statement returns an optional value outcome (if a return statement was hit), or  *)
and exec_stmt ~prog (store : store) (state : state) (astmt : stmt) :
    (T.cval Typed.t option * store * state, 'err) Csymex.Result.t =
  let (AnnotatedStatement (loc, _, stmt)) = astmt in
  let@ () = with_loc ~loc in
  match stmt with
  | AilSskip -> Result.ok (None, store, state)
  | AilSreturn e ->
      let** v, state = eval_expr ~prog ~store state e in
      L.info (fun m -> m "Returning: %a" Typed.ppa v);
      Result.ok (Some v, store, state)
  | AilSblock (bindings, stmtl) ->
      let* store = attach_bindings store bindings in
      (* Second result, corresponding to the block-scoped store, is discarded *)
      let++ res, _, state =
        Csymex.Result.fold_left stmtl ~init:(None, store, state)
          ~f:(fun (res, store, state) stmt ->
            match res with
            | Some _ -> Csymex.Result.ok (res, store, state)
            | None -> exec_stmt ~prog store state stmt)
      in
      (res, store, state)
  | AilSexpr e ->
      let** _, state = eval_expr ~prog ~store state e in
      Result.ok (None, store, state)
  | AilSif (cond, then_stmt, else_stmt) ->
      let** v, state = eval_expr ~prog ~store state cond in
      (* [v] must be an integer! (TODO: or NULL possibly...) *)
      let v : T.sint Typed.t = Typed.cast v in
      if%sat Typed.(not v #== 0s) then exec_stmt ~prog store state then_stmt
      else exec_stmt ~prog store state else_stmt
  | AilSdeclaration decls ->
      let++ store, st =
        Csymex.Result.fold_left decls ~init:(store, state)
          ~f:(fun (store, state) (pname, expr) ->
            let* ty =
              Store.find_type pname store
              |> Csymex.of_opt_not_impl ~msg:"Missing binding??"
            in
            let** ptr, state = Heap.alloc_ty ty state in
            let++ (), state =
              match expr with
              | None -> Result.ok ((), state)
              | Some expr ->
                  let** v, state = eval_expr ~prog ~store state expr in
                  Heap.store ptr ty v state
            in
            let store = Store.add pname (Some ptr, ty) store in
            (store, state))
      in
      (None, store, st)
  | _ -> Fmt.kstr not_impl "Unsupported statement: %a" Fmt_ail.pp_stmt astmt

and exec_fun ~prog ~args ~state (fundef : fundef) =
  (* Put arguments in store *)
  let name, (loc, _, _, params, stmt) = fundef in
  let@ () = with_loc ~loc in
  L.info (fun m ->
      m "Executing function %s" (Cerb_frontend.Pp_symbol.to_string name));
  let* ptys = get_param_tys ~prog name in
  let ps = Utils.List_ex.combine3 params ptys args in
  (* TODO: Introduce a with_stack_allocation.
           That would require some kind of continutation passing for executing a bunch of statements. *)
  let** store, state = alloc_params ps state in
  (* TODO: local optimisation to put values in store directly when no address is taken. *)
  let++ val_opt, _, state = exec_stmt ~prog store state stmt in
  (* We model void as zero, it should never be used anyway *)
  let value = Option.value ~default:0s val_opt in
  (value, state)
