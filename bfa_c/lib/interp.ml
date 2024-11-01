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

let cast_checked x ty =
  match Typed.cast_checked x ty with
  | Some x -> Csymex.return x
  | None ->
      Fmt.kstr Csymex.not_impl "Failed to cast %a to %a" Typed.ppa x
        Typed.ppa_ty ty

let cast_to_ptr x =
  let open Csymex.Syntax in
  let open Svalue.Infix in
  match Typed.get_ty x with
  | TInt ->
      if%sat (Typed.untyped x) #== Svalue.zero then Csymex.return Typed.Ptr.null
      else
        Fmt.kstr Csymex.not_impl "Int-to-pointer that is not 0: %a" Typed.ppa x
  | TPointer ->
      let x : T.sptr Typed.t = Typed.cast x in
      Csymex.return x
  | _ -> Fmt.kstr Csymex.not_impl "Not a pointer: %a" Typed.ppa x

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

let debug_show ~prog:_ ~args:_ ~state =
  let loc = get_loc () in
  let str = (Fmt.to_to_string Heap.pp) state in
  Csymex.push_give_up (str, loc);
  Result.ok (0s, state)

let find_stub ~prog:_ fname : 'err fun_exec option =
  let name = Cerb_frontend.Pp_symbol.to_string fname in
  if String.starts_with ~prefix:"__nondet__" name then Some nondet_int_fun
  else if String.starts_with ~prefix:"malloc" name then Some C_std.malloc
  else if String.starts_with ~prefix:"free" name then Some C_std.free
  else if String.starts_with ~prefix:"___bfa_debug_show" name then
    Some debug_show
  else None

let rec equality_check ~loc ~state (v1 : [< Typed.T.cval ] Typed.t)
    (v2 : [< Typed.T.cval ] Typed.t) =
  match (Typed.get_ty v1, Typed.get_ty v2) with
  | TInt, TInt | TPointer, TPointer ->
      Result.ok (v1 #== v2 |> Typed.int_of_bool, state)
  | TPointer, TInt ->
      let v2 : T.sint Typed.t = Typed.cast v2 in
      if%sat Typed.(v2 #== zero) then
        Result.ok (v1 #== Typed.Ptr.null |> Typed.int_of_bool, state)
      else Result.error (`UBPointerComparison, loc)
  | TInt, TPointer -> equality_check ~loc ~state v2 v1
  | _ ->
      Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a" Typed.ppa
        v1 Typed.ppa v2

let rec arith_add ~loc ~state (v1 : [< Typed.T.cval ] Typed.t)
    (v2 : [< Typed.T.cval ] Typed.t) =
  match (Typed.get_ty v1, Typed.get_ty v2) with
  | TInt, TInt ->
      let v1 = Typed.cast v1 in
      let v2 = Typed.cast v2 in
      Result.ok (v1 #+ v2, state)
  | TPointer, TInt ->
      let v1 : T.sptr Typed.t = Typed.cast v1 in
      let v2 : T.sint Typed.t = Typed.cast v2 in
      let loc = Typed.Ptr.loc v1 in
      let ofs = (Typed.Ptr.ofs v1) #+ v2 in
      Result.ok (Typed.Ptr.mk loc ofs, state)
  | TInt, TPointer -> arith_add ~loc ~state v2 v1
  | TPointer, TPointer -> Result.error (`UBPointerArithmetic, loc)
  | _ ->
      Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
        Typed.ppa v2

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
  | AilEunary (Address, e) -> eval_expr ~lvalue:true state e
  | AilEunary (op, e) -> (
      let** v, state = eval_expr state e in
      match op with
      | Indirection ->
          if lvalue then Result.ok (v, state)
          else
            let ty = type_of aexpr in
            let* v = cast_to_ptr v in
            Heap.load v ty state
      | Address -> failwith "unreachable: address"
      | _ ->
          Fmt.kstr not_impl "Unsupported unary operator %a" Fmt_ail.pp_unop op)
  | AilEbinary (e1, op, e2) -> (
      (* TODO: Binary operators should return a cval, right now this is not right, I need to model integers *)
      let** v1, state = eval_expr state e1 in
      let** v2, state = eval_expr state e2 in
      match op with
      | Ge ->
          (* TODO: comparison operators for pointers *)
          let* v1 = cast_checked v1 Typed.t_int in
          let* v2 = cast_checked v2 Typed.t_int in
          Result.ok (v1 #>= v2 |> Typed.int_of_bool, state)
      | Gt ->
          let* v1 = cast_checked v1 Typed.t_int in
          let* v2 = cast_checked v2 Typed.t_int in
          Result.ok (v1 #> v2 |> Typed.int_of_bool, state)
      | Lt ->
          let* v1 = cast_checked v1 Typed.t_int in
          let* v2 = cast_checked v2 Typed.t_int in
          Result.ok (v1 #< v2 |> Typed.int_of_bool, state)
      | Le ->
          let* v1 = cast_checked v1 Typed.t_int in
          let* v2 = cast_checked v2 Typed.t_int in
          Result.ok (v1 #<= v2 |> Typed.int_of_bool, state)
      | Eq -> equality_check ~loc ~state v1 v2
      | Arithmetic a_op -> (
          match a_op with
          | Div -> (
              let* v1 = cast_checked v1 Typed.t_int in
              let* v2 = cast_checked v2 Typed.t_int in
              let+ v2 = Typed.check_nonzero v2 in
              match v2 with
              | Ok v2 -> Stdlib.Result.ok (v1 #/ v2, state)
              | Error `NonZeroIsZero ->
                  Stdlib.Result.error (`DivisionByZero, loc))
          | Mul ->
              let* v1 = cast_checked v1 Typed.t_int in
              let+ v2 = cast_checked v2 Typed.t_int in
              Stdlib.Result.ok (v1 #* v2, state)
          | Add -> arith_add ~loc ~state v1 v2
          | _ ->
              Fmt.kstr not_impl "Unsupported arithmetic operator: %a"
                Fmt_ail.pp_arithop a_op)
      | _ ->
          Fmt.kstr not_impl "Unsupported binary operator: %a" Fmt_ail.pp_binop
            op)
  | AilErvalue e ->
      let** lvalue, state = eval_expr state e in
      let ty = type_of e in
      (* At this point, lvalue must be a pointer (including to the stack) *)
      let* lvalue = cast_to_ptr lvalue in
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
      let* ptr = cast_to_ptr ptr in
      let ty = type_of lvalue in
      let++ (), state = Heap.store ptr ty rval state in
      (rval, state)
  | AilSyntax.AilEsizeof (_quals, ty) ->
      let+ res = Layout.size_of_s ty in

      Ok (res, state)
  | AilSyntax.AilEmemberofptr (ptr, member) ->
      let** ptr_v, state = eval_expr state ptr in
      let* ty_pointee =
        type_of ptr
        |> Cerb_frontend.AilTypesAux.referenced_type
        |> Csymex.of_opt_not_impl
             ~msg:"Member of Pointer that isn't of type pointer"
      in
      let* mem_ofs = Layout.member_ofs member ty_pointee in
      arith_add ~loc ~state ptr_v mem_ofs
  | AilSyntax.AilEcompoundAssign (_, _, _)
  | AilSyntax.AilEcond (_, _, _)
  | AilSyntax.AilEcast (_, _, _)
  | AilSyntax.AilEassert _
  | AilSyntax.AilEoffsetof (_, _)
  | AilSyntax.AilEgeneric (_, _)
  | AilSyntax.AilEarray (_, _, _)
  | AilSyntax.AilEstruct (_, _)
  | AilSyntax.AilEunion (_, _, _)
  | AilSyntax.AilEcompound (_, _, _)
  | AilSyntax.AilEmemberof (_, _)
  | AilSyntax.AilEbuiltin _ | AilSyntax.AilEstr _ | AilSyntax.AilEsizeof_expr _
  | AilSyntax.AilEalignof (_, _)
  | AilSyntax.AilEannot (_, _)
  | AilSyntax.AilEva_start (_, _)
  | AilSyntax.AilEva_arg (_, _)
  | AilSyntax.AilEva_copy (_, _)
  | AilSyntax.AilEva_end _ | AilSyntax.AilEprint_type _
  | AilSyntax.AilEbmc_assume _ | AilSyntax.AilEreg_load _
  | AilSyntax.AilEarray_decay _ | AilSyntax.AilEfunction_decay _
  | AilSyntax.AilEatomic _
  | AilSyntax.AilEgcc_statement (_, _) ->
      Fmt.kstr not_impl "Unsupported expr: %a" Fmt_ail.pp_expr aexpr

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
      let* v = cast_checked v Typed.t_int in
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
