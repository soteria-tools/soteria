open Bfa_symex.Compo_res
open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
open Ail_tys
module Ctype = Cerb_frontend.Ctype
module AilSyntax = Cerb_frontend.AilSyntax
module T = Typed.T

module Make (Heap : Heap_intf.S) = struct
  module C_std = C_std.M (Heap)

  exception Unsupported of (string * Cerb_location.t)

  let type_of expr = Cerb_frontend.Translation_aux.ctype_of expr
  let unwrap_ctype (Ctype.Ctype (_, ty)) = ty

  let pointer_inner (Ctype.Ctype (_, ty)) =
    match ty with Pointer (_, ty) -> Some ty | _ -> None

  type state = Heap.t
  type store = (T.sptr Typed.t option * Ctype.ctype) Store.t

  let cast_checked ~ty x =
    match Typed.cast_checked x ty with
    | Some x -> Csymex.return x
    | None ->
        Fmt.kstr Csymex.not_impl "Failed to cast %a to %a" Typed.ppa x
          Typed.ppa_ty ty

  let cast_to_ptr x =
    let open Csymex.Syntax in
    let open Typed.Infix in
    match Typed.get_ty x with
    | TInt ->
        if%sat x ==@ Typed.zero then Csymex.return Typed.Ptr.null
        else
          Fmt.kstr Csymex.not_impl "Int-to-pointer that is not 0: %a" Typed.ppa
            x
    | TPointer ->
        let x : T.sptr Typed.t = Typed.cast x in
        Csymex.return x
    | _ -> Fmt.kstr Csymex.not_impl "Not a pointer: %a" Typed.ppa x

  type 'err fun_exec =
    prog:sigma ->
    args:T.cval Typed.t list ->
    state:state ->
    (T.cval Typed.t * state, 'err, Heap.serialized list) Result.t

  let get_param_tys ~prog name =
    let ptys = Ail_helpers.get_param_tys ~prog name in
    Csymex.of_opt_not_impl ~msg:"Couldn't find function prototype" ptys

  let attach_bindings store (bindings : AilSyntax.bindings) =
    ListLabels.fold_left bindings ~init:store
      ~f:(fun
          store (pname, ((loc, duration, _is_register), align, _quals, ty)) ->
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

  (* We're assuming all bindings declared, and they have already been removed from the store. *)
  let free_bindings store state (bindings : AilSyntax.bindings) =
    Result.fold_list bindings ~init:state
      ~f:(fun heap (pname, ((loc, _, _), _, _, _)) ->
        let@ () = with_loc_immediate ~loc in
        match Store.find_value pname store with
        | Some ptr ->
            let++ (), heap = Heap.free ptr heap in
            heap
        | None -> Result.ok heap)

  let alloc_params params st =
    Csymex.Result.fold_list params ~init:(Store.empty, st)
      ~f:(fun (store, st) (pname, ty, value) ->
        let** ptr, st = Heap.alloc_ty ty st in
        let store = Store.add pname (Some ptr, ty) store in
        let++ (), st = Heap.store ptr ty value st in
        (store, st))

  let dealloc_store store st =
    Csymex.Result.fold_list (Store.bindings store) ~init:st
      ~f:(fun st (_, (ptr, _)) ->
        match ptr with
        | None -> Result.ok st
        | Some ptr ->
            let++ (), st = Heap.free ptr st in
            st)

  let value_of_constant (c : constant) : T.cval Typed.t Csymex.t =
    match c with
    | ConstantInteger (IConstant (z, _basis, _suff)) ->
        Csymex.return (Typed.int_z z)
    | _ -> Csymex.not_impl "value of constant?"

  let debug_show ~prog:_ ~args:_ ~state =
    let loc = get_loc () in
    let str = (Fmt.to_to_string (Heap.pp_pretty ~ignore_freed:false)) state in
    Csymex.push_give_up (str, loc);
    Result.ok (0s, state)

  let unwrap_expr (AnnotatedExpression (_, _, _, e) : expr) = e

  let find_stub ~prog:_ fname : 'err fun_exec option =
    let name = Cerb_frontend.Pp_symbol.to_string fname in
    if String.starts_with ~prefix:"__nondet__" name then
      Some C_std.nondet_int_fun
    else if String.starts_with ~prefix:"malloc" name then Some C_std.malloc
    else if String.starts_with ~prefix:"free" name then Some C_std.free
    else if String.starts_with ~prefix:"memcpy" name then Some C_std.memcpy
    else if String.starts_with ~prefix:"__assert__" name then Some C_std.assert_
    else if String.starts_with ~prefix:"___bfa_debug_show" name then
      Some debug_show
    else None

  let cast ~old_ty:(Ctype.Ctype (_, old_ty)) ~new_ty:(Ctype.Ctype (_, new_ty))
      (v : [> T.cval ] Typed.t) =
    let open Typed in
    match (old_ty, new_ty) with
    | Ctype.Basic (Integer _), Ctype.Pointer (_quals, _ty) -> (
        match get_ty v with
        | TInt -> return (Ptr.mk Ptr.null_loc (Typed.cast v))
        | TPointer -> return v
        | _ ->
            Fmt.kstr Csymex.not_impl "BUG: not a valid C value: %a" Typed.ppa v)
    | Ctype.Pointer (_, _), Ctype.Pointer (_, _) -> return v
    | _ ->
        Fmt.kstr Csymex.not_impl "Cast %a -> %a" Fmt_ail.pp_ty_ old_ty
          Fmt_ail.pp_ty_ new_ty

  let rec equality_check ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt | TPointer, TPointer ->
        Result.ok (v1 ==@ v2 |> Typed.int_of_bool, state)
    | TPointer, TInt ->
        let v2 : T.sint Typed.t = Typed.cast v2 in
        if%sat Typed.(v2 ==@ zero) then
          Result.ok (v1 ==@ Typed.Ptr.null |> Typed.int_of_bool, state)
        else Heap.error `UBPointerComparison state
    | TInt, TPointer -> equality_check ~state v2 v1
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let rec arith_add ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 +@ v2, state)
    | TPointer, TInt ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs = Typed.Ptr.ofs v1 +@ v2 in
        Result.ok (Typed.Ptr.mk loc ofs, state)
    | TInt, TPointer -> arith_add ~state v2 v1
    | TPointer, TPointer -> Heap.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
          Typed.ppa v2

  let arith_mul ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 *@ v2, state)
    | TPointer, _ | _, TPointer -> Heap.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in multiplication: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let rec resolve_function ~(prog : sigma) fexpr : 'err fun_exec Csymex.t =
    let* loc, fname =
      match fexpr with
      | AilSyntax.AnnotatedExpression
          ( _,
            _,
            loc,
            AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname))
          ) ->
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
      Csymex.Result.fold_list el ~init:([], state) ~f:(fun (acc, state) e ->
          let++ new_res, state = eval_expr ~prog ~store state e in
          (new_res :: acc, state))
    in
    (List.rev vs, state)

  and eval_expr ~(prog : sigma) ~(store : store) (state : state) (aexpr : expr)
      =
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
        let++ v, state =
          let+- err = exec_fun ~prog ~args ~state in
          Heap.add_to_call_trace err
            (Call_trace.make_element ~loc ~msg:"Call trace" ())
        in
        L.debug (fun m -> m "returned %a from %a" Typed.ppa v Fmt_ail.pp_expr f);
        (v, state)
    | AilEunary (Address, e) -> (
        match unwrap_expr e with
        | AilEunary (Indirection, e) -> (* &*e <=> e *) eval_expr state e
        | AilEident id -> (
            match Store.find_value id store with
            | Some ptr -> Result.ok ((ptr :> T.cval Typed.t), state)
            | None ->
                Fmt.kstr not_impl "Variable %a is not declared" Fmt_ail.pp_sym
                  id)
        | _ -> Fmt.kstr not_impl "Unsupported address_of: %a" Fmt_ail.pp_expr e)
    | AilEunary (op, e) -> (
        let** v, state = eval_expr state e in
        match op with
        | PostfixIncr ->
            (* TODO: not quite sure if I should be evaluating e in lvalue mode or not *)
            let* ptr = cast_to_ptr v in
            let** v, state = Heap.load ptr (type_of e) state in
            let* incr_operand =
              match type_of e |> pointer_inner with
              | Some ty -> Layout.size_of_s ty
              | None -> return 1s
            in
            let** v_incr, state = arith_add ~state v incr_operand in
            let++ (), state = Heap.store ptr (type_of e) v_incr state in
            (v, state)
        | Indirection -> Result.ok (v, state)
        | Address -> failwith "unreachable: address_of already handled"
        | _ ->
            Fmt.kstr not_impl "Unsupported unary operator %a" Fmt_ail.pp_unop op
        )
    | AilEbinary (e1, op, e2) -> (
        (* TODO: Binary operators should return a cval, right now this is not right, I need to model integers *)
        let** v1, state = eval_expr state e1 in
        let** v2, state = eval_expr state e2 in
        match op with
        | Ge ->
            (* TODO: comparison operators for pointers *)
            let* v1 = cast_checked v1 ~ty:Typed.t_int in
            let* v2 = cast_checked v2 ~ty:Typed.t_int in
            Result.ok (v1 >=@ v2 |> Typed.int_of_bool, state)
        | Gt ->
            let* v1 = cast_checked v1 ~ty:Typed.t_int in
            let* v2 = cast_checked v2 ~ty:Typed.t_int in
            Result.ok (v1 >@ v2 |> Typed.int_of_bool, state)
        | Lt ->
            let* v1 = cast_checked v1 ~ty:Typed.t_int in
            let* v2 = cast_checked v2 ~ty:Typed.t_int in
            Result.ok (v1 <@ v2 |> Typed.int_of_bool, state)
        | Le ->
            let* v1 = cast_checked v1 ~ty:Typed.t_int in
            let* v2 = cast_checked v2 ~ty:Typed.t_int in
            Result.ok (v1 <=@ v2 |> Typed.int_of_bool, state)
        | Eq -> equality_check ~state v1 v2
        | Ne ->
            (* TODO: Semantics of Ne might be different from semantics of not eq? *)
            let++ res, state = equality_check ~state v1 v2 in
            (Typed.not_int_bool res, state)
        | And ->
            let* v1 = cast_checked v1 ~ty:Typed.t_int in
            let+ v2 = cast_checked v2 ~ty:Typed.t_int in
            let b_res = Typed.bool_of_int v1 &&@ Typed.bool_of_int v2 in
            Ok (Typed.int_of_bool b_res, state)
        | Arithmetic a_op -> (
            match a_op with
            | Div -> (
                let* v1 = cast_checked v1 ~ty:Typed.t_int in
                let* v2 = cast_checked v2 ~ty:Typed.t_int in
                let* v2 = Csymex.check_nonzero v2 in
                match v2 with
                | Ok v2 -> Csymex.Result.ok (v1 /@ v2, state)
                | Error `NonZeroIsZero -> Heap.error `DivisionByZero state
                | Missing e -> (* Unreachable but still *) Csymex.Result.miss e)
            | Mul -> arith_mul ~state v1 v2
            | Add -> (
                match
                  (type_of e1 |> pointer_inner, type_of e2 |> pointer_inner)
                with
                | Some _, Some _ -> Heap.error `UBPointerArithmetic state
                | Some ty, None ->
                    let* factor = Layout.size_of_s ty in
                    let** v2, state = arith_mul ~state v2 factor in
                    arith_add ~state v1 v2
                | None, Some ty ->
                    let* factor = Layout.size_of_s ty in
                    let** v1, state = arith_mul ~state v1 factor in
                    arith_add ~state v2 v1
                | None, None -> arith_add ~state v1 v2)
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
            let v = (v :> T.cval Typed.t) in
            Result.ok (v, state)
        | None ->
            Fmt.kstr not_impl "Variable %a not found in store" Fmt_ail.pp_sym id
        )
    | AilEassign (lvalue, rvalue) ->
        (* Evaluate rvalue first *)
        let** rval, state = eval_expr state rvalue in
        let** ptr, state = eval_expr state lvalue in
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
        arith_add ~state ptr_v mem_ofs
    | AilSyntax.AilEcast (_quals, new_ty, expr) ->
        let old_ty = type_of expr in
        let** v, state = eval_expr state expr in
        let+ new_v = cast ~old_ty ~new_ty v in
        Ok (new_v, state)
    | AilSyntax.AilEcompoundAssign (_, _, _)
    | AilSyntax.AilEcond (_, _, _)
    | AilSyntax.AilEassert _
    | AilSyntax.AilEoffsetof (_, _)
    | AilSyntax.AilEgeneric (_, _)
    | AilSyntax.AilEarray (_, _, _)
    | AilSyntax.AilEstruct (_, _)
    | AilSyntax.AilEunion (_, _, _)
    | AilSyntax.AilEcompound (_, _, _)
    | AilSyntax.AilEmemberof (_, _)
    | AilSyntax.AilEbuiltin _ | AilSyntax.AilEstr _
    | AilSyntax.AilEsizeof_expr _
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

  (** Executing a statement returns an optional value outcome (if a return
      statement was hit), or *)
  and exec_stmt ~prog (store : store) (state : state) (astmt : stmt) :
      ( T.cval Typed.t option * store * state,
        'err,
        Heap.serialized list )
      Csymex.Result.t =
    L.debug (fun m -> m "Executing statement: %a" Fmt_ail.pp_stmt astmt);
    let* () = Csymex.consume_fuel_steps 1 in
    Stats.incr_executed_statements ();
    let (AnnotatedStatement (loc, _, stmt)) = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | AilSskip -> Result.ok (None, store, state)
    | AilSreturn e ->
        let** v, state = eval_expr ~prog ~store state e in
        L.info (fun m -> m "Returning: %a" Typed.ppa v);
        Result.ok (Some v, store, state)
    | AilSblock (bindings, stmtl) ->
        let previous_store = store in
        let* store = attach_bindings store bindings in
        (* Second result, corresponding to the block-scoped store, is discarded *)
        let** res, store, state =
          Csymex.Result.fold_list stmtl ~init:(None, store, state)
            ~f:(fun (res, store, state) stmt ->
              match res with
              | Some _ -> Csymex.Result.ok (res, store, state)
              | None -> exec_stmt ~prog store state stmt)
        in
        let++ state = free_bindings store state bindings in
        (res, previous_store, state)
    | AilSexpr e ->
        let** _, state = eval_expr ~prog ~store state e in
        Result.ok (None, store, state)
    | AilSif (cond, then_stmt, else_stmt) ->
        let** v, state = eval_expr ~prog ~store state cond in
        (* [v] must be an integer! (TODO: or NULL possibly...) *)
        let* v = cast_checked v ~ty:Typed.t_int in
        if%sat Typed.bool_of_int v then exec_stmt ~prog store state then_stmt
        else exec_stmt ~prog store state else_stmt
    | AilSwhile (cond, stmt, _loopid) ->
        let rec loop store state =
          let** cond_v, state = eval_expr ~prog ~store state cond in
          let* cond_v = cast_checked cond_v ~ty:Typed.t_int in
          if%sat Typed.bool_of_int cond_v then
            let** res, store, state = exec_stmt ~prog store state stmt in
            match res with
            | Some _ -> Result.ok (res, store, state)
            | None -> loop store state
          else Result.ok (None, store, state)
        in
        loop store state
    | AilSlabel (_label, stmt, _annot) ->
        (* TODO: keep track of labels in a record or something!! *)
        exec_stmt ~prog store state stmt
    | AilSdeclaration decls ->
        let++ store, st =
          Csymex.Result.fold_list decls ~init:(store, state)
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
    let** val_opt, _, state = exec_stmt ~prog store state stmt in
    let++ state = dealloc_store store state in
    (* We model void as zero, it should never be used anyway *)
    let value = Option.value ~default:0s val_opt in
    (value, state)
end
