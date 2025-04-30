open Soteria_symex.Compo_res
open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
open Ail_tys
module Ctype = Cerb_frontend.Ctype
module AilSyntax = Cerb_frontend.AilSyntax
module T = Typed.T

(* It's a good occasion to play with effects: we have a global immutable state,
   and want to avoid passing it to every single function in the interpreter.
   TODO: If this works well, do the same thing for prog.
   *)
type _ Effect.t += Get_fun_ctx : Fun_ctx.t Effect.t

let get_fun_ctx () = Effect.perform Get_fun_ctx

module Make (State : State_intf.S) = struct
  module C_std = C_std.M (State)

  exception Unsupported of (string * Cerb_location.t)

  let type_of expr = Cerb_frontend.Translation_aux.ctype_of expr
  let unwrap_ctype (Ctype.Ctype (_, ty)) = ty

  let pointer_inner (Ctype.Ctype (_, ty)) =
    match ty with Pointer (_, ty) -> Some ty | _ -> None

  type state = State.t
  type store = (T.sptr Typed.t option * Ctype.ctype) Store.t

  let cast_to_ptr x =
    let open Csymex.Syntax in
    let open Typed.Infix in
    match Typed.get_ty x with
    | TInt ->
        (* TODO: Should we just make a pointer with null loc, but any offset? *)
        if%sat x ==@ Typed.zero then Csymex.return Typed.Ptr.null
        else
          Fmt.kstr Csymex.not_impl "Int-to-pointer that is not 0: %a" Typed.ppa
            x
    | TPointer ->
        let x : T.sptr Typed.t = Typed.cast x in
        Csymex.return x
    | _ -> Fmt.kstr Csymex.not_impl "Not a pointer: %a" Typed.ppa x

  let cast_to_int (x : [< T.cval ] Typed.t) : [> T.sint ] Typed.t Csymex.t =
    match Typed.get_ty x with
    | TInt -> Csymex.return (Typed.cast x)
    | TPointer ->
        let x = Typed.cast x in
        if%sat Typed.Ptr.is_at_null_loc x then Csymex.return (Typed.Ptr.ofs x)
        else not_impl "Pointer to int that is not at null loc"
    | _ -> failwith "cast_to_int a cval?"

  let cast_to_bool (x : [< T.cval ] Typed.t) : [> T.sbool ] Typed.t =
    let open Typed in
    match get_ty x with
    | TInt -> bool_of_int (cast x)
    | TPointer -> not (Ptr.is_null (cast x))
    | _ -> failwith "unreachable"

  type 'err fun_exec =
    prog:linked_program ->
    args:T.cval Typed.t list ->
    state:state ->
    (T.cval Typed.t * state, 'err, State.serialized list) Result.t

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
      ~f:(fun state (pname, ((loc, _, _), _, _, _)) ->
        let@ () = with_loc_immediate ~loc in
        match Store.find_value pname store with
        | Some ptr ->
            let++ (), state = State.free ptr state in
            state
        | None -> Result.ok state)

  let alloc_params params st =
    Csymex.Result.fold_list params ~init:(Store.empty, st)
      ~f:(fun (store, st) (pname, ty, value) ->
        L.trace (fun m -> m "Allocating variable %a" Fmt_ail.pp_sym pname);
        let** ptr, st = State.alloc_ty ty st in
        let store = Store.add pname (Some ptr, ty) store in
        let++ (), st = State.store ptr ty value st in
        (store, st))

  let dealloc_store store st =
    Csymex.Result.fold_list (Store.bindings store) ~init:st
      ~f:(fun st (_, (ptr, _)) ->
        match ptr with
        | None -> Result.ok st
        | Some ptr ->
            let++ (), st = State.free ptr st in
            st)

  let value_of_constant (c : constant) : T.cval Typed.t Csymex.t =
    match c with
    | ConstantInteger (IConstant (z, _basis, _suff)) ->
        Csymex.return (Typed.int_z z)
    | ConstantNull -> Csymex.return Typed.Ptr.null
    | _ ->
        Fmt.kstr Csymex.not_impl "value of constant? %a" Fmt_ail.pp_constant c

  let debug_show ~prog:_ ~args:_ ~state =
    let loc = get_loc () in
    let str = (Fmt.to_to_string (State.pp_pretty ~ignore_freed:false)) state in
    Csymex.push_give_up (str, loc);
    Result.ok (0s, state)

  let unwrap_expr (AnnotatedExpression (_, _, _, e) : expr) = e

  let find_stub ~prog:_ (fname : Cerb_frontend.Symbol.sym) :
      'err fun_exec option =
    let (Symbol (_, _, descr)) = fname in
    match descr with
    | Cerb_frontend.Symbol.SD_Id name -> (
        match name with
        | "__nondet__" -> Some C_std.nondet_int_fun
        | "malloc" -> Some C_std.malloc
        | "calloc" -> Some C_std.calloc
        | "free" -> Some C_std.free
        | "memcpy" -> Some C_std.memcpy
        | "__assert__" -> Some C_std.assert_
        | "__soteria_debug_show" -> Some debug_show
        | _ -> None)
    | _ -> None

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
        else State.error `UBPointerComparison state
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
    | TPointer, TPointer -> State.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
          Typed.ppa v2

  let arith_sub ~state (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TInt, TInt ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        Result.ok (v1 -@ v2, state)
    | TPointer, TInt ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs = Typed.Ptr.ofs v1 -@ v2 in
        Result.ok (Typed.Ptr.mk loc ofs, state)
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
    | TPointer, _ | _, TPointer -> State.error `UBPointerArithmetic state
    | _ ->
        Fmt.kstr not_impl "Unexpected types in multiplication: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let arith ~state (v1, t1) a_op (v2, t2) =
    match (a_op : AilSyntax.arithmeticOperator) with
    | Div -> (
        let* v1 = cast_to_int v1 in
        let* v2 = cast_to_int v2 in
        let* v2 = Csymex.check_nonzero v2 in
        match v2 with
        | Ok v2 -> Csymex.Result.ok (v1 /@ v2, state)
        | Error `NonZeroIsZero -> State.error `DivisionByZero state
        | Missing e -> (* Unreachable but still *) Csymex.Result.miss e)
    | Mul -> arith_mul ~state v1 v2
    | Add -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | Some _, Some _ -> State.error `UBPointerArithmetic state
        | Some ty, None ->
            let* factor = Layout.size_of_s ty in
            let** v2, state = arith_mul ~state v2 factor in
            arith_add ~state v1 v2
        | None, Some ty ->
            let* factor = Layout.size_of_s ty in
            let** v1, state = arith_mul ~state v1 factor in
            arith_add ~state v2 v1
        | None, None -> arith_add ~state v1 v2)
    | Sub -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | _, Some _ -> State.error `UBPointerArithmetic state
        | Some ty, None ->
            let* factor = Layout.size_of_s ty in
            let** v2, state = arith_mul ~state v2 factor in
            arith_sub ~state v1 v2
        | None, None -> arith_sub ~state v1 v2)
    | _ ->
        Fmt.kstr not_impl "Unsupported arithmetic operator: %a"
          Fmt_ail.pp_arithop a_op

  (* We do this in the untyped world *)
  let ineq_comparison ~state ~cmp_op left right =
    let cmp_op left right = cmp_op left right |> Typed.int_of_bool in
    match (Typed.get_ty left, Typed.get_ty right) with
    | TInt, TInt ->
        let left = Typed.cast left in
        let right = Typed.cast right in
        Result.ok (cmp_op left right, state)
    | TPointer, TPointer ->
        let left = Typed.cast left in
        let right = Typed.cast right in
        if%sat Typed.Ptr.loc left ==@ Typed.Ptr.loc right then
          Csymex.Result.ok
            (cmp_op (Typed.Ptr.ofs left) (Typed.Ptr.ofs right), state)
        else State.error `UBPointerComparison state
    | _ -> State.error `UBPointerComparison state

  let rec resolve_function ~(prog : linked_program) ~store state fexpr =
    let** (loc, fname), state =
      let (AilSyntax.AnnotatedExpression (_, _, loc, inner_expr)) = fexpr in
      match inner_expr with
      (* Special case when we can immediately resolve the function name *)
      | AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname)) ->
          Csymex.Result.ok ((loc, fname), state)
      | _ ->
          (* Some function pointer *)
          let** fptr, state = eval_expr ~prog ~store state fexpr in
          let* fptr = cast_to_ptr fptr in
          if%sat
            Typed.not (Typed.Ptr.ofs fptr ==@ 0s)
            ||@ Typed.Ptr.is_at_null_loc fptr
          then State.error `InvalidFunctionPtr state
          else
            let fctx = get_fun_ctx () in
            let* sym = Fun_ctx.get_sym (Typed.Ptr.loc fptr) fctx in
            Csymex.Result.ok ((loc, sym), state)
    in
    let@ () = with_loc ~loc in
    let fundef_opt = Ail_helpers.find_fun_def ~prog fname in
    match fundef_opt with
    | Some fundef -> Csymex.Result.ok (exec_fun fundef, state)
    | None -> (
        match find_stub ~prog fname with
        | Some stub -> Csymex.Result.ok (stub, state)
        | None ->
            Fmt.kstr not_impl "Cannot call external function: %a" Fmt_ail.pp_sym
              fname)

  and eval_expr_list ~(prog : linked_program) ~(store : store) (state : state)
      (el : expr list) =
    let++ vs, state =
      Csymex.Result.fold_list el ~init:([], state) ~f:(fun (acc, state) e ->
          let++ new_res, state = eval_expr ~prog ~store state e in
          (new_res :: acc, state))
    in
    (List.rev vs, state)

  and eval_expr ~(prog : linked_program) ~(store : store) (state : state)
      (aexpr : expr) =
    let eval_expr = eval_expr ~prog ~store in
    let (AnnotatedExpression (_, _, loc, expr)) = aexpr in
    let@ () = with_loc ~loc in
    match expr with
    | AilEconst c ->
        let+ v = value_of_constant c in
        Ok (v, state)
    | AilEcall (f, args) ->
        let** exec_fun, state = resolve_function ~prog ~store state f in
        let** args, state = eval_expr_list ~prog ~store state args in
        let++ v, state =
          let+- err = exec_fun ~prog ~args ~state in
          State.add_to_call_trace err
            (Call_trace.make_element ~loc ~msg:"Call trace" ())
        in
        L.debug (fun m -> m "returned %a from %a" Typed.ppa v Fmt_ail.pp_expr f);
        (v, state)
    | AilEunary (Address, e) -> (
        match unwrap_expr e with
        | AilEunary (Indirection, e) -> (* &*e <=> e *) eval_expr state e
        | AilEident id -> (
            let id = Ail_helpers.resolve_sym ~prog id in
            match Store.find_value id store with
            | Some ptr -> Result.ok ((ptr :> T.cval Typed.t), state)
            | None ->
                let+ ptr, state = State.get_global id state in
                Ok (ptr, state))
        | _ -> Fmt.kstr not_impl "Unsupported address_of: %a" Fmt_ail.pp_expr e)
    | AilEunary (op, e) -> (
        let** v, state = eval_expr state e in
        match op with
        | PostfixIncr ->
            let* ptr = cast_to_ptr v in
            let** v, state = State.load ptr (type_of e) state in
            let* incr_operand =
              match type_of e |> pointer_inner with
              | Some ty -> Layout.size_of_s ty
              | None -> return 1s
            in
            let** v_incr, state = arith_add ~state v incr_operand in
            let++ (), state = State.store ptr (type_of e) v_incr state in
            (v, state)
        | PostfixDecr ->
            let* ptr = cast_to_ptr v in
            let** v, state = State.load ptr (type_of e) state in
            let* incr_operand =
              match type_of e |> pointer_inner with
              | Some ty -> Layout.size_of_s ty
              | None -> return 1s
            in
            let** v_decr, state = arith_sub ~state v incr_operand in
            let++ (), state = State.store ptr (type_of e) v_decr state in
            (v, state)
        | Indirection -> Result.ok (v, state)
        | Address -> failwith "unreachable: address_of already handled"
        | Minus ->
            let* v = cast_to_int v in
            arith_sub ~state Typed.zero v
        | _ ->
            Fmt.kstr not_impl "Unsupported unary operator %a" Fmt_ail.pp_unop op
        )
    | AilEbinary (e1, op, e2) -> (
        (* TODO: Binary operators should return a cval, right now this is not right, I need to model integers *)
        let** v1, state = eval_expr state e1 in
        let** v2, state = eval_expr state e2 in
        match op with
        | Ge -> ineq_comparison ~state ~cmp_op:( >=@ ) v1 v2
        | Gt -> ineq_comparison ~state ~cmp_op:( >@ ) v1 v2
        | Lt -> ineq_comparison ~state ~cmp_op:( <@ ) v1 v2
        | Le -> ineq_comparison ~state ~cmp_op:( <=@ ) v1 v2
        | Eq -> equality_check ~state v1 v2
        | Ne ->
            (* TODO: Semantics of Ne might be different from semantics of not eq? *)
            let++ res, state = equality_check ~state v1 v2 in
            (Typed.not_int_bool res, state)
        | And ->
            let b_res = cast_to_bool v1 &&@ cast_to_bool v2 in
            Result.ok (Typed.int_of_bool b_res, state)
        | Or ->
            let b_res = cast_to_bool v1 ||@ cast_to_bool v2 in
            Result.ok (Typed.int_of_bool b_res, state)
        | Arithmetic a_op -> arith ~state (v1, type_of e1) a_op (v2, type_of e2)
        | _ ->
            Fmt.kstr not_impl "Unsupported binary operator: %a" Fmt_ail.pp_binop
              op)
    | AilErvalue e ->
        let** lvalue, state = eval_expr state e in
        let ty = type_of e in
        (* At this point, lvalue must be a pointer (including to the stack) *)
        let* lvalue = cast_to_ptr lvalue in
        State.load lvalue ty state
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym ~prog id in
        match Store.find_value id store with
        | Some v ->
            (* A pointer is a value *)
            let v = (v :> T.cval Typed.t) in
            Result.ok (v, state)
        | None ->
            (* If the variable isn't in the store, it must be a global variable. *)
            let+ ptr, state = State.get_global id state in
            Ok (ptr, state))
    | AilEassign (lvalue, rvalue) ->
        (* Evaluate rvalue first *)
        let** rval, state = eval_expr state rvalue in
        let** ptr, state = eval_expr state lvalue in
        (* [ptr] is a necessarily a pointer, and [rval] is a memory value.
         I don't support pointer fragments for now, so let's say it's an *)
        let* ptr = cast_to_ptr ptr in
        let ty = type_of lvalue in
        let++ (), state = State.store ptr ty rval state in
        (rval, state)
    | AilEcompoundAssign (lvalue, op, rvalue) ->
        let** rval, state = eval_expr state rvalue in
        let** ptr, state = eval_expr state lvalue in
        let lty = type_of lvalue in
        (* At this point, lvalue must be a pointer (including to the stack) *)
        let* ptr = cast_to_ptr ptr in
        let** operand, state = State.load ptr lty state in
        let** res, state =
          arith ~state (operand, lty) op (rval, type_of rvalue)
        in
        let++ (), state = State.store ptr lty res state in
        (res, state)
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
    | AilSyntax.AilEfunction_decay
        (AnnotatedExpression (_, _, _, fexpr) as outer_fexpr) -> (
        match fexpr with
        | AilEident id ->
            let id = Ail_helpers.resolve_sym ~prog id in
            let ctx = get_fun_ctx () in
            let+ floc = Fun_ctx.decay_fn_sym id ctx in
            Soteria_symex.Compo_res.Ok (Typed.Ptr.mk floc 0s, state)
        | _ ->
            Fmt.kstr not_impl "Unsupported function decay: %a" Fmt_ail.pp_expr
              outer_fexpr)
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
    | AilSyntax.AilEarray_decay _ | AilSyntax.AilEatomic _
    | AilSyntax.AilEgcc_statement (_, _) ->
        Fmt.kstr not_impl "Unsupported expr: %a" Fmt_ail.pp_expr aexpr

  (** Executing a statement returns an optional value outcome (if a return
      statement was hit), or *)
  and exec_stmt ~prog (store : store) (state : state) (astmt : stmt) :
      ( T.cval Typed.t option * store * state,
        'err,
        State.serialized list )
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
    | AilSreturnVoid -> Result.ok (Some 0s, store, state)
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
        let* v = cast_to_int v in
        if%sat Typed.bool_of_int v then
          exec_stmt ~prog store state then_stmt [@name "if branch"]
        else exec_stmt ~prog store state else_stmt [@name "else branch"]
    | AilSwhile (cond, stmt, _loopid) ->
        let rec loop store state =
          let** cond_v, state = eval_expr ~prog ~store state cond in
          if%sat cast_to_bool cond_v then
            let** res, store, state = exec_stmt ~prog store state stmt in
            match res with
            | Some _ -> Result.ok (res, store, state)
            | None -> loop store state
          else Result.ok (None, store, state)
        in
        loop store state
    | AilSdo (stmt, cond, _loop_id) ->
        let rec loop store state =
          let** res, store, state = exec_stmt ~prog store state stmt in
          match res with
          | Some _ -> Result.ok (res, store, state)
          | None ->
              let** cond_v, state = eval_expr ~prog ~store state cond in
              if%sat cast_to_bool cond_v then loop store state
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
              let** ptr, state = State.alloc_ty ty state in
              let++ (), state =
                match expr with
                | None -> Result.ok ((), state)
                | Some expr ->
                    let** v, state = eval_expr ~prog ~store state expr in
                    State.store ptr ty v state
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
    L.debug (fun m -> m "Executing function %a" Fmt_ail.pp_sym name);
    let* ptys = get_param_tys ~prog name in
    let ps = List.combine3 params ptys args in
    (* TODO: Introduce a with_stack_allocation.
           That would require some kind of continutation passing for executing a bunch of statements. *)
    let** store, state = alloc_params ps state in
    (* TODO: local optimisation to put values in store directly when no address is taken. *)
    let** val_opt, _, state = exec_stmt ~prog store state stmt in
    let++ state = dealloc_store store state in
    (* We model void as zero, it should never be used anyway *)
    let value = Option.value ~default:0s val_opt in
    (value, state)

  let init_prog_state (prog : Ail_tys.linked_program) =
    (* Produce_zero will be useful when Kayvan allows for knowing when no declaration is given. *)
    let _produce_zero (ptr : [< T.sptr ] Typed.t) ty (state : State.t) =
      let loc = Typed.Ptr.loc ptr in
      let offset = Typed.Ptr.ofs ptr in
      let* len = Layout.size_of_s ty in
      let serialized : State.serialized =
        {
          heap = [ (loc, Freeable.Alive [ Tree_block.Zeros { offset; len } ]) ];
          globs = [];
        }
      in
      let+ state = State.produce serialized state in
      Soteria_symex.Compo_res.ok state
    in
    let produce_value (ptr : [< T.sptr ] Typed.t) ty expr (state : State.t) =
      let loc = Typed.Ptr.loc ptr in
      let offset = Typed.Ptr.ofs ptr in
      (* I somehow have to support global initialisation urgh.
       I might be able to extract some of that into interp *)
      let** v, state = eval_expr ~prog ~store:Store.empty state expr in
      let serialized : State.serialized =
        {
          heap =
            [ (loc, Freeable.Alive [ Tree_block.TypedVal { offset; ty; v } ]) ];
          globs = [];
        }
      in
      let+ state = State.produce serialized state in
      Soteria_symex.Compo_res.ok state
    in
    Csymex.Result.fold_list prog.sigma.object_definitions ~init:State.empty
      ~f:(fun (state : State.t) def ->
        let id, e = def in
        let* ty =
          match Ail_helpers.find_obj_decl ~prog id with
          | None -> Csymex.not_impl "Couldn't find object declaration"
          | Some (_, _, _, ty) -> Csymex.return ty
        in
        (* TODO: handle other parameters here *)
        let* ptr, state = State.get_global id state in
        produce_value ptr ty e state)
end
