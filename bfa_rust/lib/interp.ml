open Bfa_symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
module Utils_ = Utils
open Charon
open Charon_util
module T = Typed.T

module Make (Heap : Heap_intf.S) = struct
  exception Unsupported of (string * Meta.span)

  type state = Heap.t
  type store = (T.sptr Typed.t option * Types.ty) Store.t

  let cast_checked ~ty x =
    match Typed.cast_checked x ty with
    | Some x -> Rustsymex.return x
    | None ->
        Fmt.kstr Rustsymex.not_impl "Failed to cast %a to %a" Typed.ppa x
          Typed.ppa_ty ty

  type 'err fun_exec =
    prog:UllbcAst.fun_decl ->
    args:T.cval Typed.t list ->
    state:state ->
    (T.cval Typed.t * state, 'err, Heap.serialized list) Result.t

  let get_param_tys ~(fn : UllbcAst.fun_decl) = fn.signature.inputs

  let alloc_params params st =
    Rustsymex.Result.fold_list params ~init:(Store.empty, st)
      ~f:(fun (store, st) (pname, ty, value) ->
        let** ptr, st = Heap.alloc_ty ty st in
        let store = Store.add pname (Some ptr, ty) store in
        let++ (), st = Heap.store ptr ty value st in
        (store, st))

  let dealloc_store store st =
    Rustsymex.Result.fold_list (Store.bindings store) ~init:st
      ~f:(fun st (_, (ptr, _)) ->
        match ptr with
        | None -> Result.ok st
        | Some ptr ->
            let++ (), st = Heap.free ptr st in
            st)

  let debug_show ~prog:_ ~args:_ ~state =
    let loc = get_loc () in
    let str = (Fmt.to_to_string (Heap.pp_pretty ~ignore_freed:false)) state in
    Rustsymex.push_give_up (str, loc);
    Result.ok (0s, state)

  let find_stub ~prog:_ (fname : Types.name) : 'err fun_exec option =
    let name = List.hd @@ List.rev fname in
    match name with PeIdent (_name, _) -> None | _ -> None

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

  let rec eval_operand ~prog:_prog ~store:_store state
      (op : Expressions.operand) =
    match op with
    | Constant c ->
        let+ v = value_of_constant c in
        (v, state)
    | Copy _ | Move _ ->
        Fmt.kstr not_impl "Unsupported operand: %a" Expressions.pp_operand op

  and eval_rvalue ~prog ~(store : store) (state : state)
      (expr : Expressions.rvalue) =
    let eval_operand = eval_operand ~prog ~store in
    match expr with
    | Use op ->
        let+ v, state = eval_operand state op in
        Ok (v, state)
    | UnaryOp (op, e) -> (
        let* _v, _state = eval_operand state e in
        match op with
        | _ ->
            Fmt.kstr not_impl "Unsupported unary operator %a"
              Expressions.pp_unop op)
    | BinaryOp (op, e1, e2) -> (
        (* TODO: Binary operators should return a cval, right now this is not right, I need to model integers *)
        let* v1, state = eval_operand state e1 in
        let* v2, state = eval_operand state e2 in
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
        | Div -> (
            let* v1 = cast_checked v1 ~ty:Typed.t_int in
            let* v2 = cast_checked v2 ~ty:Typed.t_int in
            let* v2 = Rustsymex.check_nonzero v2 in
            match v2 with
            | Ok v2 -> Rustsymex.Result.ok (v1 /@ v2, state)
            | Error `NonZeroIsZero -> Heap.error `DivisionByZero state
            | Missing e -> (* Unreachable but still *) Rustsymex.Result.miss e)
        | Mul -> arith_mul ~state v1 v2
        | bop ->
            Fmt.kstr not_impl "Unsupported binary operator: %a"
              Expressions.pp_binop bop)
    | v -> Fmt.kstr not_impl "Unsupported rvalue: %a" Expressions.pp_rvalue v

  and eval_rvalue_list ~prog ~(store : store) (state : state) el =
    let++ vs, state =
      Rustsymex.Result.fold_list el ~init:([], state) ~f:(fun (acc, state) e ->
          let++ new_res, state = eval_rvalue ~prog ~store state e in
          (new_res :: acc, state))
    in
    (List.rev vs, state)

  (** Executing a statement returns an optional value outcome (if a return
      statement was hit), or *)
  and exec_stmt ~prog (store : store) (state : state)
      (astmt : UllbcAst.statement) :
      ( T.cval Typed.t option * store * state,
        'err,
        Heap.serialized list )
      Rustsymex.Result.t =
    L.debug (fun m -> m "Executing statement: %a" UllbcAst.pp_statement astmt);
    let* () = Rustsymex.consume_fuel_steps 1 in
    let { span = loc; content = stmt; _ } : UllbcAst.statement = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | Nop -> Result.ok (None, store, state)
    | Assign ({ ty; kind = PlaceBase var }, rval) ->
        let** ptr, state = Heap.alloc_ty ty state in
        let** v, state = eval_rvalue ~prog ~store state rval in
        let++ (), state = Heap.store ptr ty v state in
        let store = Store.add var (Some ptr, ty) store in
        (None, store, state)
    | s ->
        Fmt.kstr not_impl "Unsupported statement: %a" UllbcAst.pp_raw_statement
          s

  and exec_fun ~prog ~args ~state (fundef : UllbcAst.fun_decl) =
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let** body =
      match body with
      | None -> Fmt.kstr not_impl "Function %a is opaque" Types.pp_name name
      | Some body -> Result.ok body
    in
    let@ () = with_loc ~loc in
    let ctx = PrintUllbcAst.Crate.crate_to_fmt_env prog in
    L.info (fun m ->
        m "Executing function %s" (PrintTypes.name_to_string ctx name));
    let ptys = get_param_tys ~fn:fundef in
    let params = List.map (fun (v : GAst.var) -> v.index) body.locals.vars in
    let ps = Utils_.List_ex.combine3 params ptys args in
    (* TODO: Introduce a with_stack_allocation.
           That would require some kind of continutation passing for executing a bunch of statements. *)
    let** store, state = alloc_params ps state in
    (* TODO: local optimisation to put values in store directly when no address is taken. *)
    let stts =
      List.concat_map (fun (b : UllbcAst.block) -> b.statements) body.body
    in
    let** val_opt, _, state =
      Rustsymex.Result.fold_list stts ~init:(None, store, state)
        ~f:(fun (res, store, state) stmt ->
          match res with
          | Some _ -> Rustsymex.Result.ok (res, store, state)
          | None -> exec_stmt ~prog store state stmt)
    in
    let++ state = dealloc_store store state in
    (* We model void as zero, it should never be used anyway *)
    let value = Option.value ~default:0s val_opt in
    (value, state)
end
