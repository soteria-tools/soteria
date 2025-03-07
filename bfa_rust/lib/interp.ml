open Bfa_symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
module Utils_ = Utils
open Charon
open Charon_util
module T = Typed.T

type termination = RetVal of T.cval Typed.t | GoToBlock of UllbcAst.block_id

module Make (Heap : Heap_intf.S) = struct
  module Std = Rust_std.M (Heap)

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
    crate:UllbcAst.crate ->
    args:rust_val list ->
    state:state ->
    (rust_val * state, 'err, Heap.serialized list) Result.t

  let alloc_stack (locals : GAst.locals) args st =
    if List.length args <> locals.arg_count then
      Fmt.failwith "Function expects %d arguments, but got %d" locals.arg_count
        (List.length args);
    Rustsymex.Result.fold_list locals.vars ~init:(Store.empty, st)
      ~f:(fun (store, st) { index; var_ty = ty; _ } ->
        let** ptr, st = Heap.alloc_ty ty st in
        let store = Store.add index (Some ptr, ty) store in
        let index = Expressions.VarId.to_int index in
        if 0 < index && index <= locals.arg_count then
          let value = List.nth args (index - 1) in
          let++ (), st = Heap.store ptr ty value st in
          (store, st)
        else Result.ok (store, st))

  let dealloc_store store st =
    Rustsymex.Result.fold_list (Store.bindings store) ~init:st
      ~f:(fun st (_, (ptr, _)) ->
        match ptr with
        | None -> Result.ok st
        | Some ptr ->
            let++ (), st = Heap.free ptr st in
            st)

  let debug_show ~crate:_ ~args:_ ~state =
    let loc = get_loc () in
    let str = (Fmt.to_to_string (Heap.pp_pretty ~ignore_freed:false)) state in
    Rustsymex.push_give_up (str, loc);
    Result.ok (0s, state)

  let rec resolve_place ~store state ({ kind; _ } : Expressions.place) =
    match kind with
    (* Just a variable *)
    | PlaceBase var -> (
        let ptr = Store.find_value var store in
        match ptr with
        | Some ptr ->
            L.debug (fun f ->
                f "Found pointer %a of variable %a" Typed.ppa ptr
                  Expressions.pp_var_id var);
            Result.ok (ptr, state)
        | None ->
            Fmt.kstr not_impl "Variable %a not found in store"
              Expressions.pp_var_id var)
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) ->
        let** ptr, state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" Typed.ppa ptr Types.pp_ty base.ty);
        let** v, state = Heap.load ptr base.ty state in
        let* v' = rustval_as_ptr v in
        L.debug (fun f ->
            f "Dereferenced pointer %a to pointer %a" Typed.ppa ptr Typed.ppa v');
        Result.ok (v', state)
    | PlaceProjection (base, Field (kind, field)) ->
        let** ptr, state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Projecting field %a (kind %a) for %a" Types.pp_field_id field
              Expressions.pp_field_proj_kind kind Typed.ppa ptr);
        let field = Types.FieldId.to_int field in
        let layout, field =
          match kind with
          | ProjAdt (adt_id, Some variant) ->
              (* Skip discriminator, so field + 1 *)
              (Layout.of_enum_variant adt_id variant, field + 1)
          | ProjAdt (adt_id, None) -> (Layout.of_adt_id adt_id, field)
          | ProjTuple _arity -> (Layout.layout_of base.ty, field)
        in
        let offset = Array.get layout.members_ofs field in
        let loc = Typed.Ptr.loc ptr in
        let ofs = Typed.Ptr.ofs ptr +@ Typed.int offset in
        let ptr = Typed.Ptr.mk loc ofs in
        L.debug (fun f ->
            f
              "Dereferenced ADT projection %a, field %d, with pointer %a to \
               pointer %a"
              Expressions.pp_field_proj_kind kind field Typed.ppa ptr Typed.ppa
              ptr);
        Result.ok (ptr, state)

  let overflow_check state v (ty : Types.ty) =
    let* constraints =
      match ty with
      | TLiteral ty ->
          Rustsymex.of_opt_not_impl ~msg:"Constraints for overflow check"
            (Layout.constraints ty)
      | ty ->
          Fmt.kstr Rustsymex.not_impl "Unexpected type for overflow check: %a"
            Types.pp_ty ty
    in
    if%sat Typed.conj @@ constraints v then Rustsymex.Result.ok (v, state)
    else Heap.error `Overflow state

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

  let rec resolve_function ~(crate : UllbcAst.crate) (fnop : GAst.fn_operand) :
      'err fun_exec Rustsymex.t =
    let* fid =
      match fnop with
      | FnOpRegular
          {
            func = FunId (FRegular fid);
            generics =
              { regions = []; types = []; const_generics = []; trait_refs = [] };
          } ->
          Rustsymex.return fid
      | FnOpRegular { func = FunId (FBuiltin _); _ } ->
          Fmt.kstr not_impl "Builtin function call is not supported: %a"
            GAst.pp_fn_operand fnop
      | FnOpRegular { func = FunId _; _ } ->
          Fmt.kstr not_impl "Generic function call is not supported: %a"
            GAst.pp_fn_operand fnop
      | FnOpRegular { func = TraitMethod (_, _, fid); _ } ->
          Rustsymex.return fid
      | FnOpMove _ ->
          Fmt.kstr not_impl "Move function call is not supported: %a"
            GAst.pp_fn_operand fnop
    in
    let fundef_opt = Expressions.FunDeclId.Map.find_opt fid crate.fun_decls in
    match fundef_opt with
    | Some fundef -> (
        match Std.std_fun_eval ~crate fundef with
        | Some fn -> Rustsymex.return fn
        | None -> Rustsymex.return (exec_fun fundef))
    | None ->
        Fmt.kstr not_impl "Cannot call external function: %a"
          Expressions.FunDeclId.pp_id fid

  and eval_operand ~crate:_ ~store state (op : Expressions.operand) =
    match op with
    | Constant c ->
        let v = value_of_constant c in
        Result.ok (Base v, state)
    | Move loc ->
        let ty = loc.ty in
        let** loc, state = resolve_place ~store state loc in
        let** v, state = Heap.load loc ty state in
        (* TODO: mark value as moved!!! !== freeing it, btw *)
        Result.ok (v, state)
    | Copy loc ->
        let ty = loc.ty in
        let** loc, state = resolve_place ~store state loc in
        let** v, state = Heap.load loc ty state in
        Result.ok (v, state)

  and eval_operand_list ~crate ~store state ops =
    let++ vs, state =
      Result.fold_list ops ~init:([], state) ~f:(fun (acc, state) op ->
          let++ new_res, state = eval_operand ~crate ~store state op in
          (new_res :: acc, state))
    in
    (List.rev vs, state)

  and eval_rvalue ~crate ~store state (expr : Expressions.rvalue) =
    let eval_operand = eval_operand ~crate ~store in
    match expr with
    | Use op -> eval_operand state op
    | RvRef (place, _borrow) ->
        let** ptr, state = resolve_place ~store state place in
        Result.ok (Base (ptr :> T.cval Typed.t), state)
    | Global { global_id; _ } -> (
        let decl =
          UllbcAst.GlobalDeclId.Map.find global_id UllbcAst.(crate.global_decls)
        in
        match Std.global_eval ~crate decl with
        | None ->
            Fmt.kstr not_impl "Global %a not found" GAst.pp_global_decl decl
        | Some global -> Result.ok (global, state))
    | UnaryOp (op, e) -> (
        let** v, _state = eval_operand state e in
        match op with
        | Not ->
            let* v_int =
              match v with
              | Base v ->
                  of_opt_not_impl ~msg:"Not requires int argument"
                    (Typed.cast_checked v Typed.t_int)
              | _ -> not_impl "Not requires a Base argument"
            in
            let v_int' =
              v_int |> Typed.bool_of_int |> Typed.not |> Typed.int_of_bool
            in
            let v' = Base v_int' in
            Result.ok (v', state)
        | _ ->
            Fmt.kstr not_impl "Unsupported unary operator %a"
              Expressions.pp_unop op)
    | BinaryOp (op, e1, e2) ->
        (* TODO: Binary operators should return a cval, right now this is not right, I need to model integers *)
        let** v1, state = eval_operand state e1 in
        let** v2, state = eval_operand state e2 in
        let v1, v2 =
          match (v1, v2) with
          | Base v1, Base v2 -> (v1, v2)
          | _, _ ->
              Fmt.failwith "Expected base values in BinaryOp: %a/%a" pp_rust_val
                v1 pp_rust_val v2
        in
        let++ res, state =
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
          | Eq ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              equality_check ~state v1 v2
          | Ne ->
              (* TODO: Semantics of Ne might be different from semantics of not eq? *)
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              let++ res, state = equality_check ~state v1 v2 in
              (Typed.not_int_bool res, state)
          | Div -> (
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              let* v2 = Rustsymex.check_nonzero v2 in
              match v2 with
              | Ok v2 -> overflow_check state (v1 /@ v2) (type_of_operand e1)
              | Error `NonZeroIsZero -> Heap.error `DivisionByZero state
              | Missing e -> (* Unreachable but still *) Rustsymex.Result.miss e
              )
          | Rem -> (
              let* v1 = cast_checked v1 ~ty:Typed.t_int in
              let* v2 = cast_checked v2 ~ty:Typed.t_int in
              let* v2 = Rustsymex.check_nonzero v2 in
              match v2 with
              | Ok v2 -> (
                  match type_of_operand e1 with
                  | TLiteral (TInteger ty) ->
                      let min_val = Layout.min_value ty in
                      (* Overflow if left is MIN and right is -1 *)
                      if%sat
                        v1
                        ==@ min_val
                        &&@ ((v2 :> T.sint Typed.t) ==@ Typed.int (-1))
                      then Heap.error `Overflow state
                      else Rustsymex.Result.ok (v1 %@ v2, state)
                  | ty ->
                      Fmt.kstr not_impl "Unsupported type for rem: %a"
                        Types.pp_ty ty)
              | Error `NonZeroIsZero -> Heap.error `DivisionByZero state
              | Missing e -> (* Unreachable but still *) Rustsymex.Result.miss e
              )
          | Mul ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              arith_mul ~state v1 v2
          | Add ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              arith_add ~state v1 v2
          | Sub ->
              let v1 = Typed.cast v1 in
              let v2 = Typed.cast v2 in
              arith_add ~state v1 (Typed.minus 0s v2)
          | bop ->
              Fmt.kstr not_impl "Unsupported binary operator: %a"
                Expressions.pp_binop bop
        in
        (Base res, state)
    | Discriminant (place, kind) ->
        let** place, state = resolve_place ~store state place in
        let enum = Types.TypeDeclId.Map.find kind UllbcAst.(crate.type_decls) in
        let* enum_discr_ty =
          match enum.kind with
          | Enum (var :: _) ->
              let int_ty = var.discriminant.int_ty in
              return (Types.TLiteral (TInteger int_ty))
          | Enum [] ->
              Fmt.kstr not_impl "Unsupported discriminant for empty enums"
          | k ->
              Fmt.failwith "Expected an enum for discriminant, got %a"
                Types.pp_type_decl_kind k
        in
        Heap.load place enum_discr_ty state
    (* Enum aggregate *)
    | Aggregate (AggregatedAdt (TAdtId t_id, Some v_id, None, _), vals) ->
        let type_decl =
          Types.TypeDeclId.Map.find t_id UllbcAst.(crate.type_decls)
        in
        let variant =
          match (type_decl : Types.type_decl) with
          | { kind = Enum variants; _ } -> Types.VariantId.nth variants v_id
          | _ ->
              Fmt.failwith "Unexpected type declaration in enum aggregate: %a"
                Types.pp_type_decl type_decl
        in
        let discr = value_of_scalar variant.discriminant in
        let++ vals, state = eval_operand_list ~crate ~store state vals in
        (Enum (discr, vals), state)
    (* Union aggregate *)
    | Aggregate (AggregatedAdt (_, None, Some _, _), _) as v ->
        Fmt.kstr not_impl "Union rvalues not supported: %a"
          Expressions.pp_rvalue v
    (* Special case? unit (zero-tuple) *)
    | Aggregate (AggregatedAdt (TTuple, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Charon_util.Tuple values, state)
    (* Struct aggregate *)
    | Aggregate (AggregatedAdt (TAdtId _, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Charon_util.Struct values, state)
    (* Invalid aggregate (not sure, but seems like it) *)
    | Aggregate _ as v ->
        Fmt.failwith "Invalid aggregate rvalue: %a" Expressions.pp_rvalue v
    | v -> Fmt.kstr not_impl "Unsupported rvalue: %a" Expressions.pp_rvalue v

  and eval_rvalue_list ~crate ~(store : store) (state : state) el =
    let++ vs, state =
      Rustsymex.Result.fold_list el ~init:([], state) ~f:(fun (acc, state) e ->
          let++ new_res, state = eval_rvalue ~crate ~store state e in
          (new_res :: acc, state))
    in
    (List.rev vs, state)

  and exec_stmt ~crate store state astmt :
      (store * state, 'err, Heap.serialized list) Rustsymex.Result.t =
    L.info (fun m ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        m "Statement: %s" (PrintUllbcAst.Ast.statement_to_string ctx "" astmt));
    L.debug (fun m -> m "Statement full: %a" UllbcAst.pp_statement astmt);
    let* () = Rustsymex.consume_fuel_steps 1 in
    let { span = loc; content = stmt; _ } : UllbcAst.statement = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | Nop -> Result.ok (store, state)
    | Assign (({ ty; _ } as place), rval) ->
        let** ptr, state = resolve_place ~store state place in
        let** v, state = eval_rvalue ~crate ~store state rval in
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | Call { func; args; dest = { kind = PlaceBase var; ty } } ->
        let* exec_fun = resolve_function ~crate func in
        let** args, state = eval_operand_list ~crate ~store state args in
        let** v, state =
          let+- err = exec_fun ~crate ~args ~state in
          Heap.add_to_call_trace err
            (Call_trace.make_element ~loc ~msg:"Call trace" ())
        in
        L.debug (fun m ->
            m "Returned %a from %a" pp_rust_val v GAst.pp_fn_operand func);
        let ptr =
          match Store.find_value var store with
          | Some ptr -> ptr
          | None ->
              failwith "Tried storing in a variable that was not allocated"
        in
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | StorageDead var ->
        let* ptr, ty =
          (* TODO: maybe only mark as unusable and dont deallocate? maybe NOP? *)
          match Store.find_opt var store with
          | Some (Some ptr, ty) -> return (ptr, ty)
          | Some (None, _) ->
              Fmt.kstr not_impl
                "Variable %a already deallocated - UB? unreachable?"
                Expressions.pp_var_id var
          | None ->
              Fmt.kstr not_impl "Variable %a not found in store"
                Expressions.pp_var_id var
        in
        let++ (), state = Heap.free ptr state in
        let store = Store.add var (None, ty) store in
        (store, state)
    | FakeRead _ ->
        (* TODO: update tree borrow with read *)
        Result.ok (store, state)
    | Drop place ->
        (* TODO: this is probably super wrong, drop glue etc. *)
        let** place_ptr, state = resolve_place ~store state place in
        let++ (), state = Heap.uninit place_ptr state in
        (* let store =
          match place.kind with
          | PlaceBase var_id -> Store.add var_id (None, place.ty) store
          | _ -> store
        in *)
        (store, state)
    | Assert { cond; expected } ->
        let** cond, state = eval_operand ~crate ~store state cond in
        let* cond_int =
          match cond with
          | Base cond ->
              of_opt_not_impl ~msg:"Expected an integer assertion"
                (Typed.cast_checked cond Typed.t_int)
          | _ -> not_impl "Expected a base Rust value in assert"
        in
        let cond_bool = Typed.bool_of_int cond_int in
        let cond_bool =
          if expected = true then cond_bool else Typed.not cond_bool
        in
        if%sat cond_bool then Result.ok (store, state)
        else Heap.error `FailedAssert state
    | s ->
        Fmt.kstr not_impl "Unsupported statement: %a" UllbcAst.pp_raw_statement
          s

  and exec_block ~crate ~(body : UllbcAst.expr_body) store state
      ({ statements; terminator } : UllbcAst.block) =
    let** store, state =
      Rustsymex.Result.fold_list statements ~init:(store, state)
        ~f:(fun (store, state) stmt -> exec_stmt ~crate store state stmt)
    in
    L.debug (fun f ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        f "Terminator: %s"
          (PrintUllbcAst.Ast.terminator_to_string ctx "" terminator));
    let { span = loc; content = term; _ } : UllbcAst.terminator = terminator in
    let@ () = with_loc ~loc in
    match term with
    | Goto b ->
        let block = UllbcAst.BlockId.nth body.body b in
        exec_block ~crate ~body store state block
    | Return ->
        let value_ptr, value_ty = Store.find Expressions.VarId.zero store in
        let* value_ptr =
          match value_ptr with
          | Some x -> return x
          | None -> Fmt.kstr not_impl "Return value unset, but returned"
        in
        let++ value, _ = Heap.load value_ptr value_ty state in
        (value, store, state)
    | Switch (discr, switch) -> (
        let** discr, state = eval_operand ~crate ~store state discr in
        let discr =
          match discr with
          | Base discr -> discr
          | _ ->
              Fmt.failwith "Expected base value for discriminant, got %a"
                pp_rust_val discr
        in
        match switch with
        | If (if_block, else_block) ->
            let open Typed.Infix in
            L.info (fun g ->
                g "Switch if/else %a/%a for %a" UllbcAst.pp_block_id if_block
                  UllbcAst.pp_block_id else_block Typed.ppa discr);
            let* block =
              if%sat discr ==@ Typed.one then return if_block
              else return else_block
            in
            let block = UllbcAst.BlockId.nth body.body block in
            exec_block ~crate ~body store state block
        | SwitchInt (_, options, default) -> (
            let* block =
              Rustsymex.fold_list options ~init:None
                ~f:(fun block (test, if_equal) ->
                  match block with
                  | Some _ -> return block
                  | None ->
                      let test = Charon_util.value_of_scalar test in
                      if%sat discr ==@ test then return (Some if_equal)
                      else return None)
            in
            match block with
            | None ->
                let block = UllbcAst.BlockId.nth body.body default in
                exec_block ~crate ~body store state block
            | Some block ->
                let block = UllbcAst.BlockId.nth body.body block in
                exec_block ~crate ~body store state block))
    | Abort kind -> (
        match kind with
        | UndefinedBehavior -> Heap.error `UBAbort state
        | Panic name ->
            let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
            let name_str = PrintTypes.name_to_string ctx name in
            Heap.error (`Panic name_str) state)

  and exec_fun ~crate ~args ~state (fundef : UllbcAst.fun_decl) =
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let** body =
      match body with
      | None ->
          let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
          Fmt.kstr not_impl "Function %s is opaque"
            (PrintTypes.name_to_string ctx name)
      | Some body -> Result.ok body
    in
    let@ () = with_loc ~loc in
    L.info (fun m ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        m "Calling %s with @[%a@]"
          (PrintTypes.name_to_string ctx name)
          Fmt.(list ~sep:comma pp_rust_val)
          args);
    let** store, state = alloc_stack body.locals args state in
    (* TODO: local optimisation to put values in store directly when no address is taken. *)
    let starting_block = List.hd body.body in
    let** value, store, state =
      exec_block ~crate ~body store state starting_block
    in
    let++ state = dealloc_store store state in
    (* We model void as zero, it should never be used anyway *)
    (value, state)
end
