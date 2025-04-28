open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
open Charon
open Charon_util
module T = Typed.T

module Make (Heap : Heap_intf.S) = struct
  module Core = Core.M (Heap)
  module Std_funs = Std_funs.M (Heap)
  module Sptr = Heap.Sptr

  let pp_rust_val = pp_rust_val Sptr.pp

  exception Unsupported of (string * Meta.span)

  type full_ptr = Sptr.t Charon_util.full_ptr
  type state = Heap.t
  type store = (full_ptr option * Types.ty) Store.t

  let pp_full_ptr = Charon_util.pp_full_ptr Sptr.pp

  type ('err, 'fixes) fun_exec =
    crate:UllbcAst.crate ->
    args:Sptr.t rust_val list ->
    state:state ->
    (Sptr.t rust_val * state, 'err, 'fixes) Result.t

  let alloc_stack (locals : GAst.locals) args st :
      (store * full_ptr list * state, 'e, 'm) Result.t =
    if List.compare_length_with args locals.arg_count <> 0 then
      Fmt.failwith "Function expects %d arguments, but got %d" locals.arg_count
        (List.length args);
    (* create a store with all types *)
    let store =
      List.fold_left
        (fun st (local : GAst.local) ->
          Store.add local.index (None, local.var_ty) st)
        Store.empty locals.locals
    in
    (* allocate arguments and return value, updating store *)
    let alloc_locs = List.take (1 + locals.arg_count) locals.locals in
    let tys =
      List.map (fun ({ var_ty; _ } : GAst.local) -> var_ty) alloc_locs
    in
    let** ptrs, st = Heap.alloc_tys tys st in
    let tys_ptrs = List.combine alloc_locs ptrs in
    let store =
      List.fold_left
        (fun store ((local : GAst.local), ptr) ->
          Store.add local.index (Some ptr, local.var_ty) store)
        store tys_ptrs
    in
    (* store values for the arguments *)
    let tys_ptrs = List.tl tys_ptrs in
    let++ protected, st =
      Result.fold_list tys_ptrs ~init:([], st)
        ~f:(fun (protected, st) ({ index; var_ty = ty; _ }, ptr) ->
          let index = Expressions.LocalId.to_int index in
          let value = List.nth args (index - 1) in
          let** value, protected', st =
            match (value, ty) with
            | Ptr ptr, (TRawPtr (subty, mut) | TRef (_, subty, mut)) ->
                let** ptr', st = Heap.protect ptr mut st in
                (* Function calls perform a dummy read on the variable *)
                let++ _, st = Heap.load ptr' subty st in
                (Ptr ptr', ptr' :: protected, st)
            | _ -> Result.ok (value, protected, st)
          in
          let++ (), st = Heap.store ptr ty value st in
          (protected', st))
    in
    (store, protected, st)

  (** [dealloc_store ?protected_address store protected st] Deallocates the
      locations in [st] used for the variables in [store]; if
      [protected_address] is provided, will not deallocate that location (this
      is used e.g. for globals, that return a &'static reference). Will also
      remove the protectors from the pointers [protected] that were given at the
      function's entry. *)
  let dealloc_store ?protected_address store protected st =
    let** (), st =
      Result.fold_list protected ~init:((), st) ~f:(fun ((), st) ptr ->
          Heap.unprotect ptr st)
    in
    Result.fold_list (Store.bindings store) ~init:((), st)
      ~f:(fun ((), st) (_, (ptr, _)) ->
        match (ptr, protected_address) with
        | None, _ -> Result.ok ((), st)
        | Some ptr, None -> Heap.free ptr st
        | Some ((ptr, _) as fptr), Some protect ->
            if%sat Sptr.sem_eq ptr protect then Result.ok ((), st)
            else Heap.free fptr st)

  let resolve_constant (const : Expressions.constant_expr) state =
    match const.value with
    | CLiteral (VScalar scalar) ->
        Result.ok (Base (value_of_scalar scalar), state)
    | CLiteral (VBool b) ->
        Result.ok (Base (if b then Typed.one else Typed.zero), state)
    | CLiteral (VChar c) -> Result.ok (Base (Typed.int (Uchar.to_int c)), state)
    | CLiteral (VFloat { float_value; float_ty = F16 }) ->
        Result.ok (Base (Typed.f16 @@ Float.of_string float_value), state)
    | CLiteral (VFloat { float_value; float_ty = F32 }) ->
        Result.ok (Base (Typed.f32 @@ Float.of_string float_value), state)
    | CLiteral (VFloat { float_value; float_ty = F64 }) ->
        Result.ok (Base (Typed.f64 @@ Float.of_string float_value), state)
    | CLiteral (VFloat { float_value; float_ty = F128 }) ->
        Result.ok (Base (Typed.f128 @@ Float.of_string float_value), state)
    | CLiteral (VStr str) -> (
        let** ptr_opt, state = Heap.load_str_global str state in
        match ptr_opt with
        | Some v -> Result.ok (Ptr v, state)
        | None ->
            (* We "cheat" and model strings as an array of chars, with &str a slice *)
            let len = String.length str in
            let chars =
              String.to_bytes str
              |> Bytes.fold_left
                   (fun l c -> Base (Typed.int (Char.code c)) :: l)
                   []
              |> List.rev
            in
            let char_arr = Array chars in
            let str_ty : Types.ty = mk_array_ty (TLiteral (TInteger U8)) len in
            let** ptr, state = Heap.alloc_ty str_ty state in
            let** (), state = Heap.store ptr str_ty char_arr state in
            let++ (), state = Heap.store_str_global str ptr state in
            (Ptr ptr, state))
    | e ->
        Fmt.kstr not_impl "TODO: resolve_constant %a"
          Expressions.pp_raw_constant_expr e

  (** Resolves a place to a pointer, in the form of a rust_val. We use rust_val
      rather than T.sptr Typed.t, to be able to handle fat pointers; however
      there is the guarantee that this function returns either a Base or a
      FatPointer value. *)
  let rec resolve_place ~store state ({ kind; _ } : Expressions.place) :
      (full_ptr * state, 'e, 'm) Result.t =
    match kind with
    (* Just a local *)
    | PlaceLocal local -> (
        let ptr = Store.find_value local store in
        match ptr with
        | Some ptr ->
            L.debug (fun f ->
                f "Found pointer %a of variable %a" pp_full_ptr ptr
                  Expressions.pp_var_id local);
            Result.ok (ptr, state)
        | None ->
            Fmt.kstr not_impl "Variable %a not found in store"
              Expressions.pp_var_id local)
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) ->
        let** ptr, state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" pp_full_ptr ptr Types.pp_ty base.ty);
        let** v, state = Heap.load ptr base.ty state in
        let v = as_ptr v in
        L.debug (fun f ->
            f "Dereferenced pointer %a to pointer %a" pp_full_ptr ptr
              pp_full_ptr v);
        Result.ok (v, state)
    | PlaceProjection (base, Field (kind, field)) ->
        (* when projecting, we lose the metadata *)
        let** (ptr, _), state = resolve_place ~store state base in
        L.debug (fun f ->
            f "Projecting field %a (kind %a) for %a" Types.pp_field_id field
              Expressions.pp_field_proj_kind kind Sptr.pp ptr);
        let ptr' = Sptr.project base.ty kind field ptr in
        L.debug (fun f ->
            f
              "Dereferenced ADT projection %a, field %a, with pointer %a to \
               pointer %a"
              Expressions.pp_field_proj_kind kind Types.pp_field_id field
              Sptr.pp ptr Sptr.pp ptr');
        Result.ok ((ptr', None), state)

  let rec resolve_function ~(crate : UllbcAst.crate) (fnop : GAst.fn_operand) :
      ('err, 'fixes) fun_exec Rustsymex.t =
    match fnop with
    | FnOpRegular { func = FunId (FRegular fid); _ }
    | FnOpRegular { func = TraitMethod (_, _, fid); _ } -> (
        let fundef = Expressions.FunDeclId.Map.find fid crate.fun_decls in
        L.info (fun g ->
            let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
            g "Resolved function call to %s"
              (PrintTypes.name_to_string ctx fundef.item_meta.name));
        match Std_funs.std_fun_eval ~crate fundef with
        | Some fn -> Rustsymex.return fn
        | None -> Rustsymex.return (exec_fun fundef))
    | FnOpRegular { func = FunId (FBuiltin fn); generics } ->
        Rustsymex.return @@ Std_funs.builtin_fun_eval ~crate fn generics
    | FnOpMove _ ->
        Fmt.kstr not_impl "Move function call is not supported: %a"
          GAst.pp_fn_operand fnop

  (** Resolves a global into a *pointer* Rust value to where that global is *)
  and resolve_global ~crate (g : Types.global_decl_id) state =
    let decl = UllbcAst.GlobalDeclId.Map.find g UllbcAst.(crate.global_decls) in
    let** v_opt, state = Heap.load_global g state in
    match v_opt with
    | Some v -> Result.ok (v, state)
    | None ->
        let** v, state =
          match Std_globals.global_eval ~crate decl with
          | Some global -> Result.ok (global, state)
          | None ->
              (* Same as with strings -- here we need to somehow cache where we store the globals *)
              let fundef =
                UllbcAst.FunDeclId.Map.find decl.body crate.fun_decls
              in
              L.info (fun g ->
                  let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
                  g "Resolved function call to %s"
                    (PrintTypes.name_to_string ctx fundef.item_meta.name));
              let global_fn =
                match Std_funs.std_fun_eval ~crate fundef with
                | Some fn -> fn
                | None -> exec_fun fundef
              in
              global_fn ~crate ~args:[] ~state
        in
        let** ptr, state = Heap.alloc_ty decl.ty state in
        let** (), state = Heap.store ptr decl.ty v state in
        let++ (), state = Heap.store_global g ptr state in
        (ptr, state)

  and eval_operand ~crate:_ ~store state (op : Expressions.operand) =
    match op with
    | Constant c ->
        let++ v, state = resolve_constant c state in
        (v, state)
    | Move loc | Copy loc ->
        let ty = loc.ty in
        let** ptr, state = resolve_place ~store state loc in
        let is_move =
          (* TODO: properly detect if ty has the Copy trait, in which case is_move is
             always false. *)
          match (op, ty) with
          | _, TLiteral _ -> false
          | Move _, _ -> true
          | _ -> false
        in
        let++ v, state = Heap.load ~is_move ptr ty state in
        (v, state)

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
    | RvRef (place, borrow) ->
        let** ptr, state = resolve_place ~store state place in
        let++ ptr', state = Heap.borrow ptr borrow state in
        (Ptr ptr', state)
    | Global { global_id; _ } ->
        let** ptr, state = resolve_global ~crate global_id state in
        let decl =
          UllbcAst.GlobalDeclId.Map.find global_id crate.global_decls
        in
        Heap.load ptr decl.ty state
    | GlobalRef ({ global_id; _ }, _mut) ->
        (* TODO: handle mutability *)
        let++ ptr, state = resolve_global ~crate global_id state in
        (Ptr ptr, state)
    | UnaryOp (op, e) -> (
        let** v, state = eval_operand state e in
        match op with
        | Not ->
            let v = as_base_of ~ty:Typed.t_int v in
            let* v' =
              match type_of_operand e with
              | TLiteral TBool -> return (Typed.not_int_bool v)
              | TLiteral
                  (TInteger ((Usize | U8 | U16 | U32 | U64 | U128) as ty)) ->
                  let max = Layout.max_value ty in
                  return (max -@ v)
              | TLiteral (TInteger (Isize | I8 | I16 | I32 | I64 | I128)) ->
                  return (~-v -@ 1s)
              | ty ->
                  Fmt.kstr not_impl "Unexpect type in UnaryOp.Neg: %a" pp_ty ty
            in
            Result.ok (Base v', state)
        | Neg ->
            let v = as_base_of ~ty:Typed.t_int v in
            Result.ok (Base ~-v, state)
        | PtrMetadata -> (
            match v with
            | Ptr (_, None) -> Result.ok (Tuple [], state)
            | Ptr (_, Some v) -> Result.ok (Base v, state)
            | _ -> not_impl "Invalid value for PtrMetadata")
        | Cast (CastRawPtr (_from, _to)) -> Result.ok (v, state)
        | Cast (CastTransmute (from_ty, to_ty)) ->
            let++ v =
              Heap.lift_err state @@ Encoder.transmute ~from_ty ~to_ty v
            in
            (v, state)
        | Cast (CastScalar (TInteger from_ty, TInteger to_ty)) ->
            let from_size = Layout.size_of_int_ty from_ty in
            let to_size = Layout.size_of_int_ty to_ty in
            if Layout.is_signed from_ty = Layout.is_signed to_ty then
              (* same sign: *)
              if from_size <= to_size then
                (* to a larger number *)
                Result.ok (v, state)
              else if not @@ Layout.is_signed from_ty then
                (* to a smaller number (unsigned) *)
                let max_value = Layout.max_value_z to_ty in
                let v_int = as_base_of ~ty:Typed.t_int v in
                let v_int' = v_int %@ Typed.nonzero_z max_value in
                Result.ok (Base v_int', state)
              else
                (* to a smaller number (signed) *)
                not_impl "Unsupported: integer cast to a smaller signed number"
            else if from_size = to_size then
              (* same size *)
              let min = Typed.int_z @@ Layout.min_value_z from_ty in
              let v_int = as_base_of ~ty:Typed.t_int v in
              let v_int' =
                if Layout.is_signed from_ty then v_int -@ min else v_int +@ min
              in
              Result.ok (Base v_int', state)
            else
              not_impl
                "Unsupported: integer cast with different signedness and sign"
        | Cast (CastScalar (TBool, TInteger (U8 | U16 | U32 | U64 | U128))) ->
            Result.ok (v, state)
        | Cast (CastScalar (TChar, TInteger (U32 | U64 | U128))) ->
            Result.ok (v, state)
        | Cast (CastScalar (TInteger _, TFloat fp)) ->
            let v = as_base_of ~ty:Typed.t_int v in
            let v' = Typed.float_of_int fp v in
            Result.ok (Base v', state)
        | Cast
            (CastUnsize
               ( TAdt
                   ( TBuiltin TBox,
                     {
                       types =
                         [
                           TAdt
                             (TBuiltin TArray, { const_generics = [ size ]; _ });
                         ];
                       _;
                     } ),
                 TAdt
                   (TBuiltin TBox, { types = [ TAdt (TBuiltin TSlice, _) ]; _ })
               )) ->
            let ptr, _ = as_ptr v in
            let size = Typed.int @@ int_of_const_generic size in
            Result.ok (Ptr (ptr, Some size), state)
        | Cast kind ->
            Fmt.kstr not_impl "Unsupported cast kind: %a"
              Expressions.pp_cast_kind kind)
    | BinaryOp (op, e1, e2) -> (
        let** v1, state = eval_operand state e1 in
        let** v2, state = eval_operand state e2 in
        match (v1, v2, type_of_operand e1) with
        | Base v1, Base v2, TLiteral ty -> (
            match op with
            | Ge | Gt | Lt | Le ->
                let op =
                  match op with
                  | Ge -> Typed.geq
                  | Gt -> Typed.gt
                  | Lt -> Typed.lt
                  | Le -> Typed.leq
                  | _ -> assert false
                in
                let* v1, v2, ty = cast_checked2 v1 v2 in
                if Typed.equal_ty ty Typed.t_ptr then
                  Heap.error `UBPointerComparison state
                else
                  let v = op v1 v2 |> Typed.int_of_bool in
                  Result.ok (Base v, state)
            | Eq | Ne ->
                let* v1, v2, _ = cast_checked2 v1 v2 in
                let++ res = Core.equality_check v1 v2 state in
                let res = if op = Eq then res else Typed.not_int_bool res in
                (Base (res :> T.cval Typed.t), state)
            | Add | Sub | Mul | Div | Rem ->
                let++ res = Core.eval_lit_binop op ty v1 v2 state in
                (Base res, state)
            | CheckedAdd | CheckedSub | CheckedMul ->
                let++ res = Core.eval_checked_lit_binop op ty v1 v2 state in
                (res, state)
            | Cmp ->
                let* v1, v2, ty = cast_checked2 v1 v2 in
                if Typed.equal_ty ty Typed.t_ptr then
                  Heap.error `UBPointerComparison state
                else
                  let v = Typed.minus v1 v2 in
                  let* cmp = Core.cmp_of_int v in
                  Result.ok (Base cmp, state)
            | Offset -> not_impl "Offset cannot be used on non-pointer values"
            | (BitOr | BitAnd | BitXor | Shl | Shr) as bop ->
                Fmt.kstr not_impl "Unsupported binary operator: %a"
                  Expressions.pp_binop bop)
        | ((Ptr _ | Base _) as p1), ((Ptr _ | Base _) as p2), _ -> (
            match op with
            | Offset ->
                let* p, meta, v =
                  match (p1, p2) with
                  | Ptr (p, meta), Base v | Base v, Ptr (p, meta) ->
                      return (p, meta, v)
                  | _ -> not_impl "Invalid operands in offset"
                in
                let* v = cast_checked ~ty:Typed.t_int v in
                let p' = Sptr.offset p v in
                Result.ok (Ptr (p', meta), state)
            | _ ->
                let++ res = Core.eval_ptr_binop op p1 p2 state in
                (Base res, state))
        | v1, v2, _ ->
            Fmt.kstr not_impl
              "Unsupported values for binary operator (%a): %a / %a"
              Expressions.pp_binop op pp_rust_val v1 pp_rust_val v2)
    | NullaryOp (op, ty) -> (
        match op with
        | UbChecks ->
            (* See https://doc.rust-lang.org/std/intrinsics/fn.ub_checks.html
               From what I understand: our execution already checks for UB, so we should return
               false, to say we don't want to do UB checks at runtime. *)
            Result.ok (Base (Typed.int_of_bool Typed.v_false), state)
        | SizeOf ->
            let layout = Layout.layout_of ty in
            Result.ok (Base (Typed.int layout.size), state)
        | AlignOf ->
            let layout = Layout.layout_of ty in
            Result.ok (Base (Typed.int layout.align), state)
        | OffsetOf _ ->
            Fmt.kstr not_impl "Unsupported nullary operator: %a"
              Expressions.pp_nullop op)
    | Discriminant (place, kind) ->
        let** (loc, _), state = resolve_place ~store state place in
        let enum = Types.TypeDeclId.Map.find kind UllbcAst.(crate.type_decls) in
        let* discr_ofs, discr_ty =
          match enum.kind with
          | Enum (var :: _) ->
              let int_ty = var.discriminant.int_ty in
              let layout = Layout.of_variant var in
              let discr_ofs = Typed.int @@ Array.get layout.members_ofs 0 in
              return (discr_ofs, Types.TLiteral (TInteger int_ty))
          | Enum [] ->
              Fmt.kstr not_impl "Unsupported discriminant for empty enums"
          | k ->
              Fmt.failwith "Expected an enum for discriminant, got %a"
                Types.pp_type_decl_kind k
        in
        let loc = Sptr.offset loc discr_ofs in
        Heap.load (loc, None) discr_ty state
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
    | Aggregate (AggregatedAdt (_, None, Some field, _), ops) ->
        let* op =
          match ops with
          | [ op ] -> return op
          | _ -> not_impl "union aggregate with >1 values?"
        in
        let++ value, state = eval_operand state op in
        (Union (field, value), state)
    (* Special case? unit (zero-tuple) *)
    | Aggregate (AggregatedAdt (TTuple, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Tuple values, state)
    (* Struct aggregate *)
    | Aggregate (AggregatedAdt (TAdtId _, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Struct values, state)
    (* Invalid aggregate (not sure, but seems like it) *)
    | Aggregate ((AggregatedAdt _ as v), _) ->
        Fmt.failwith "Invalid aggregate kind: %a" Expressions.pp_aggregate_kind
          v
    (* Array aggregate *)
    | Aggregate (AggregatedArray (_ty, _size), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Array values, state)
    | Aggregate (AggregatedClosure _, _) ->
        not_impl "Unsupported rvalue: aggregated closure"
    (* Raw pointer *)
    | RawPtr (place, _kind) ->
        let++ ptr, state = resolve_place ~store state place in
        (Ptr ptr, state)
    (* Length of a &[T;N] or &[T] *)
    | Len (place, _, size_opt) ->
        let** (_, meta), state = resolve_place ~store state place in
        let+ len =
          match (meta, size_opt) with
          | _, Some size -> return (Typed.int @@ int_of_const_generic size)
          | Some len, None -> return len
          | _ -> not_impl "Unexpected len rvalue"
        in
        Soteria_symex.Compo_res.Ok (Base len, state)

  and exec_stmt ~crate store state astmt :
      (store * state, 'err, Heap.serialized list) Rustsymex.Result.t =
    L.info (fun m ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        m "Statement: %s" (PrintUllbcAst.Ast.statement_to_string ctx "" astmt));
    L.debug (fun m ->
        m "Statement full:@.%a" UllbcAst.pp_raw_statement astmt.content);
    let* () = Rustsymex.consume_fuel_steps 1 in
    let { span = loc; content = stmt; _ } : UllbcAst.statement = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | Nop -> Result.ok (store, state)
    | Assign (({ ty; _ } as place), rval) ->
        let** ptr, state = resolve_place ~store state place in
        let** v, state = eval_rvalue ~crate ~store state rval in
        L.info (fun m -> m "Assigning %a <- %a" pp_full_ptr ptr pp_rust_val v);
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | Call { func; args; dest = { ty; _ } as place } ->
        let* exec_fun = resolve_function ~crate func in
        let** args, state = eval_operand_list ~crate ~store state args in
        L.info (fun g ->
            g "Executing function with arguments [%a]"
              Fmt.(list ~sep:comma pp_rust_val)
              args);
        let** v, state =
          let+- err = exec_fun ~crate ~args ~state in
          Heap.add_to_call_trace err
            (Call_trace.make_element ~loc ~msg:"Call trace" ())
        in
        let** ptr, state = resolve_place ~store state place in
        L.info (fun m ->
            let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
            m "Returned %a from %s" pp_rust_val v
              (PrintGAst.fn_operand_to_string ctx func));
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | StorageLive local ->
        let** ty, state =
          match Store.find_opt local store with
          | Some (None, ty) -> Result.ok (ty, state)
          | Some (Some ptr, ty) ->
              let++ (), state = Heap.free ptr state in
              (ty, state)
          | None ->
              Fmt.kstr not_impl "Local %a not found in store?"
                Expressions.pp_var_id local
        in
        let++ ptr, state = Heap.alloc_ty ty state in
        let store = Store.add local (Some ptr, ty) store in
        (store, state)
    | StorageDead local -> (
        match Store.find_opt local store with
        | Some (Some ptr, ty) ->
            let++ (), state = Heap.free ptr state in
            let store = Store.add local (None, ty) store in
            (store, state)
        | Some (None, _) -> Result.ok (store, state)
        | None ->
            Fmt.kstr not_impl "Local %a not found in store"
              Expressions.pp_var_id local)
    | Drop place ->
        (* TODO: this is probably super wrong, drop glue etc. *)
        let** place_ptr, state = resolve_place ~store state place in
        let++ (), state = Heap.uninit place_ptr place.ty state in
        (store, state)
    | Assert { cond; expected; on_failure } -> (
        let** cond, state = eval_operand ~crate ~store state cond in
        let* cond_int =
          match cond with
          | Base cond -> cast_checked cond ~ty:Typed.t_int
          | _ -> not_impl "Expected a base Rust value in assert"
        in
        let cond_bool = Typed.bool_of_int cond_int in
        let cond_bool =
          if expected = true then cond_bool else Typed.not cond_bool
        in
        if%sat cond_bool then Result.ok (store, state)
        else
          match on_failure with
          | UndefinedBehavior -> Heap.error `UBAbort state
          | Panic None -> Heap.error (`Panic "unnamed") state
          | Panic (Some name) ->
              let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
              let name = PrintTypes.name_to_string ctx name in
              Heap.error (`Panic name) state)
    | s ->
        Fmt.kstr not_impl "Unsupported statement: %a" UllbcAst.pp_raw_statement
          s

  and exec_block ~crate ~(body : UllbcAst.expr_body) store state
      ({ statements; terminator } : UllbcAst.block) =
    let** store, state =
      Rustsymex.Result.fold_list statements ~init:(store, state)
        ~f:(fun (store, state) stmt -> exec_stmt ~crate store state stmt)
    in
    L.info (fun f ->
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
        let value_ptr, value_ty = Store.find Expressions.LocalId.zero store in
        let* value_ptr =
          match value_ptr with
          | Some x -> return x
          | None -> Fmt.kstr not_impl "Return value unset, but returned"
        in
        let++ value, state = Heap.load value_ptr value_ty state in
        (value, store, state)
    | Switch (discr, switch) -> (
        let** discr, state = eval_operand ~crate ~store state discr in
        match switch with
        | If (if_block, else_block) ->
            L.info (fun g ->
                g "Switch if/else %a/%a for %a" UllbcAst.pp_block_id if_block
                  UllbcAst.pp_block_id else_block pp_rust_val discr);
            let* block =
              (* if a base value, compare with 0 -- if a pointer, check for null *)
              match discr with
              | Base discr ->
                  if%sat discr ==@ 0s then return else_block
                  else return if_block
              | Ptr (ptr, _) ->
                  if%sat Sptr.is_at_null_loc ptr then return else_block
                  else return if_block
              | _ ->
                  Fmt.kstr not_impl
                    "Expected base value for discriminant, got %a" pp_rust_val
                    discr
            in
            let block = UllbcAst.BlockId.nth body.body block in
            exec_block ~crate ~body store state block
        | SwitchInt (_, options, default) ->
            L.info (fun g ->
                let options =
                  List.map
                    (fun (v, b) -> (PrintValues.scalar_value_to_string v, b))
                    options
                in
                g "Switch options %a (else %a) for %a"
                  Fmt.(
                    list ~sep:comma
                    @@ pair ~sep:(any "->") string UllbcAst.pp_block_id)
                  options UllbcAst.pp_block_id default pp_rust_val discr);
            let compare_discr =
              match discr with
              | Base discr -> fun (v, _) -> discr ==@ value_of_scalar v
              | Ptr (ptr, _) ->
                  fun (v, _) ->
                    if Z.equal Z.zero v.value then Sptr.is_at_null_loc ptr
                    else failwith "Can't compare pointer with non-0 scalar"
              | _ ->
                  fun (v, _) ->
                    Fmt.failwith
                      "Didn't know how to compare discriminant %a with scalar \
                       %s"
                      pp_rust_val discr
                      (PrintValues.scalar_value_to_string v)
            in
            let* block = match_on options ~constr:compare_discr in
            let block = Option.fold ~none:default ~some:snd block in
            let block = UllbcAst.BlockId.nth body.body block in
            exec_block ~crate ~body store state block)
    | Abort kind -> (
        match kind with
        | UndefinedBehavior -> Heap.error `UBAbort state
        | Panic None -> Heap.error (`Panic "unnamed") state
        | Panic (Some name) ->
            let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
            let name_str = PrintTypes.name_to_string ctx name in
            Heap.error (`Panic name_str) state)

  and exec_fun ~crate ~args ~state (fundef : UllbcAst.fun_decl) =
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let* body =
      match body with
      | None ->
          let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
          Fmt.kstr not_impl "Function %s is opaque"
            (PrintTypes.name_to_string ctx name)
      | Some body -> return body
    in
    let@ () = with_loc ~loc in
    L.info (fun m ->
        let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
        m "Calling %s with [@[%a@]]"
          (PrintTypes.name_to_string ctx name)
          Fmt.(list ~sep:comma pp_rust_val)
          args);
    let** store, protected, state = alloc_stack body.locals args state in
    let starting_block = List.hd body.body in
    let** value, store, state =
      exec_block ~crate ~body store state starting_block
    in
    let protected_address =
      match (fundef.signature.output, value) with
      | TRef (RStatic, _, RShared), Ptr (addr, _) -> Some addr
      | _ -> None
    in
    let++ (), state = dealloc_store ?protected_address store protected state in
    (value, state)

  (* re-define this for the export, nowhere else: *)
  let exec_fun ?(ignore_leaks = false) ~crate ~args ~state fundef =
    let** value, state = exec_fun ~crate ~args ~state fundef in
    let++ (), state =
      if ignore_leaks then Result.ok ((), state)
      else
        let@ () = with_loc ~loc:fundef.item_meta.span in
        Heap.leak_check state
    in
    (value, state)
end
