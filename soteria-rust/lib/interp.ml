open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
open Charon
open Charon_util
module T = Typed.T

module Make (Heap : Heap_intf.S) = struct
  module Core = Core.M (Heap)
  module Std_funs = Builtins.Eval.M (Heap)
  module Sptr = Heap.Sptr
  module Encoder = Encoder.Make (Sptr)

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

  let get_variable var_id (store : store) state =
    match Store.find_value var_id store with
    | Some ptr ->
        L.debug (fun m ->
            m "Variable %a has pointer %a" Expressions.pp_var_id var_id
              pp_full_ptr ptr);
        Result.ok (ptr, state)
    | None -> Heap.error `DeadVariable state

  let get_variable_and_ty var_id (store : store) state =
    match Store.find_opt var_id store with
    | Some ptr_and_ty -> Result.ok (ptr_and_ty, state)
    | None -> Heap.error `DeadVariable state

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
                let** ptr', st = Heap.protect ptr subty mut st in
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
    | CLiteral (VFloat { float_value; float_ty }) ->
        Result.ok (Base (Typed.float float_ty float_value), state)
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
            let** (ptr, _), state = Heap.alloc_ty str_ty state in
            let ptr = (ptr, Some (Typed.int len)) in
            let** (), state = Heap.store ptr str_ty char_arr state in
            let++ (), state = Heap.store_str_global str ptr state in
            (Ptr ptr, state))
    | CLiteral (VByteStr _) -> not_impl "TODO: resolve const ByteStr"
    | CTraitConst _ -> not_impl "TODO: resolve const TraitConst"
    | CFnPtr _ -> not_impl "TODO: resolve const FnPtr"
    | CRawMemory _ -> not_impl "TODO: resolve const RawMemory"
    | COpaque msg -> Fmt.kstr not_impl "Opaque constant: %s" msg
    | CVar _ -> not_impl "TODO: resolve const Var (mono error)"

  (** Resolves a place to a pointer, in the form of a rust_val. We use rust_val
      rather than T.sptr Typed.t, to be able to handle fat pointers; however
      there is the guarantee that this function returns either a Base or a
      FatPointer value. *)
  let rec resolve_place ~crate ~store state ({ kind; ty } : Expressions.place) :
      (full_ptr * state, 'e, 'm) Result.t =
    match kind with
    (* Just a local *)
    | PlaceLocal v -> get_variable v store state
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) -> (
        let** ptr, state = resolve_place ~crate ~store state base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" pp_full_ptr ptr Types.pp_ty base.ty);
        let** v, state = Heap.load ptr base.ty state in
        match v with
        | Ptr v ->
            L.debug (fun f ->
                f "Dereferenced pointer %a to pointer %a" pp_full_ptr ptr
                  pp_full_ptr v);
            let pointee = Charon_util.get_pointee base.ty in
            let** () = Heap.check_ptr_align (fst v) pointee in
            Result.ok (v, state)
        | Base off ->
            let* off = cast_checked ~ty:Typed.t_int off in
            let ptr = Sptr.null_ptr in
            let ptr = Sptr.offset ptr off in
            Result.ok ((ptr, None), state)
        | _ -> not_impl "Unexpected value when dereferencing place")
    | PlaceProjection (base, Field (kind, field)) ->
        let** (ptr, meta), state = resolve_place ~crate ~store state base in
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
        if not @@ Layout.is_inhabited ty then Heap.error `RefToUninhabited state
        else Result.ok ((ptr', meta), state)
    | PlaceProjection (base, ProjIndex (idx, from_end)) ->
        let** (ptr, meta), state = resolve_place ~crate ~store state base in
        let len =
          match (meta, base.ty) with
          (* Array with static size *)
          | None, TAdt (TBuiltin TArray, { const_generics = [ len ]; _ }) ->
              Typed.int @@ Charon_util.int_of_const_generic len
          | Some len, TAdt (TBuiltin TSlice, _) -> Typed.cast len
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let** idx, state = eval_operand ~crate ~store state idx in
        let idx = as_base_of ~ty:Typed.t_int idx in
        let idx = if from_end then len -@ 1s -@ idx else idx in
        if%sat 0s <=@ idx &&@ (idx <@ len) then (
          let ptr' = Sptr.offset ~ty ptr idx in
          L.debug (fun f ->
              f "Projected %a, index %a, to pointer %a" Sptr.pp ptr Typed.ppa
                idx Sptr.pp ptr');
          Result.ok ((ptr', None), state))
        else Heap.error `OutOfBounds state
    | PlaceProjection (base, Subslice (from, to_, from_end)) ->
        let** (ptr, meta), state = resolve_place ~crate ~store state base in
        let ty, len =
          match (meta, base.ty) with
          (* Array with static size *)
          | ( None,
              TAdt
                ( TBuiltin TArray,
                  { const_generics = [ len ]; types = [ ty ]; _ } ) ) ->
              (ty, Typed.int @@ Charon_util.int_of_const_generic len)
          | Some len, TAdt (TBuiltin TSlice, { types = [ ty ]; _ }) ->
              (ty, Typed.cast len)
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let** from, state = eval_operand ~crate ~store state from in
        let** to_, state = eval_operand ~crate ~store state to_ in
        let from = as_base_of ~ty:Typed.t_int from in
        let to_ = as_base_of ~ty:Typed.t_int to_ in
        let to_ = if from_end then len -@ to_ else to_ in
        if%sat 0s <=@ from &&@ (from <@ len) &&@ (from <=@ to_) &&@ (to_ <=@ len)
        then (
          let ptr' = Sptr.offset ~ty ptr from in
          let slice_len = to_ -@ from in
          L.debug (fun f ->
              f "Projected %a, slice %a..%a%s, to pointer %a, len %a" Sptr.pp
                ptr Typed.ppa from Typed.ppa to_
                (if from_end then "(from end)" else "")
                Sptr.pp ptr' Typed.ppa slice_len);
          Result.ok ((ptr', Some slice_len), state))
        else Heap.error `OutOfBounds state

  and resolve_function ~(crate : UllbcAst.crate) (fnop : GAst.fn_operand) :
      ('err, 'fixes) fun_exec Rustsymex.t =
    match fnop with
    | FnOpRegular { func = FunId (FRegular fid); _ }
    | FnOpRegular { func = TraitMethod (_, _, fid); _ } -> (
        let fundef = Expressions.FunDeclId.Map.find fid crate.fun_decls in
        L.info (fun g ->
            g "Resolved function call to %s"
              (name_str crate fundef.item_meta.name));
        match Std_funs.std_fun_eval ~crate fundef with
        | Some fn -> Rustsymex.return fn
        | None -> Rustsymex.return (exec_fun fundef))
    | FnOpRegular { func = FunId (FBuiltin fn); generics } ->
        Rustsymex.return @@ Std_funs.builtin_fun_eval fn generics
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
        (* Same as with strings -- here we need to somehow cache where we store the globals *)
        let fundef = UllbcAst.FunDeclId.Map.find decl.body crate.fun_decls in
        L.info (fun g ->
            g "Resolved global init call to %s"
              (name_str crate fundef.item_meta.name));
        let global_fn =
          match Std_funs.std_fun_eval ~crate fundef with
          | Some fn -> fn
          | None -> exec_fun fundef
        in
        (* First we allocate the global and store it in the heap  *)
        let** ptr, state = Heap.alloc_ty decl.ty state in
        let** (), state = Heap.store_global g ptr state in
        (* And only after we compute it; this enables recursive globals *)
        let** v, state = global_fn ~crate ~args:[] ~state in
        let++ (), state = Heap.store ptr decl.ty v state in
        (ptr, state)

  and eval_operand ~crate ~store state (op : Expressions.operand) =
    match op with
    | Constant c ->
        let++ v, state = resolve_constant c state in
        (v, state)
    | (Move loc | Copy loc) when not (Layout.is_inhabited loc.ty) ->
        Heap.error `RefToUninhabited state
    | Move loc | Copy loc -> (
        let ty = loc.ty in
        match Layout.as_zst ty with
        | Some zst ->
            let** _, state = resolve_place ~crate ~store state loc in
            Result.ok (zst, state)
        | None ->
            let** ptr, state = resolve_place ~crate ~store state loc in
            let is_move =
              (* TODO: properly detect if ty has the Copy trait, in which case is_move is
             always false. *)
              match (op, ty) with
              | _, TLiteral _ -> false
              | Move _, _ -> true
              | _ -> false
            in
            let++ v, state = Heap.load ~is_move ptr ty state in
            (v, state))

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
        let** ptr, state = resolve_place ~crate ~store state place in
        let++ ptr', state = Heap.borrow ptr place.ty borrow state in
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
        | Neg -> (
            match type_of_operand e with
            | TLiteral (TInteger _) ->
                let v = as_base_of ~ty:Typed.t_int v in
                Result.ok (Base ~-v, state)
            | TLiteral (TFloat _) ->
                let* v =
                  of_opt_not_impl ~msg:"Expected a float type"
                  @@ Typed.cast_float (as_base v)
                in
                Result.ok (Base (Typed.float_like v 0.0 -.@ v), state)
            | _ -> not_impl "Invalid type for Neg")
        | PtrMetadata -> (
            match v with
            | Ptr (_, None) -> Result.ok (Tuple [], state)
            | Ptr (_, Some v) -> Result.ok (Base v, state)
            | _ -> not_impl "Invalid value for PtrMetadata")
        | ArrayToSlice (_, _, len) -> (
            match v with
            | Ptr (ptr, None) ->
                let len = Typed.int @@ int_of_const_generic len in
                Result.ok (Ptr (ptr, Some len), state)
            | _ -> not_impl "Invalid value for ArrayToSlice")
        | Cast (CastRawPtr (_from, _to)) -> Result.ok (v, state)
        | Cast (CastTransmute (from_ty, to_ty)) ->
            let++ v =
              Heap.lift_err state
              @@ Encoder.transmute ~verify_ptr:(Heap.is_valid_ptr state)
                   ~from_ty ~to_ty v
            in
            (v, state)
        | Cast (CastScalar (from_ty, to_ty)) ->
            let++ v =
              Heap.lift_err state
              @@ Encoder.transmute ~verify_ptr:(Heap.is_valid_ptr state)
                   ~from_ty:(TLiteral from_ty) ~to_ty:(TLiteral to_ty) v
            in
            (v, state)
        | Cast (CastUnsize (from_ty, _)) ->
            let rec get_size = function
              | Types.TRawPtr (ty, _)
              | TRef (_, ty, _)
              | TAdt (TBuiltin TBox, { types = [ ty ]; _ }) ->
                  get_size ty
              | TAdt (TAdtId id, _) -> (
                  let type_decl =
                    Types.TypeDeclId.Map.find id UllbcAst.(crate.type_decls)
                  in
                  match type_decl.kind with
                  | Struct (_ :: _ as fields) ->
                      get_size (List.last fields).field_ty
                  | _ -> not_impl "Couldn't get size in CastUnsize")
              | TAdt (TBuiltin TArray, { const_generics = [ size ]; _ }) ->
                  return size
              | _ -> not_impl "Couldn't get size in CastUnsize"
            in
            let* size = get_size from_ty in
            let ptr, _ = as_ptr v in
            let size = Typed.int @@ int_of_const_generic size in
            Result.ok (Ptr (ptr, Some size), state)
        | Cast (CastFnPtr _ as kind) ->
            Fmt.kstr not_impl "Unsupported cast kind: %a"
              Expressions.pp_cast_kind kind)
    | BinaryOp (op, e1, e2) -> (
        let** v1, state = eval_operand state e1 in
        let** v2, state = eval_operand state e2 in
        match (v1, v2) with
        | Base v1, Base v2 -> (
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
                let* ty =
                  match type_of_operand e1 with
                  | TLiteral ty -> return ty
                  | ty ->
                      Fmt.kstr not_impl "Unexpected type in binop: %a" pp_ty ty
                in
                let++ res = Core.eval_lit_binop op ty v1 v2 state in
                (Base res, state)
            | CheckedAdd | CheckedSub | CheckedMul ->
                let* ty =
                  match type_of_operand e1 with
                  | TLiteral ty -> return ty
                  | ty ->
                      Fmt.kstr not_impl "Unexpected type in binop: %a" pp_ty ty
                in
                let++ res = Core.eval_checked_lit_binop op ty v1 v2 state in
                (res, state)
            | WrappingAdd | WrappingSub | WrappingMul -> (
                match type_of_operand e1 with
                | TLiteral (TInteger ty) ->
                    let** v = Core.safe_binop op v1 v2 state in
                    let+ res = Core.wrap_value ty v in
                    Soteria_symex.Compo_res.Ok (Base res, state)
                | TLiteral ty ->
                    (* Wrapping operations can apply to floating points too, but in that case
                       it is equivalent to the regular operation. *)
                    let++ res = Core.eval_lit_binop op ty v1 v2 state in
                    (Base res, state)
                | ty ->
                    Fmt.kstr not_impl "Unexpected type in binop: %a" pp_ty ty)
            | Cmp ->
                let* v1, v2, ty = cast_checked2 v1 v2 in
                if Typed.equal_ty ty Typed.t_ptr then
                  Heap.error `UBPointerComparison state
                else
                  let v = Typed.minus v1 v2 in
                  let* cmp = Core.cmp_of_int v in
                  Result.ok (Base cmp, state)
            | Offset ->
                (* if offset is done on integers, we just add the (size(T) * rhs) *)
                let* v1, v2, ty = cast_checked2 v1 v2 in
                if Typed.equal_ty ty Typed.t_ptr then
                  Fmt.kstr not_impl
                    "Offset cannot be used on non-int values: %a / %a" Typed.ppa
                    v1 Typed.ppa v2
                else
                  let pointee = Charon_util.get_pointee (type_of_operand e1) in
                  let* size = Layout.size_of_s pointee in
                  let res = v1 +@ (v2 *@ size) in
                  Result.ok (Base res, state)
            | BitOr | BitAnd | BitXor -> (
                let* ity =
                  match type_of_operand e1 with
                  | TLiteral (TInteger ity) -> return ity
                  | TLiteral TBool -> return Values.U8
                  | TLiteral TChar -> return Values.U32
                  | ty ->
                      Fmt.kstr not_impl
                        "Unsupported type for bitwise operation: %a" pp_ty ty
                in
                let size = 8 * Layout.size_of_int_ty ity in
                let signed = Layout.is_signed ity in
                let* v1 = cast_checked ~ty:Typed.t_int v1 in
                let* v2 = cast_checked ~ty:Typed.t_int v2 in
                match op with
                | BitOr ->
                    Result.ok (Base (Typed.bit_or size signed v1 v2), state)
                | BitAnd ->
                    Result.ok (Base (Typed.bit_and size signed v1 v2), state)
                | BitXor ->
                    Result.ok (Base (Typed.bit_xor size signed v1 v2), state)
                | _ -> assert false)
            | Shl | Shr -> (
                let* ity =
                  match type_of_operand e1 with
                  | TLiteral (TInteger ity) -> return ity
                  | TLiteral TBool -> return Values.U8
                  | TLiteral TChar -> return Values.U32
                  | ty ->
                      Fmt.kstr not_impl
                        "Unsupported type for bitwise operation: %a" pp_ty ty
                in
                let size = 8 * Layout.size_of_int_ty ity in
                let signed = Layout.is_signed ity in
                let* v1 = cast_checked ~ty:Typed.t_int v1 in
                let* v2 = cast_checked ~ty:Typed.t_int v2 in
                if%sat v2 <@ 0s ||@ (v2 >=@ Typed.int size) then
                  Heap.error `UBArithShift state
                else
                  match op with
                  | Shl ->
                      Result.ok (Base (Typed.bit_shl size signed v1 v2), state)
                  | Shr ->
                      Result.ok (Base (Typed.bit_shr size signed v1 v2), state)
                  | _ -> assert false))
        | ((Ptr _ | Base _) as p1), ((Ptr _ | Base _) as p2) -> (
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
        | v1, v2 ->
            Fmt.kstr not_impl
              "Unsupported values for binary operator (%a): %a / %a"
              Expressions.pp_binop op pp_rust_val v1 pp_rust_val v2)
    | NullaryOp (op, ty) -> (
        match op with
        | UbChecks ->
            (* See https://doc.rust-lang.org/std/intrinsics/fn.ub_checks.html
               From what I understand: our execution already checks for UB, so we should return
               false, to say we don't want to do UB checks at runtime. *)
            Result.ok (Base (Typed.int_of_bool Typed.v_true), state)
        | SizeOf ->
            let* size = Layout.size_of_s ty in
            Result.ok (Base size, state)
        | AlignOf ->
            let* align = Layout.align_of_s ty in
            Result.ok (Base align, state)
        | OffsetOf _ ->
            Fmt.kstr not_impl "Unsupported nullary operator: %a"
              Expressions.pp_nullop op)
    | Discriminant (place, kind) -> (
        let** (loc, _), state = resolve_place ~crate ~store state place in
        let enum = Types.TypeDeclId.Map.find kind UllbcAst.(crate.type_decls) in
        match enum.kind with
        (* enums with one fieldless variant are ZSTs, so we can't load their discriminant! *)
        | Enum [ { fields = []; discriminant; _ } ] ->
            let discr = Typed.int_z discriminant.value in
            Result.ok (Base discr, state)
        | Enum (var :: _) ->
            let int_ty = var.discriminant.int_ty in
            let layout = Layout.of_variant var in
            let discr_ofs = Typed.int @@ Array.get layout.members_ofs 0 in
            let discr_ty = Types.TLiteral (TInteger int_ty) in
            let loc = Sptr.offset loc discr_ofs in
            Heap.load (loc, None) discr_ty state
        | Enum [] ->
            Fmt.kstr not_impl "Unsupported discriminant for empty enums"
        | k ->
            Fmt.failwith "Expected an enum for discriminant, got %a"
              Types.pp_type_decl_kind k)
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
    (* Tuple aggregate *)
    | Aggregate (AggregatedAdt (TTuple, None, None, _), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Tuple values, state)
    (* Struct aggregate *)
    | Aggregate (AggregatedAdt (TAdtId t_id, None, None, _), operands) ->
        let type_decl =
          Types.TypeDeclId.Map.find t_id UllbcAst.(crate.type_decls)
        in
        let** values, state = eval_operand_list ~crate ~store state operands in
        let++ () =
          match values with
          | [ v ] ->
              Heap.lift_err state
              @@ Layout.apply_attributes v
                   type_decl.item_meta.attr_info.attributes
          | _ -> Result.ok ()
        in
        (Struct values, state)
    (* Invalid aggregate (not sure, but seems like it) *)
    | Aggregate ((AggregatedAdt _ as v), _) ->
        Fmt.failwith "Invalid ADT aggregate kind: %a"
          Expressions.pp_aggregate_kind v
    (* Array aggregate *)
    | Aggregate (AggregatedArray (_ty, _size), operands) ->
        let++ values, state = eval_operand_list ~crate ~store state operands in
        (Array values, state)
    (* Raw pointer construction *)
    | Aggregate (AggregatedRawPtr (_, _), operands) -> (
        let** values, state = eval_operand_list ~crate ~store state operands in
        match values with
        | [ Ptr (ptr, _); Base meta ] -> Result.ok (Ptr (ptr, Some meta), state)
        | [ Base v; Base meta ] ->
            let* v = cast_checked ~ty:Typed.t_int v in
            let ptr = Sptr.offset Sptr.null_ptr v in
            Result.ok (Ptr (ptr, Some meta), state)
        | _ ->
            Fmt.kstr not_impl "AggregatedRawPtr: invalid arguments %a"
              Fmt.(list ~sep:comma pp_rust_val)
              values)
    (* Closure state construction *)
    | Aggregate (AggregatedClosure _, _) ->
        not_impl "Unsupported rvalue: aggregated closure"
    (* Array repetition *)
    | Repeat (value, _, len) ->
        let++ value, state = eval_operand state value in
        let len = int_of_const_generic len in
        let els = List.init len (fun _ -> value) in
        (Array els, state)
    (* Shallow init box -- just casts a ptr into a box *)
    | ShallowInitBox (ptr, _) -> eval_operand state ptr
    (* Raw pointer *)
    | RawPtr (place, _kind) ->
        let++ ptr, state = resolve_place ~crate ~store state place in
        (Ptr ptr, state)
    (* Length of a &[T;N] or &[T] *)
    | Len (place, _, size_opt) ->
        let** (_, meta), state = resolve_place ~crate ~store state place in
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
    L.trace (fun m ->
        m "Statement full:@.%a" UllbcAst.pp_raw_statement astmt.content);
    let { span = loc; content = stmt; _ } : UllbcAst.statement = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | Nop -> Result.ok (store, state)
    | Assign (({ ty; _ } as place), rval) ->
        let** ptr, state = resolve_place ~crate ~store state place in
        let** v, state = eval_rvalue ~crate ~store state rval in
        L.info (fun m -> m "Assigning %a <- %a" pp_full_ptr ptr pp_rust_val v);
        let++ (), state = Heap.store ptr ty v state in
        (store, state)
    | StorageLive local ->
        let** (ptr, ty), state = get_variable_and_ty local store state in
        let** (), state =
          match ptr with
          | None -> Result.ok ((), state)
          | Some ptr -> Heap.free ptr state
        in
        let++ ptr, state = Heap.alloc_ty ty state in
        let store = Store.add local (Some ptr, ty) store in
        (store, state)
    | StorageDead local -> (
        let** (ptr, ty), state = get_variable_and_ty local store state in
        match ptr with
        | Some ptr ->
            let++ (), state = Heap.free ptr state in
            let store = Store.add local (None, ty) store in
            (store, state)
        | None -> Result.ok (store, state))
    | Drop place ->
        (* TODO: this is probably super wrong, drop glue etc. *)
        let** place_ptr, state = resolve_place ~crate ~store state place in
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
          | Panic name ->
              let name = Option.map (name_str crate) name in
              Heap.error (`Panic name) state)
    | CopyNonOverlapping { src; dst; count } ->
        let ty = get_pointee (type_of_operand src) in
        let** args, state =
          eval_operand_list ~crate ~store state [ src; dst; count ]
        in
        let++ _, state =
          Std_funs.Std.copy_nonoverlapping ty ~crate ~args ~state
        in
        (store, state)
    | SetDiscriminant (_, _) ->
        not_impl "Unsupported statement: SetDiscriminant"
    | Deinit _ -> not_impl "Unsupported statement: Deinit"

  and exec_block ~crate ~(body : UllbcAst.expr_body) store state
      ({ statements; terminator } : UllbcAst.block) =
    let* () = Rustsymex.consume_fuel_steps 1 in
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
    | Call ({ func; args; dest = { ty; _ } as place }, target) ->
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
        let** ptr, state = resolve_place ~crate ~store state place in
        L.info (fun m ->
            let ctx = PrintUllbcAst.Crate.crate_to_fmt_env crate in
            m "Returned %a from %s" pp_rust_val v
              (PrintGAst.fn_operand_to_string ctx func));
        let** (), state = Heap.store ptr ty v state in
        let block = UllbcAst.BlockId.nth body.body target in
        exec_block ~crate ~body store state block
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
        | Panic name ->
            let name = Option.map (name_str crate) name in
            Heap.error (`Panic name) state)

  and exec_fun ~crate ~args ~state (fundef : UllbcAst.fun_decl) =
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let* body =
      match body with
      | None -> Fmt.kstr not_impl "Function %s is opaque" (name_str crate name)
      | Some body -> return body
    in
    let@ () = with_loc ~loc in
    L.info (fun m ->
        m "Calling %s with [%a]" (name_str crate name)
          Fmt.(list ~sep:(any ", ") pp_rust_val)
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
    let** value, state =
      let+- err = exec_fun ~crate ~args ~state fundef in
      Heap.add_to_call_trace err
        (Call_trace.make_element ~loc:fundef.item_meta.span ~msg:"Call trace" ())
    in
    let++ (), state =
      if ignore_leaks then Result.ok ((), state)
      else
        let@ () = with_loc ~loc:fundef.item_meta.span in
        Heap.leak_check state
    in
    (value, state)
end
