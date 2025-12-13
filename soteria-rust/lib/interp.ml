open Rustsymex
module BV = Typed.BitVec
open Typed.Syntax
open Typed.Infix
open Charon
open Charon_util
open Rust_val

module Make (State : State_intf.S) = struct
  module Rust_state_m = Rust_state_m.Make (State)
  module Core = Core.M (Rust_state_m)
  module Std_funs = Builtins.Eval.M (Rust_state_m)

  exception Unsupported of (string * Meta.span_data)

  open Rust_state_m
  open Rust_state_m.Syntax

  type store = (full_ptr option * Types.ty) Store.t
  type 'a t = ('a, store) Rust_state_m.t
  type 'err fun_exec = rust_val list -> (rust_val, unit) Rust_state_m.t

  let get_variable var_id =
    let* store = get_env () in
    match Store.find_value var_id store with
    | Some ptr ->
        L.debug (fun m ->
            m "Variable %a has pointer %a" Expressions.pp_var_id var_id
              pp_full_ptr ptr);
        ok ptr
    | None -> error `DeadVariable

  let get_variable_and_ty var_id =
    let* store = get_env () in
    match Store.find_opt var_id store with
    | Some ptr_and_ty -> ok ptr_and_ty
    | None -> error `DeadVariable

  let alloc_stack (locals : GAst.locals) args : (full_ptr * Types.ty) list t =
    if List.compare_length_with args locals.arg_count <> 0 then
      failwith
        "Function called with wrong arg count, should have been caught before";
    (* create a store with all types *)
    let* () =
      map_env @@ fun store ->
      List.fold_left
        (fun st (local : GAst.local) ->
          Store.add local.index (None, local.local_ty) st)
        store locals.locals
    in
    (* allocate arguments and return value, updating store *)
    let alloc_locs = List.take (1 + locals.arg_count) locals.locals in
    let tys =
      List.map (fun ({ local_ty; _ } : GAst.local) -> local_ty) alloc_locs
    in
    let* ptrs = State.alloc_tys tys in
    let tys_ptrs = List.combine alloc_locs ptrs in
    let* () =
      map_env @@ fun store ->
      List.fold_left
        (fun store ((local : GAst.local), ptr) ->
          Store.add local.index (Some ptr, local.local_ty) store)
        store tys_ptrs
    in
    (* store values for the arguments *)
    let tys_ptrs = List.tl tys_ptrs in
    let+ protected =
      fold_list tys_ptrs ~init:[]
        ~f:(fun protected ({ index; local_ty = ty; _ }, ptr) ->
          let index = Expressions.LocalId.to_int index in
          let value = List.nth args (index - 1) in
          (* Passed (nested) references must be protected and be valid. *)
          let* value, protected' =
            Layout.update_ref_tys_in value ty ~init:protected
              ~f:(fun acc ptr subty mut ->
                let+ ptr' = State.protect ptr subty mut in
                (ptr', (ptr', subty) :: acc))
          in
          let+ () = State.store ptr ty value in
          protected')
    in
    protected

  (** [dealloc_store ?protected_address store protected st] Deallocates the
      locations in [st] used for the variables in [store]; if
      [protected_address] is provided, will not deallocate that location (this
      is used e.g. for globals, that return a &'static reference). Will also
      remove the protectors from the pointers [protected] that were given at the
      function's entry. *)
  let dealloc_store ?protected_address protected =
    let* () =
      fold_list protected ~init:() ~f:(fun () (ptr, ty) ->
          State.unprotect ptr ty)
    in
    let* store = get_env () in
    fold_list (Store.bindings store) ~init:() ~f:(fun () (_, (ptr, _)) ->
        match (ptr, protected_address) with
        | None, _ -> ok ()
        | Some ptr, None -> State.free ptr
        | Some ((ptr, _) as fptr), Some protect ->
            if%sat Sptr.sem_eq ptr protect then ok () else State.free fptr)

  let resolve_fn_ptr (fn : Types.fn_ptr) : Types.fun_decl_ref =
    match fn.kind with
    | FunId (FRegular id) -> { id; generics = fn.generics }
    (* TODO: the generics here are probably wrong; we should also go via the trait,
       and properly resolve it. *)
    | TraitMethod (_, _, id) -> { id; generics = fn.generics }
    | FunId (FBuiltin _) -> failwith "Can't resolve a builtin function"

  let rec resolve_constant (const : Expressions.constant_expr) =
    match const.kind with
    | CLiteral (VScalar scalar) -> ok (Int (BV.of_scalar scalar))
    | CLiteral (VBool b) -> ok (Int (BV.of_bool (Typed.bool b)))
    | CLiteral (VChar c) -> ok (Int (BV.u32i (Uchar.to_int c)))
    | CLiteral (VFloat { float_value; float_ty }) ->
        ok (Float (Typed.Float.mk float_ty float_value))
    | CLiteral (VStr str) -> (
        let* ptr_opt = State.load_str_global str in
        match ptr_opt with
        | Some v -> ok (Ptr v)
        | None ->
            (* We "cheat" and model strings as an array of chars, with &str a slice *)
            let len = String.length str in
            let chars =
              String.to_bytes str
              |> Bytes.fold_left (fun l c -> Int (BV.u8i (Char.code c)) :: l) []
              |> List.rev
            in
            let char_arr = Tuple chars in
            let str_ty : Types.ty =
              mk_array_ty (TLiteral (TUInt U8)) (Z.of_int len)
            in
            let* ptr, _ = State.alloc_ty ~kind:StaticString str_ty in
            let ptr = (ptr, Len (BV.usizei len)) in
            let* () = State.store ptr str_ty char_arr in
            let+ () = State.store_str_global str ptr in
            Ptr ptr)
    | CFnDef fn_ptr -> ok (ConstFn fn_ptr)
    | CLiteral (VByteStr _) -> not_impl "TODO: resolve const ByteStr"
    (* FIXME: this is hacky, but until we get proper monomorphisation this isn't too bad *)
    | CTraitConst (tref, name) -> (
        match tref.kind with
        | TraitImpl { id; _ } ->
            let timpl = Crate.get_trait_impl id in
            let _, global = List.find (fun (n, _) -> n = name) timpl.consts in
            let* glob_ptr = resolve_global global in
            let glob = Crate.get_global global.id in
            State.load glob_ptr glob.ty
        | Clause _ -> not_impl "TODO: TraitConst(Clause)"
        | ParentClause _ -> not_impl "TODO: TraitConst(ParentClause)"
        | ItemClause _ -> not_impl "TODO: TraitConst(ItemClause)"
        | Self -> not_impl "TODO: TraitConst(Self)"
        | BuiltinOrAuto _ -> not_impl "TODO: TraitConst(BuiltinOrAuto)"
        | Dyn -> not_impl "TODO: TraitConst(Dyn)"
        | UnknownTrait _ -> not_impl "TODO: TraitConst(UnknownTrait)")
    | CRawMemory bytes ->
        (* This whole function is a bit complicated, due to the fact we don't supprt
           pointer chunks, meaning we need to do a best-effort reconstruction of the
           full pointers from the pointer bytes. *)
        (* First, we iterate over the bytes and try to group up the pointers *)
        let bytes = List.mapi (fun i b -> (i, b)) bytes in
        let last, blocks =
          List.fold_left
            (fun (curr, blocks) (i, (byte : Expressions.byte)) ->
              match (curr, byte) with
              | None, Uninit -> (None, blocks)
              | None, Value b -> (None, `Byte (BV.u8i b, i) :: blocks)
              | None, Provenance (p, from_) -> (Some (p, from_, i), blocks)
              | Some (p, from_, ofs), Provenance (p', idx) ->
                  (* if different provenance or non-contiguous, stop *)
                  if
                    (not (Expressions.equal_provenance p p'))
                    || idx <> from_ + (i - ofs)
                  then
                    (Some (p', idx, i), `Ptr (p, from_, i - ofs, ofs) :: blocks)
                  else (Some (p, from_, ofs), blocks)
              | Some (p, from_, ofs), Value b ->
                  ( None,
                    `Byte (BV.u8i b, i)
                    :: `Ptr (p, from_, i - ofs, ofs)
                    :: blocks )
              | Some (p, from_, ofs), Uninit ->
                  (None, `Ptr (p, from_, i - ofs, ofs) :: blocks))
            (None, []) bytes
        in
        let blocks =
          match last with
          | None -> blocks
          | Some (p, from_, ofs) ->
              `Ptr (p, from_, List.length bytes - ofs, ofs) :: blocks
        in
        (* Map the smaller blocks to actual rust values, ie. pointers or integers *)
        let ptr_size = Crate.pointer_size () in
        let ptr_of_provenance : Expressions.provenance -> full_ptr t = function
          | Global g -> resolve_global g
          | Function f -> State.declare_fn f
          | Unknown -> not_impl "Unknown provenance in RawMemory"
        in
        let+ blocks =
          fold_list blocks ~init:[] ~f:(fun acc block ->
              match block with
              | `Byte (b, ofs) -> ok ((Int b, BV.usizei ofs) :: acc)
              | `Ptr (p, from_, size, ofs) ->
                  let* ptr, _ = ptr_of_provenance p in
                  if from_ = 0 && size = ptr_size then
                    ok ((Ptr (ptr, Thin), BV.usizei ofs) :: acc)
                  else
                    let+ ptr_int = Sptr.decay ptr in
                    let ptr_frag =
                      BV.extract (from_ * 8) ((from_ + size) * 8) ptr_int
                    in
                    (Int ptr_frag, BV.usizei ofs) :: acc)
        in
        Union blocks
    | COpaque msg -> Fmt.kstr not_impl "Opaque constant: %s" msg
    | CVar _ -> not_impl "TODO: resolve const Var (mono error)"

  (** Resolves a place to a pointer, in the form of a rust_val. We use rust_val
      rather than T.sptr Typed.t, to be able to handle fat pointers; however
      there is the guarantee that this function returns either a Base or a
      FatPointer value. *)
  and resolve_place ({ kind; ty } : Expressions.place) : full_ptr t =
    match kind with
    (* Just a local *)
    | PlaceLocal v -> get_variable v
    (* Just a global *)
    | PlaceGlobal g -> resolve_global g
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) -> (
        let* ptr = resolve_place base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" pp_full_ptr ptr pp_ty base.ty);
        let* v = State.load ptr base.ty in
        match v with
        | Ptr fptr -> (
            L.debug (fun f ->
                f "Dereferenced pointer %a to pointer %a" pp_full_ptr ptr
                  pp_full_ptr fptr);
            let pointee = Charon_util.get_pointee base.ty in
            match base.ty with
            | TRef _ | TAdt { id = TBuiltin TBox; _ } ->
                let+ () = State.check_ptr_align fptr pointee in
                fptr
            | _ -> ok fptr)
        | Int off ->
            let off = Typed.cast_i Usize off in
            let ptr = Sptr.null_ptr_of off in
            ok (ptr, Thin)
        | _ -> not_impl "Unexpected value when dereferencing place")
    (* The metadata of a pointer type is just the second part of the pointer *)
    | PlaceProjection (base, PtrMetadata) ->
        let* ((ptr, _) as fptr) = resolve_place base in
        let* () =
          let* valid = State.is_valid_ptr fptr base.ty in
          if valid then ok () else error `UBDanglingPointer
        in
        L.debug (fun f ->
            f "Projecting metadata of pointer %a for %a" Sptr.pp ptr pp_ty
              base.ty);
        let+ ptr' =
          Sptr.offset ~check:false ~ty:(TLiteral (TUInt Usize)) ~signed:false
            ptr
            Usize.(1s)
        in
        (ptr', Thin)
    | PlaceProjection (base, Field (kind, field)) ->
        let* ((ptr, meta) as fptr) = resolve_place base in
        let* () = State.check_ptr_align fptr base.ty in
        L.debug (fun f ->
            f "Projecting field %a (kind %a) for %a" Types.pp_field_id field
              Expressions.pp_field_proj_kind kind Sptr.pp ptr);
        let* ptr' = Sptr.project base.ty kind field ptr in
        L.debug (fun f ->
            f "Projecting ADT %a, field %a, with pointer %a to pointer %a"
              Expressions.pp_field_proj_kind kind Types.pp_field_id field
              Sptr.pp ptr Sptr.pp ptr');
        if not @@ Layout.is_inhabited ty then error `RefToUninhabited
        else ok (ptr', meta)
    | PlaceProjection (base, ProjIndex (idx, from_end)) ->
        let* ptr, meta = resolve_place base in
        let len =
          match (meta, base.ty) with
          (* Array with static size *)
          | ( Thin,
              TAdt
                {
                  id = TBuiltin TArray;
                  generics = { const_generics = [ len ]; _ };
                } ) ->
              BV.usize_of_const_generic len
          | Len len, TAdt { id = TBuiltin TSlice; _ } -> Typed.cast_i Usize len
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let* idx = eval_operand idx in
        let idx = as_base_i Usize idx in
        let idx = if from_end then len -!@ idx else idx in
        let* () =
          State.assert_ (Usize.(0s) <=$@ idx &&@ (idx <$@ len)) `OutOfBounds
        in
        let+ ptr' = Sptr.offset ~signed:false ~ty ptr idx in
        L.debug (fun f ->
            f "Projected %a, index %a, to pointer %a" Sptr.pp ptr Typed.ppa idx
              Sptr.pp ptr');
        (ptr', Thin)
    | PlaceProjection (base, Subslice (from, to_, from_end)) ->
        let* ptr, meta = resolve_place base in
        let ty, len =
          match (meta, base.ty) with
          (* Array with static size *)
          | ( Thin,
              TAdt
                {
                  id = TBuiltin TArray;
                  generics = { const_generics = [ len ]; types = [ ty ]; _ };
                } ) ->
              (ty, BV.usize_of_const_generic len)
          | ( Len len,
              TAdt { id = TBuiltin TSlice; generics = { types = [ ty ]; _ } } )
            ->
              (ty, Typed.cast len)
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let* from = eval_operand from in
        let* to_ = eval_operand to_ in
        let from = as_base_i Usize from in
        let to_ = as_base_i Usize to_ in
        let to_ = if from_end then len -!@ to_ else to_ in
        let* () =
          State.assert_
            (Usize.(0s) <=$@ from &&@ (from <=$@ to_) &&@ (to_ <=$@ len))
            `OutOfBounds
        in
        let+ ptr' = Sptr.offset ~signed:false ~ty ptr from in
        let slice_len = to_ -!@ from in
        L.debug (fun f ->
            f "Projected %a, slice %a..%a%s, to pointer %a, len %a" Sptr.pp ptr
              Typed.ppa from Typed.ppa to_
              (if from_end then "(from end)" else "")
              Sptr.pp ptr' Typed.ppa slice_len);
        (ptr', Len slice_len)

  (** Resolve a function operand, returning a callable symbolic function to
      execute it. It also returns the types expected of the function, which is
      needed to load the first argument of a dyn method call.

      This function also handles validating the call; given the input types it
      will be called with and the output type expected, it will make sure these
      are the right types and in the right amount.

      The arguments must be passed, as for calls on [&dyn Trait] types the first
      argument holds the VTable pointer. *)
  and resolve_function ~in_tys ~out_ty :
      GAst.fn_operand -> ('err fun_exec * Types.ty list) t =
    let validate_call ?(is_dyn = false) (fn : Types.fun_decl_ref) =
      let fn = Crate.get_fun fn.id in
      let rec check_tys l r =
        match (l, r) with
        | [], [] -> ok ()
        | ty1 :: l, ty2 :: r ->
            let* compatible = Layout.is_abi_compatible ty1 ty2 in
            if%sat compatible then check_tys l r
            else error (`InvalidFnArgTys (ty1, ty2))
        | _ ->
            error
              (`InvalidFnArgCount
                 (List.length in_tys, List.length fn.signature.inputs))
      in
      (* a bit hacky, but we don't want to compare the dyn parameter with
             the expected input; the mismatch is intended here. *)
      let in_tys, sig_ins =
        if is_dyn then (List.tl in_tys, List.tl fn.signature.inputs)
        else (in_tys, fn.signature.inputs)
      in
      check_tys (out_ty :: in_tys) (fn.signature.output :: sig_ins)
    in
    let perform_call (fn : Types.fun_decl_ref) =
      try
        let fundef = Crate.get_fun fn.id in
        L.info (fun g ->
            g "Resolved function call to %a" Crate.pp_name fundef.item_meta.name);
        match Std_funs.std_fun_eval fundef exec_fun with
        | Some fn -> ok (fn, fundef.signature.inputs)
        | None -> ok (exec_fun fundef, fundef.signature.inputs)
      with Crate.MissingDecl _ -> not_impl "Missing function declaration"
    in
    function
    (* Handle builtins separately *)
    | FnOpRegular { kind = FunId (FBuiltin fn); generics } ->
        ok (Std_funs.builtin_fun_eval fn generics, in_tys)
    (* For static calls we don't need to check types, that's what the type checker does. *)
    | FnOpRegular fn_ptr -> perform_call @@ resolve_fn_ptr fn_ptr
    (* Here we need to check the type of the actual function, as it could have been cast. *)
    | FnOpDynamic op ->
        let* fn_ptr = eval_operand op in
        let fn_ptr = as_ptr fn_ptr in
        let* fn = State.lookup_fn fn_ptr in
        let* () = validate_call fn in
        perform_call fn

  (** Resolves a global into a *pointer* Rust value to where that global is *)
  and resolve_global (glob : Types.global_decl_ref) =
    let decl = Crate.get_global glob.id in
    let* v_opt = State.load_global glob.id in
    match v_opt with
    | Some v -> ok v
    | None ->
        (* Same as with strings -- here we need to somehow cache where we store the globals *)
        let fundef = Crate.get_fun decl.init in
        L.info (fun g ->
            g "Resolved global init call to %a" Crate.pp_name
              fundef.item_meta.name);
        let global_fn =
          match Std_funs.std_fun_eval fundef exec_fun with
          | Some fn -> fn
          | None -> exec_fun fundef
        in
        (* First we allocate the global and store it in the State  *)
        let* ptr =
          State.alloc_ty ~kind:(Static glob) ~span:decl.item_meta.span.data
            decl.ty
        in
        let* () = State.store_global glob.id ptr in
        (* And only after we compute it; this enables recursive globals *)
        let* v = with_env ~env:() @@ global_fn [] in
        let+ () = State.store ptr decl.ty v in
        ptr

  and eval_operand (op : Expressions.operand) =
    match op with
    | Constant c -> resolve_constant c
    | (Move loc | Copy loc) when not (Layout.is_inhabited loc.ty) ->
        error `RefToUninhabited
    | Move loc | Copy loc -> (
        let ty = loc.ty in
        let* ptr = resolve_place loc in
        match Layout.as_zst ty with
        | Some zst ->
            let+ () = State.check_ptr_align ptr ty in
            zst
        | None -> State.load ptr ty)

  and eval_operand_list ops =
    let+ vs =
      fold_list ops ~init:[] ~f:(fun acc op ->
          let+ new_res = eval_operand op in
          new_res :: acc)
    in
    List.rev vs

  and eval_rvalue (expr : Expressions.rvalue) =
    match expr with
    | Use op -> eval_operand op
    (* Reference *)
    | RvRef (place, borrow, _metadata) ->
        let* ptr = resolve_place place in
        let* ptr' = State.borrow ptr place.ty borrow in
        let* is_valid = State.is_valid_ptr ptr' place.ty in
        if is_valid then ok (Ptr ptr') else error `UBDanglingPointer
    (* Raw pointer *)
    | RawPtr (place, _kind, _metadata) ->
        let+ ptr = resolve_place place in
        Ptr ptr
    | UnaryOp (op, e) -> (
        let* v = eval_operand e in
        let ty = type_of_operand e in
        match op with
        | Not -> (
            let ty = TypesUtils.ty_as_literal ty in
            let v = as_base ty v in
            match ty with
            | TBool -> ok (Int (BV.not_bool v))
            | TInt _ | TUInt _ -> ok (Int (BV.not v))
            | _ -> not_impl "Invalid type for Not")
        | Neg _ -> (
            match type_of_operand e with
            | TLiteral ((TInt _ | TUInt _) as ty) ->
                let v = as_base ty v in
                let res, overflowed = ~-?v in
                let+ () = State.assert_not overflowed `Overflow in
                Int res
            | TLiteral (TFloat fty) ->
                let v = as_base_f fty v in
                ok (Float (Typed.Float.neg v))
            | _ -> not_impl "Invalid type for Neg")
        | Cast (CastRawPtr (from_ty, to_ty)) -> (
            match (from_ty, to_ty) with
            | (TRef _ | TRawPtr _), TLiteral _ ->
                (* expose provenance *)
                let v, _ = as_ptr v in
                let+ v' = Sptr.expose v in
                Int v'
            | TLiteral _, (TRef _ | TRawPtr _) ->
                (* with provenance *)
                let v = as_base_i Usize v in
                let+ ptr = State.with_exposed v in
                Ptr ptr
            | _ -> ok v)
        | Cast (CastTransmute (from_ty, to_ty)) ->
            Core.transmute ~from_ty ~to_ty v
        | Cast (CastScalar (from_ty, to_ty)) ->
            let* v =
              match v with
              | Int i -> ok (i :> T.cval Typed.t)
              | Float f -> ok (f :> T.cval Typed.t)
              | _ -> not_impl "Invalid value for CastScalar"
            in
            Encoder.cast_literal ~from_ty ~to_ty v
        | Cast (CastUnsize (_, _, meta)) ->
            let update_meta prev =
              match meta with
              | MetaLength length ->
                  ok @@ Len (BV.usize_of_const_generic length)
              | MetaVTableDirect (_, glob) ->
                  (* the global adds one level of indirection *)
                  let* glob = of_opt_not_impl "Missing VTable global" glob in
                  let* ptr = resolve_global glob in
                  let+ vtable = State.load ptr unit_ptr in
                  let vtable, _ = as_ptr vtable in
                  VTable vtable
              (* We don't check validity of the metadata if the unsizing
                  doesn't need to modify the VTable. *)
              | MetaVTableNested (_, None) -> ok prev
              | MetaVTableNested (_, Some field) -> (
                  match prev with
                  | Thin -> failwith "Unsizing VTable with no meta?"
                  | Len _ -> error `UBDanglingPointer
                  | VTable vt ->
                      let idx = Types.FieldId.to_int field in
                      let* vt_addr =
                        Sptr.offset ~ty:unit_ptr ~signed:false vt
                          (BV.usizei idx)
                      in
                      let+ vt = State.load (vt_addr, Thin) unit_ptr in
                      let vt, _ = as_ptr vt in
                      VTable vt)
              | MetaUnknown ->
                  Fmt.kstr not_impl "Unsupported metadata in CastUnsize: %a"
                    Expressions.pp_unsizing_metadata meta
            in
            let rec with_ptr_meta : rust_val -> rust_val t = function
              | Ptr (v, prev) ->
                  let+ meta = update_meta prev in
                  Ptr (v, meta)
              | Tuple (_ :: _ as fs) as v -> (
                  let rec split_at_non_empty fs left =
                    match fs with
                    | [] -> None
                    | f :: rest when Rust_val.is_empty f ->
                        split_at_non_empty rest (f :: left)
                    | f :: rest -> Some (List.rev left, f, rest)
                  in
                  let opt_nonempty = split_at_non_empty (List.rev fs) [] in
                  match opt_nonempty with
                  | Some (left, nonempty, right) -> (
                      let+ nonempty = with_ptr_meta nonempty in
                      let fs = List.rev (left @ [ nonempty ] @ right) in
                      match v with Tuple _ -> Tuple fs | _ -> assert false)
                  | None -> not_impl "Couldn't set pointer meta in CastUnsize")
              | _ -> not_impl "Couldn't set pointer meta in CastUnsize"
            in
            with_ptr_meta v
        | Cast (CastConcretize (_from, _to)) ->
            not_impl "Unsupported: dyn (concretize)"
        | Cast (CastFnPtr (_from, _to)) -> (
            match v with
            | ConstFn fn_ptr ->
                let fn = resolve_fn_ptr fn_ptr in
                let+ ptr = State.declare_fn fn in
                Ptr ptr
            | Ptr _ as ptr -> ok ptr
            | _ -> not_impl "Invalid argument to CastFnPtr"))
    | BinaryOp (op, e1, e2) -> (
        let* v1 = eval_operand e1 in
        let* v2 = eval_operand e2 in
        match (v1, v2) with
        | Int v1, Int v2 -> (
            match op with
            | Ge | Gt | Lt | Le ->
                let lit_ty = TypesUtils.ty_as_literal (type_of_operand e1) in
                let signed = Layout.is_signed lit_ty in
                let op =
                  match op with
                  | Ge -> BV.geq
                  | Gt -> BV.gt
                  | Lt -> BV.lt
                  | Le -> BV.leq
                  | _ -> assert false
                in
                let v = op ~signed v1 v2 |> BV.of_bool in
                ok (Int v)
            | Eq | Ne ->
                let v1, v2, _ = Typed.cast_checked2 v1 v2 in
                let+ res = Core.equality_check v1 v2 in
                let res = if op = Eq then res else BV.not_bool res in
                Int res
            | Add _ | Sub _ | Mul _ | Div _ | Rem _ | Shl _ | Shr _ ->
                let ty = TypesUtils.ty_as_literal (type_of_operand e1) in
                let+ res = Core.eval_lit_binop op ty v1 v2 in
                Int (Typed.cast res)
            | AddChecked | SubChecked | MulChecked ->
                let ty =
                  match type_of_operand e1 with
                  | TLiteral ty -> ty
                  | ty -> Fmt.failwith "Unexpected type in binop: %a" pp_ty ty
                in
                Core.eval_checked_lit_binop op ty v1 v2
            | Cmp ->
                let v1, v2, ty = Typed.cast_checked2 v1 v2 in
                if Typed.equal_ty ty (Typed.t_ptr ()) then
                  error `UBPointerComparison
                else
                  let ty = type_of_operand e1 in
                  let ty = TypesUtils.ty_as_literal ty in
                  let+ cmp = Core.cmp ~signed:(Layout.is_signed ty) v1 v2 in
                  Int cmp
            | Offset ->
                (* non-zero offset on integer pointer is not permitted, as these are always
                   dangling *)
                let v2 = Typed.cast_i Usize v2 in
                let ty = Charon_util.get_pointee (type_of_operand e1) in
                let* size = Layout.size_of ty in
                let+ () =
                  State.assert_
                    (v2 ==@ Usize.(0s) ||@ (size ==@ Usize.(0s)))
                    `UBDanglingPointer
                in
                Int v1
            | BitOr | BitAnd | BitXor -> (
                let ty = TypesUtils.ty_as_literal (type_of_operand e1) in
                let v1 = Typed.cast_lit ty v1 in
                let v2 = Typed.cast_lit ty v2 in
                match op with
                | BitOr -> ok (Int (v1 |@ v2))
                | BitAnd -> ok (Int (v1 &@ v2))
                | BitXor -> ok (Int (v1 ^@ v2))
                | _ -> assert false))
        | ((Ptr _ | Int _) as p1), ((Ptr _ | Int _) as p2) -> (
            match op with
            | Offset ->
                let^ p, meta, v =
                  match (p1, p2) with
                  | Ptr (p, meta), Int v -> return (p, meta, v)
                  | _ -> Rustsymex.not_impl "Invalid operands in offset"
                in
                let ty = Charon_util.get_pointee (type_of_operand e1) in
                let v = Typed.cast_i Usize v in
                let off_ty = TypesUtils.ty_as_literal (type_of_operand e2) in
                let signed = Layout.is_signed off_ty in
                let+ p' = Sptr.offset ~signed ~ty p v in
                Ptr (p', meta)
            | _ ->
                let+ res = Core.eval_ptr_binop op p1 p2 in
                Int res)
        | Float v1, Float v2 -> (
            match op with
            | Eq -> ok (Int (BV.of_bool (v1 ==.@ v2)))
            | Ne -> ok (Int (BV.of_bool (Typed.not (v1 ==.@ v2))))
            | Ge -> ok (Int (BV.of_bool (v1 >=.@ v2)))
            | Gt -> ok (Int (BV.of_bool (v1 >.@ v2)))
            | Lt -> ok (Int (BV.of_bool (v1 <.@ v2)))
            | Le -> ok (Int (BV.of_bool (v1 <=.@ v2)))
            | Add _ -> ok (Float (v1 +.@ v2))
            | Sub _ -> ok (Float (v1 -.@ v2))
            | Mul _ -> ok (Float (v1 *.@ v2))
            | Div _ -> ok (Float (v1 /.@ v2))
            | Rem _ -> ok (Float (Typed.Float.rem v1 v2))
            | _ ->
                Fmt.kstr not_impl "Unsupported float binary operator (%a)"
                  Expressions.pp_binop op)
        | v1, v2 ->
            Fmt.kstr not_impl
              "Unsupported values for binary operator (%a): %a / %a"
              Expressions.pp_binop op pp_rust_val v1 pp_rust_val v2)
    | NullaryOp (op, ty) -> (
        match op with
        | UbChecks ->
            (* See https://doc.rust-lang.org/std/intrinsics/fn.ub_checks.html
               Our execution already checks for UB, so we should return
               false, to indicate runtime UB checks aren't needed. *)
            ok (Int (BV.of_bool Typed.v_false))
        | OverflowChecks ->
            (* See https://doc.rust-lang.org/nightly/std/intrinsics/fn.overflow_checks.html
               Our execution already checks for overflows, so we don't need them at runtime. *)
            ok (Int (BV.of_bool Typed.v_false))
        | ContractChecks ->
            (* For now we don't do contracts. *)
            ok (Int (BV.of_bool Typed.v_false))
        | SizeOf ->
            let+ size = Layout.size_of ty in
            Int size
        | AlignOf ->
            let+ align = Layout.align_of ty in
            Int (align :> T.sint Typed.t)
        | OffsetOf (ty, variant, field) ->
            let variant = Option.value variant ~default:Types.VariantId.zero in
            let field = Types.FieldId.to_int field in
            let ty : Types.ty = TAdt ty in
            let+ layout = Layout.layout_of ty in
            let fields =
              Layout.Fields_shape.shape_for_variant variant layout.fields
            in
            let inner_off = Layout.Fields_shape.offset_of field fields in
            Int inner_off)
    | Discriminant place -> (
        let* loc = resolve_place place in
        match place.ty with
        | TAdt { id = TAdtId enum; _ } when Crate.is_enum enum ->
            let variants = Crate.as_enum enum in
            let+ variant_id = State.load_discriminant loc place.ty in
            let variant = Types.VariantId.nth variants variant_id in
            Int (BV.of_literal variant.discriminant)
        (* If a type doesn't have variants, return 0.
           https://doc.rust-lang.org/std/intrinsics/fn.discriminant_value.html *)
        | _ -> ok (Int U8.(0s)))
    (* Enum aggregate *)
    | Aggregate (AggregatedAdt ({ id = TAdtId t_id; _ }, Some v_id, None), vals)
      ->
        let variants = Crate.as_enum t_id in
        let variant = Types.VariantId.nth variants v_id in
        let discr = BV.of_literal variant.discriminant in
        let+ vals = eval_operand_list vals in
        Enum (discr, vals)
    (* Union aggregate *)
    | Aggregate (AggregatedAdt (ty, None, Some field), ops) ->
        let op =
          match ops with
          | [ op ] -> op
          | [] -> failwith "union aggregate with 0 values?"
          | _ :: _ -> failwith "union aggregate with >1 values?"
        in
        let* value = eval_operand op in
        let field = Types.FieldId.to_int field in
        let* layout = Layout.layout_of (TAdt ty) in
        let offset = Layout.Fields_shape.offset_of field layout.fields in
        let+ op_blocks =
          Encoder.rust_to_cvals ~offset value (type_of_operand op)
        in
        Union op_blocks
    (* Tuple aggregate *)
    | Aggregate (AggregatedAdt ({ id = TTuple; _ }, None, None), operands) ->
        let+ values = eval_operand_list operands in
        Tuple values
    (* Struct aggregate *)
    | Aggregate (AggregatedAdt ({ id = TAdtId t_id; _ }, None, None), operands)
      ->
        let type_decl = Crate.get_adt t_id in
        let* values = eval_operand_list operands in
        let+ () =
          match values with
          | [ v ] ->
              let attribs = type_decl.item_meta.attr_info.attributes in
              State.lift_err @@ Layout.apply_attributes v attribs
          | _ -> ok ()
        in
        Tuple values
    (* Invalid aggregate (not sure, but seems like it) *)
    | Aggregate ((AggregatedAdt _ as v), _) ->
        Fmt.failwith "Invalid ADT aggregate kind: %a"
          Expressions.pp_aggregate_kind v
    (* Array aggregate *)
    | Aggregate (AggregatedArray (_ty, _size), operands) ->
        let+ values = eval_operand_list operands in
        Tuple values
    (* Raw pointer construction *)
    | Aggregate (AggregatedRawPtr (_, _), operands) ->
        let* values = eval_operand_list operands in
        let ptr, meta =
          match values with
          | [ ptr; meta ] -> (ptr, meta)
          | _ -> failwith "Non-2 arguments in AggregatedRawPtr?"
        in
        let* ptr =
          match ptr with
          | Ptr (ptr, _) -> ok ptr
          | Int v ->
              let v = Typed.cast_i Usize v in
              ok (Sptr.null_ptr_of v)
          | _ ->
              Fmt.kstr not_impl "Unexpected ptr in AggregatedRawPtr: %a"
                pp_rust_val ptr
        in
        (* we flatten the meta, to simplify processing stuff like [std::ptr::DynMetadata] *)
        let+ meta =
          match Rust_val.flatten meta with
          | [] -> ok Thin
          | [ Int meta ] -> ok (Len (Typed.cast_i Usize meta))
          | [ Ptr (ptr, Thin) ] -> ok (VTable ptr)
          | elms ->
              Fmt.kstr not_impl "Unexpected meta in AggregatedRawPtr: %a"
                Fmt.(list ~sep:comma pp_rust_val)
                elms
        in
        Ptr (ptr, meta)
    (* Array repetition *)
    | Repeat (value, _, len) ->
        let+ value = eval_operand value in
        let len = int_of_const_generic len in
        (* FIXME: this is horrible for large arrays! *)
        let els = List.init len (fun _ -> value) in
        Tuple els
    (* Shallow init box -- get the pointer and transmute it to a box *)
    | ShallowInitBox (ptr, _) ->
        let+ ptr = eval_operand ptr in
        Std_funs.Std._mk_box ptr
    (* Length of a &[T;N] or &[T] *)
    | Len (place, _, size_opt) -> (
        let* _, meta = resolve_place place in
        match (meta, size_opt) with
        | _, Some size -> ok (Int (BV.usize_of_const_generic size))
        | Len len, None -> ok (Int len)
        | _ -> not_impl "Unexpected len rvalue")

  and exec_stmt (stmt : UllbcAst.statement) : unit t =
    L.info (fun m -> m "Statement: %a" Crate.pp_statement stmt);
    L.trace (fun m ->
        m "Statement full:@.%a" UllbcAst.pp_statement_kind stmt.kind);
    let@ () = with_loc ~loc:stmt.span.data in
    match stmt.kind with
    | Nop -> ok ()
    | Assign (place, rval) ->
        let* ptr = resolve_place place in
        let* v = eval_rvalue rval in
        L.info (fun m -> m "Assigning %a <- %a" pp_full_ptr ptr pp_rust_val v);
        State.store ptr place.ty v
    | StorageLive local ->
        let* ptr, ty = get_variable_and_ty local in
        let* () = match ptr with None -> ok () | Some ptr -> State.free ptr in
        let* ptr = State.alloc_ty ty in
        map_env (Store.add local (Some ptr, ty))
    | StorageDead local -> (
        let* ptr, ty = get_variable_and_ty local in
        match ptr with
        | Some ptr ->
            let* () = State.free ptr in
            map_env (Store.add local (None, ty))
        | None -> ok ())
    | Assert { cond; expected; on_failure } -> (
        let* cond = eval_operand cond in
        let cond_int = as_base TBool cond in
        let cond_bool = BV.to_bool cond_int in
        let cond_bool = if expected then cond_bool else Typed.not cond_bool in
        if%sat cond_bool then ok ()
        else
          match on_failure with
          | UndefinedBehavior -> error `UBAbort
          | UnwindTerminate -> error `UnwindTerminate
          | Panic name ->
              let name = Option.map (Fmt.to_to_string Crate.pp_name) name in
              error (`Panic name))
    | CopyNonOverlapping { src; dst; count } ->
        let ty = get_pointee (type_of_operand src) in
        let* src = eval_operand src in
        let* dst = eval_operand dst in
        let* count = eval_operand count in
        let src = as_ptr_or ~make:Sptr.null_ptr_of src in
        let dst = as_ptr_or ~make:Sptr.null_ptr_of dst in
        let count = as_base_i Usize count in
        let* () =
          with_env ~env:()
          @@ Std_funs.Intrinsics.copy_nonoverlapping ~t:ty ~src ~dst ~count
        in
        ok ()
    | Deinit place ->
        let* place_ptr = resolve_place place in
        State.uninit place_ptr place.ty
    | SetDiscriminant (_, _) ->
        not_impl "Unsupported statement: SetDiscriminant"

  and exec_block ~(body : UllbcAst.expr_body)
      ({ statements; terminator } : UllbcAst.block) =
    let^ () = Rustsymex.consume_fuel_steps 1 in
    let* () = fold_list statements ~init:() ~f:(fun () -> exec_stmt) in
    L.info (fun f -> f "Terminator: %a" Crate.pp_terminator terminator);
    let@ () = with_loc ~loc:terminator.span.data in
    match terminator.kind with
    | Call ({ func; args; dest }, target, on_unwind) ->
        let in_tys = List.map type_of_operand args in
        let* exec_fun, exp_tys =
          resolve_function ~in_tys ~out_ty:dest.ty func
        in
        (* the expected types of the function may differ to those passed, e.g. with
           function pointers or dyn calls, so we transmute here. *)
        let* args =
          fold_list (List.combine3 args in_tys exp_tys) ~init:[]
            ~f:(fun acc (arg, from_ty, to_ty) ->
              let* arg = eval_operand arg in
              if Types.equal_ty from_ty to_ty then ok (arg :: acc)
              else
                let+ arg = Core.transmute ~from_ty ~to_ty arg in
                arg :: acc)
        in
        let args = List.rev args in
        L.info (fun g ->
            g "Executing function with arguments [%a]"
              Fmt.(list ~sep:(any ", ") pp_rust_val)
              args);
        let fun_exec =
          with_extra_call_trace ~loc:terminator.span.data ~msg:"Call trace"
          @@ with_env ~env:()
          @@ exec_fun args
        in
        State.unwind_with fun_exec
          ~f:(fun v ->
            let* ptr = resolve_place dest in
            L.info (fun m ->
                m "Returned %a <- %a from %a" Crate.pp_place dest pp_rust_val v
                  Crate.pp_fn_operand func);
            let* () = State.store ptr dest.ty v in
            let block = UllbcAst.BlockId.nth body.body target in
            exec_block ~body block)
          ~fe:(fun err ->
            let* () = State.add_error err in
            L.info (fun m -> m "Unwinding from %a" Crate.pp_fn_operand func);
            let block = UllbcAst.BlockId.nth body.body on_unwind in
            exec_block ~body block)
    | Goto b ->
        let block = UllbcAst.BlockId.nth body.body b in
        exec_block ~body block
    | Return ->
        let* ptr, ty = get_variable_and_ty Expressions.LocalId.zero in
        let* ptr = of_opt_not_impl "Return value unset, but returned" ptr in
        let* value = State.load ptr ty in
        let ptr_tys = Layout.ref_tys_in value ty in
        let+ () =
          fold_list ptr_tys ~init:() ~f:(fun () (ptr, ty) ->
              State.tb_load ptr ty)
        in
        value
    | Switch (discr, switch) -> (
        let* discr = eval_operand discr in
        match switch with
        | If (if_block, else_block) ->
            L.info (fun g ->
                g "Switch if/else %a/%a for %a" UllbcAst.pp_block_id if_block
                  UllbcAst.pp_block_id else_block pp_rust_val discr);
            let bool_discr =
              match discr with
              | Int discr -> BV.to_bool (Typed.cast_lit TBool discr)
              | Ptr (ptr, _) -> Typed.not (Sptr.sem_eq ptr (Sptr.null_ptr ()))
              | _ -> failwith "Expected base value for if discriminant"
            in
            if%sat [@lname "if case"] [@rname "else case"] bool_discr then
              let block = UllbcAst.BlockId.nth body.body if_block in
              exec_block ~body block
            else
              let block = UllbcAst.BlockId.nth body.body else_block in
              exec_block ~body block
        | SwitchInt (_, options, default) ->
            L.info (fun g ->
                let options =
                  List.map
                    (fun (v, b) -> (PrintValues.literal_to_string v, b))
                    options
                in
                g "Switch options %a (else %a) for %a"
                  Fmt.(
                    list ~sep:comma
                    @@ pair ~sep:(any "->") string UllbcAst.pp_block_id)
                  options UllbcAst.pp_block_id default pp_rust_val discr);
            let compare_discr =
              match discr with
              | Int discr -> fun (v, _) -> discr ==@ BV.of_literal v
              | Ptr (ptr, _) ->
                  fun (v, _) ->
                    if Z.equal Z.zero (z_of_literal v) then
                      Sptr.sem_eq ptr (Sptr.null_ptr ())
                    else failwith "Can't compare pointer with non-0 scalar"
              | _ ->
                  fun (v, _) ->
                    Fmt.failwith
                      "Didn't know how to compare discriminant %a with scalar \
                       %s"
                      pp_rust_val discr
                      (PrintValues.literal_to_string v)
            in
            let^ block = match_on options ~constr:compare_discr in
            let block = Option.fold ~none:default ~some:snd block in
            let block = UllbcAst.BlockId.nth body.body block in
            exec_block ~body block)
    | Drop (drop_kind, place, trait_ref, target, on_unwind) -> (
        assert (drop_kind = Precise);
        let* place_ptr = resolve_place place in
        match trait_ref.kind with
        | TraitImpl impl_ref ->
            let impl = Crate.get_trait_impl impl_ref.id in
            (* The Drop trait will only have the drop function *)
            let _, drop_ref = List.hd impl.methods in
            let drop = Crate.get_fun drop_ref.binder_value.id in
            let fun_exec =
              with_extra_call_trace ~loc:terminator.span.data ~msg:"Drop"
              @@ with_env ~env:()
              @@ exec_fun drop [ Ptr place_ptr ]
            in
            State.unwind_with fun_exec
              ~f:(fun _ ->
                let block = UllbcAst.BlockId.nth body.body target in
                L.info (fun m ->
                    m "Dropped with %a" Crate.pp_name drop.item_meta.name);
                exec_block ~body block)
              ~fe:(fun err ->
                let* () = State.add_error err in
                L.info (fun m ->
                    m "Unwinding drop from %a" Crate.pp_name drop.item_meta.name);
                let block = UllbcAst.BlockId.nth body.body on_unwind in
                exec_block ~body block)
        | _ ->
            let* () = State.uninit place_ptr place.ty in
            let block = UllbcAst.BlockId.nth body.body target in
            exec_block ~body block)
    | Abort kind -> (
        match kind with
        | UndefinedBehavior -> error `UBAbort
        | UnwindTerminate -> error `UnwindTerminate
        | Panic name ->
            let name = Option.map (Fmt.to_to_string Crate.pp_name) name in
            error (`Panic name))
    | UnwindResume -> State.pop_error ()

  and exec_fun (fundef : UllbcAst.fun_decl) args =
    let name = fundef.item_meta.name in
    let* body =
      match fundef.body with
      | None -> Fmt.kstr not_impl "Function %a is opaque" Crate.pp_name name
      | Some body -> ok body
    in
    let@@ () = with_env ~env:Store.empty in
    let@ () = with_loc ~loc:fundef.item_meta.span.data in
    L.info (fun m ->
        m "Calling %a with %a" Crate.pp_name name
          Fmt.(hbox @@ brackets @@ list ~sep:comma pp_rust_val)
          args);
    let* protected = alloc_stack body.locals args in
    let starting_block = List.hd body.body in
    let exec_block = exec_block ~body starting_block in
    State.unwind_with exec_block
      ~f:(fun value ->
        let protected_address =
          match (fundef.signature.output, value) with
          | (TRef (RStatic, _, _) | TRawPtr _), Ptr (addr, _) -> Some addr
          | _ -> None
        in
        let+ () = dealloc_store ?protected_address protected in
        value)
      ~fe:(fun err ->
        let* () = dealloc_store protected in
        error_raw err)

  (* re-define this for the export, nowhere else: *)
  let exec_fun ~args ~state (fundef : UllbcAst.fun_decl) =
    let@ () = Rust_state_m.run ~env:() ~state in
    let@@ () =
      with_extra_call_trace ~loc:fundef.item_meta.span.data ~msg:"Entry point"
    in
    let* value = exec_fun fundef args in
    if !Config.current.ignore_leaks then ok value
    else
      let@ () = with_loc ~loc:fundef.item_meta.span.data in
      let+ () = State.leak_check () in
      value
end
