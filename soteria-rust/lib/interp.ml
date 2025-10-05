open Rustsymex
module BV = Typed.BitVec
open Typed.Syntax
open Typed.Infix
open Charon
open Charon_util
open Rust_val

module Make (State : State_intf.S) = struct
  module InterpM = State_monad.Make (State)
  module Core = Core.M (State)
  module Std_funs = Builtins.Eval.M (State)
  module Sptr = State.Sptr
  module Encoder = Encoder.Make (Sptr)

  let pp_rust_val = pp_rust_val Sptr.pp

  exception Unsupported of (string * Meta.span)

  open InterpM
  open InterpM.Syntax

  type state = State.t
  type store = (full_ptr option * Types.ty) Store.t
  type 'a t = ('a, store) InterpM.t
  type 'err fun_exec = rust_val list -> (rust_val, unit) InterpM.t

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
            update_ref_tys_in value ty ~init:protected
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

  let resolve_constant (const : Expressions.constant_expr) =
    match const.kind with
    | CLiteral (VScalar scalar) -> ok (Base (BV.of_scalar scalar))
    | CLiteral (VBool b) -> ok (Base (BV.of_bool (Typed.bool b)))
    | CLiteral (VChar c) -> ok (Base (BV.u32i (Uchar.to_int c)))
    | CLiteral (VFloat { float_value; float_ty }) ->
        ok (Base (Typed.Float.mk float_ty float_value))
    | CLiteral (VStr str) -> (
        let* ptr_opt = State.load_str_global str in
        match ptr_opt with
        | Some v -> ok (Ptr v)
        | None ->
            (* We "cheat" and model strings as an array of chars, with &str a slice *)
            let len = String.length str in
            let chars =
              String.to_bytes str
              |> Bytes.fold_left
                   (fun l c -> Base (BV.u8i (Char.code c)) :: l)
                   []
              |> List.rev
            in
            let char_arr = Array chars in
            let str_ty : Types.ty =
              mk_array_ty (TLiteral (TUInt U8)) (Z.of_int len)
            in
            let* ptr, _ = State.alloc_ty str_ty in
            let ptr = (ptr, Some (BV.usizei len)) in
            let* () = State.store ptr str_ty char_arr in
            let+ () = State.store_str_global str ptr in
            Ptr ptr)
    | CFnPtr fn_ptr -> ok (ConstFn fn_ptr)
    | CLiteral (VByteStr _) -> not_impl "TODO: resolve const ByteStr"
    (* FIXME: this is hacky, but until we get proper monomorphisation this isn't too bad *)
    | CTraitConst (tref, "IS_ZST") ->
        let ty = List.hd tref.trait_decl_ref.binder_value.generics.types in
        let^+ size = Layout.size_of_s ty in
        Base (BV.of_bool (size ==@ Usize.(0s)))
    | CTraitConst (tref, "LAYOUT") ->
        let ty = List.hd tref.trait_decl_ref.binder_value.generics.types in
        let^ size = Layout.size_of_s ty in
        let^+ align = Layout.align_of_s ty in
        (* The alignment is a struct storing a value of the enum AlignmentEnum, where the
           discriminant's value for variant N is 1 << N. *)
        Struct [ Base size; Struct [ Enum (align, []) ] ]
    | CTraitConst (_, name) ->
        Fmt.kstr not_impl "TODO: resolve const TraitConst (%s)" name
    | CRawMemory bytes ->
        let value = List.map (fun x -> Base (BV.u8i x)) bytes in
        let value = Array value in
        let from_ty =
          mk_array_ty (TLiteral (TUInt U8)) (Z.of_int @@ List.length bytes)
        in
        let^^+ value = Encoder.transmute value ~from_ty ~to_ty:const.ty in
        value
    | COpaque msg -> Fmt.kstr not_impl "Opaque constant: %s" msg
    | CVar _ -> not_impl "TODO: resolve const Var (mono error)"

  (** Resolves a place to a pointer, in the form of a rust_val. We use rust_val
      rather than T.sptr Typed.t, to be able to handle fat pointers; however
      there is the guarantee that this function returns either a Base or a
      FatPointer value. *)
  let rec resolve_place ({ kind; ty } : Expressions.place) : full_ptr t =
    match kind with
    (* Just a local *)
    | PlaceLocal v -> get_variable v
    (* Just a global *)
    | PlaceGlobal g -> resolve_global g.id
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) -> (
        let* ptr = resolve_place base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" pp_full_ptr ptr pp_ty base.ty);
        let* v = State.load ptr base.ty in
        match v with
        | Ptr ((ptr_in, _) as fptr) -> (
            L.debug (fun f ->
                f "Dereferenced pointer %a to pointer %a" pp_full_ptr ptr
                  pp_full_ptr fptr);
            let pointee = Charon_util.get_pointee base.ty in
            match base.ty with
            | TRef _ | TAdt { id = TBuiltin TBox; _ } ->
                let+ () = State.check_ptr_align ptr_in pointee in
                fptr
            | _ -> ok fptr)
        | Base off ->
            let off = Typed.cast_i Usize off in
            let ptr = Sptr.null_ptr_of off in
            ok (ptr, None)
        | _ -> not_impl "Unexpected value when dereferencing place")
    (* The metadata of a pointer type is just the second part of the pointer *)
    | PlaceProjection (base, PtrMetadata) ->
        let* ptr, _ = resolve_place base in
        let^^+ ptr' =
          Sptr.offset ~check:false ~ty:(TLiteral (TUInt Usize)) ~signed:false
            ptr
            Usize.(1s)
        in
        (ptr', None)
    | PlaceProjection (base, Field (kind, field)) ->
        let* ptr, meta = resolve_place base in
        let* () = State.check_ptr_align ptr base.ty in
        L.debug (fun f ->
            f "Projecting field %a (kind %a) for %a" Types.pp_field_id field
              Expressions.pp_field_proj_kind kind Sptr.pp ptr);
        let^^ ptr' = Sptr.project base.ty kind field ptr in
        L.debug (fun f ->
            f
              "Dereferenced ADT projection %a, field %a, with pointer %a to \
               pointer %a"
              Expressions.pp_field_proj_kind kind Types.pp_field_id field
              Sptr.pp ptr Sptr.pp ptr');
        if not @@ Layout.is_inhabited ty then error `RefToUninhabited
        else ok (ptr', meta)
    | PlaceProjection (base, ProjIndex (idx, from_end)) ->
        let* ptr, meta = resolve_place base in
        let len =
          match (meta, base.ty) with
          (* Array with static size *)
          | ( None,
              TAdt
                {
                  id = TBuiltin TArray;
                  generics = { const_generics = [ len ]; _ };
                } ) ->
              BV.usize_of_const_generic len
          | Some len, TAdt { id = TBuiltin TSlice; _ } -> Typed.cast_i Usize len
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let* idx = eval_operand idx in
        let idx = as_base_i Usize idx in
        let idx = if from_end then len -!@ idx else idx in
        let* () =
          State.assert_ (Usize.(0s) <=$@ idx &&@ (idx <$@ len)) `OutOfBounds
        in
        let^^+ ptr' = Sptr.offset ~signed:false ~ty ptr idx in
        L.debug (fun f ->
            f "Projected %a, index %a, to pointer %a" Sptr.pp ptr Typed.ppa idx
              Sptr.pp ptr');
        (ptr', None)
    | PlaceProjection (base, Subslice (from, to_, from_end)) ->
        let* ptr, meta = resolve_place base in
        let ty, len =
          match (meta, base.ty) with
          (* Array with static size *)
          | ( None,
              TAdt
                {
                  id = TBuiltin TArray;
                  generics = { const_generics = [ len ]; types = [ ty ]; _ };
                } ) ->
              (ty, BV.usize_of_const_generic len)
          | ( Some len,
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
        let^^+ ptr' = Sptr.offset ~signed:false ~ty ptr from in
        let slice_len = to_ -!@ from in
        L.debug (fun f ->
            f "Projected %a, slice %a..%a%s, to pointer %a, len %a" Sptr.pp ptr
              Typed.ppa from Typed.ppa to_
              (if from_end then "(from end)" else "")
              Sptr.pp ptr' Typed.ppa slice_len);
        (ptr', Some slice_len)

  (** Resolve a function operand, returning a callable symbolic function to
      execute it.

      This function also handles validating the call; given the input types it
      will be called with and the output type expected, it will make sure these
      are the right types and in the right amount. *)
  and resolve_function ~in_tys ~out_ty : GAst.fn_operand -> 'err fun_exec t =
    function
    (* For static calls we don't need to check types, that's what the type checker does. *)
    | FnOpRegular { kind = FunId (FRegular fid); _ }
    | FnOpRegular { kind = TraitMethod (_, _, fid); _ } -> (
        try
          let fundef = Crate.get_fun fid in
          L.info (fun g ->
              g "Resolved function call to %a" Crate.pp_name
                fundef.item_meta.name);
          match Std_funs.std_fun_eval fundef exec_fun with
          | Some fn -> ok fn
          | None -> ok (exec_fun fundef)
        with Crate.MissingDecl _ -> not_impl "Missing function declaration")
    | FnOpRegular { kind = FunId (FBuiltin fn); generics } ->
        ok (Std_funs.builtin_fun_eval fn generics)
    (* Here we need to check the type of the actual function, as it could have been cast. *)
    | FnOpMove place ->
        let* fn_ptr_ptr = resolve_place place in
        let* fn_ptr = State.load ~is_move:true fn_ptr_ptr place.ty in
        let* fn_ptr =
          match fn_ptr with Ptr ptr -> ok ptr | _ -> error `UBDanglingPointer
        in
        let* fn = State.lookup_fn fn_ptr in
        let* () =
          match fn.kind with
          | FunId (FRegular fid) | TraitMethod (_, _, fid) ->
              let fn = Crate.get_fun fid in
              let rec check_tys l r =
                match (l, r) with
                | [], [] -> ok ()
                | ty1 :: l, ty2 :: r ->
                    if Layout.is_abi_compatible ty1 ty2 then check_tys l r
                    else error (`InvalidFnArgTys (ty1, ty2))
                | _ ->
                    error
                      (`InvalidFnArgCount
                         (List.length in_tys, List.length fn.signature.inputs))
              in
              check_tys (out_ty :: in_tys)
                (fn.signature.output :: fn.signature.inputs)
          | FunId (FBuiltin _) -> ok ()
        in
        let fnop : GAst.fn_operand = FnOpRegular fn in
        resolve_function ~in_tys ~out_ty fnop

  (** Resolves a global into a *pointer* Rust value to where that global is *)
  and resolve_global (g : Types.global_decl_id) =
    let decl = Crate.get_global g in
    let* v_opt = State.load_global g in
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
        let* ptr = State.alloc_ty decl.ty in
        let* () = State.store_global g ptr in
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
            let+ () = State.check_ptr_align (fst ptr) ty in
            zst
        | None ->
            let is_move =
              (* TODO: properly detect if ty has the Copy trait, in which case is_move is
             always false. *)
              match (op, ty) with
              | _, TLiteral _ -> false
              | Move _, _ -> true
              | _ -> false
            in
            State.load ~is_move ptr ty)

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
            | TBool -> ok (Base (BV.not_bool v))
            | TInt _ | TUInt _ -> ok (Base (BV.not v))
            | ty ->
                Fmt.kstr not_impl "Unexpect type in UnaryOp.Neg: %a" pp_ty
                  (TLiteral ty))
        | Neg _ -> (
            match type_of_operand e with
            | TLiteral ((TInt _ | TUInt _) as ty) ->
                let v = as_base ty v in
                let res, overflowed = ~-?v in
                let+ () = State.assert_not overflowed `Overflow in
                Base res
            | TLiteral (TFloat fty) ->
                let v = as_base_f fty v in
                ok (Base (Typed.Float.neg v))
            | _ -> not_impl "Invalid type for Neg")
        | Cast (CastRawPtr (_from, _to)) -> ok v
        | Cast (CastTransmute (from_ty, to_ty)) ->
            let* verify_ptr = State.is_valid_ptr_fn in
            State.lift_err @@ Encoder.transmute ~verify_ptr ~from_ty ~to_ty v
        | Cast (CastScalar (from_ty, to_ty)) ->
            State.lift_err @@ Encoder.transmute_literal ~from_ty ~to_ty v
        | Cast (CastUnsize (_, _, MetaVTablePtr _)) ->
            not_impl "Unsupported: dyn"
        | Cast (CastUnsize (_, _, MetaUnknown)) ->
            not_impl "Unknown unsize kind"
        | Cast (CastUnsize (_, _, MetaLength length)) ->
            let rec with_ptr_meta meta : rust_val -> rust_val t = function
              | Ptr (v, _) -> ok (Ptr (v, Some meta))
              | ( Struct (_ :: _ as fs)
                | Array (_ :: _ as fs)
                | Tuple (_ :: _ as fs) ) as v -> (
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
                      let+ nonempty = with_ptr_meta meta nonempty in
                      let fs = List.rev (left @ [ nonempty ] @ right) in
                      match v with
                      | Struct _ -> Struct fs
                      | Array _ -> Array fs
                      | Tuple _ -> Tuple fs
                      | _ -> assert false)
                  | None -> not_impl "Couldn't set pointer meta in CastUnsize")
              | _ -> not_impl "Couldn't set pointer meta in CastUnsize"
            in
            let size = BV.usize_of_const_generic length in
            with_ptr_meta size v
        | Cast (CastFnPtr (_from, _to)) -> (
            match v with
            | ConstFn fn_ptr ->
                let+ ptr = State.declare_fn fn_ptr in
                Ptr ptr
            | Ptr _ as ptr -> ok ptr
            | _ -> not_impl "Invalid argument to CastFnPtr"))
    | BinaryOp (op, e1, e2) -> (
        let* v1 = eval_operand e1 in
        let* v2 = eval_operand e2 in
        match (v1, v2) with
        | Base v1, Base v2 -> (
            match op with
            | Ge | Gt | Lt | Le -> (
                let v1, v2, ty = Typed.cast_checked2 v1 v2 in
                match Typed.untype_type ty with
                | TBitVector _ ->
                    let lit_ty =
                      TypesUtils.ty_as_literal (type_of_operand e1)
                    in
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
                    ok (Base v)
                | TFloat _ ->
                    let op =
                      match op with
                      | Ge -> Typed.Float.geq
                      | Gt -> Typed.Float.gt
                      | Lt -> Typed.Float.lt
                      | Le -> Typed.Float.leq
                      | _ -> assert false
                    in
                    let v1, v2 = (Typed.cast v1, Typed.cast v2) in
                    let v = op v1 v2 |> BV.of_bool in
                    ok (Base v)
                | TPointer _ -> error `UBPointerComparison
                | _ -> assert false)
            | Eq | Ne ->
                let v1, v2, _ = Typed.cast_checked2 v1 v2 in
                let^^+ res = Core.equality_check v1 v2 in
                let res = if op = Eq then res else BV.not_bool res in
                Base (res :> T.cval Typed.t)
            | Add _ | Sub _ | Mul _ | Div _ | Rem _ | Shl _ | Shr _ ->
                let ty = TypesUtils.ty_as_literal (type_of_operand e1) in
                let^^+ res = Core.eval_lit_binop op ty v1 v2 in
                Base res
            | AddChecked | SubChecked | MulChecked ->
                let ty =
                  match type_of_operand e1 with
                  | TLiteral ty -> ty
                  | ty -> Fmt.failwith "Unexpected type in binop: %a" pp_ty ty
                in
                State.lift_err @@ Core.eval_checked_lit_binop op ty v1 v2
            | Cmp ->
                let v1, v2, ty = Typed.cast_checked2 v1 v2 in
                if Typed.equal_ty ty (Typed.t_ptr ()) then
                  error `UBPointerComparison
                else
                  let ty = type_of_operand e1 in
                  let ty = TypesUtils.ty_as_literal ty in
                  let^+ cmp = Core.cmp ~signed:(Layout.is_signed ty) v1 v2 in
                  Base cmp
            | Offset ->
                (* non-zero offset on integer pointer is not permitted, as these are always
                   dangling *)
                let v2 = Typed.cast_i Usize v2 in
                let ty = Charon_util.get_pointee (type_of_operand e1) in
                let^ size = Layout.size_of_s ty in
                let+ () =
                  State.assert_
                    (v2 ==@ Usize.(0s) ||@ (size ==@ Usize.(0s)))
                    `UBDanglingPointer
                in
                Base v1
            | BitOr | BitAnd | BitXor -> (
                let ty = TypesUtils.ty_as_literal (type_of_operand e1) in
                let v1 = Typed.cast_lit ty v1 in
                let v2 = Typed.cast_lit ty v2 in
                match op with
                | BitOr -> ok (Base (v1 |@ v2))
                | BitAnd -> ok (Base (v1 &@ v2))
                | BitXor -> ok (Base (v1 ^@ v2))
                | _ -> assert false))
        | ((Ptr _ | Base _) as p1), ((Ptr _ | Base _) as p2) -> (
            match op with
            | Offset ->
                let^ p, meta, v =
                  match (p1, p2) with
                  | Ptr (p, meta), Base v -> return (p, meta, v)
                  | _ -> Rustsymex.not_impl "Invalid operands in offset"
                in
                let ty = Charon_util.get_pointee (type_of_operand e1) in
                let v = Typed.cast_i Usize v in
                let off_ty = TypesUtils.ty_as_literal (type_of_operand e2) in
                let signed = Layout.is_signed off_ty in
                let^^+ p' = Sptr.offset ~signed ~ty p v in
                Ptr (p', meta)
            | _ ->
                let^^+ res = Core.eval_ptr_binop op p1 p2 in
                Base res)
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
            ok (Base (BV.of_bool Typed.v_false))
        | SizeOf ->
            let^+ size = Layout.size_of_s ty in
            Base size
        | AlignOf ->
            let^+ align = Layout.align_of_s ty in
            Base align
        | OffsetOf _ ->
            Fmt.kstr not_impl "Unsupported nullary operator: %a"
              Expressions.pp_nullop op)
    | Discriminant place -> (
        let* loc = resolve_place place in
        match place.ty with
        | TAdt { id = TAdtId enum; _ } when Crate.is_enum enum ->
            let variants = Crate.as_enum enum in
            let+ variant_id = State.load_discriminant loc place.ty in
            let variant = Types.VariantId.nth variants variant_id in
            Base (BV.of_literal variant.discriminant)
        (* If a type doesn't have variants, return 0.
           https://doc.rust-lang.org/std/intrinsics/fn.discriminant_value.html *)
        | _ -> ok (Base U8.(0s)))
    (* Enum aggregate *)
    | Aggregate (AggregatedAdt ({ id = TAdtId t_id; _ }, Some v_id, None), vals)
      ->
        let variants = Crate.as_enum t_id in
        let variant = Types.VariantId.nth variants v_id in
        let discr = BV.of_literal variant.discriminant in
        let+ vals = eval_operand_list vals in
        Enum (discr, vals)
    (* Union aggregate *)
    | Aggregate (AggregatedAdt (_, None, Some field), ops) ->
        let op =
          match ops with
          | [ op ] -> op
          | [] -> failwith "union aggregate with 0 values?"
          | _ :: _ -> failwith "union aggregate with >1 values?"
        in
        let+ value = eval_operand op in
        Union (field, value)
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
        Struct values
    (* Invalid aggregate (not sure, but seems like it) *)
    | Aggregate ((AggregatedAdt _ as v), _) ->
        Fmt.failwith "Invalid ADT aggregate kind: %a"
          Expressions.pp_aggregate_kind v
    (* Array aggregate *)
    | Aggregate (AggregatedArray (_ty, _size), operands) ->
        let+ values = eval_operand_list operands in
        Array values
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
          | Base v ->
              let v = Typed.cast_i Usize v in
              ok (Sptr.null_ptr_of v)
          | _ ->
              Fmt.kstr not_impl "Unexpected ptr in AggregatedRawPtr: %a"
                pp_rust_val ptr
        in
        let+ meta =
          match meta with
          | Tuple [] -> ok None
          | Base meta -> ok (Some meta)
          | _ ->
              Fmt.kstr not_impl "Unexpected meta in AggregatedRawPtr: %a"
                pp_rust_val meta
        in
        Ptr (ptr, meta)
    (* Array repetition *)
    | Repeat (value, _, len) ->
        let+ value = eval_operand value in
        let len = int_of_const_generic len in
        (* FIXME: this is horrible for large arrays! *)
        let els = List.init len (fun _ -> value) in
        Array els
    (* Shallow init box -- get the pointer and transmute it to a box *)
    | ShallowInitBox (ptr, _) ->
        let+ ptr = eval_operand ptr in
        Std_funs.Std._mk_box ptr
    (* Length of a &[T;N] or &[T] *)
    | Len (place, _, size_opt) -> (
        let* _, meta = resolve_place place in
        match (meta, size_opt) with
        | _, Some size -> ok (Base (BV.usize_of_const_generic size))
        | Some len, None -> ok (Base len)
        | _ -> not_impl "Unexpected len rvalue")

  and exec_stmt stmt : unit t =
    L.info (fun m -> m "Statement: %a" Crate.pp_statement stmt);
    L.trace (fun m ->
        m "Statement full:@.%a" UllbcAst.pp_statement_kind stmt.kind);
    let { span = loc; kind = stmt; _ } : UllbcAst.statement = stmt in
    let@ () = with_loc ~loc in
    match stmt with
    | Nop -> ok ()
    | Assign (({ ty; _ } as place), rval) ->
        let* ptr = resolve_place place in
        let* v = eval_rvalue rval in
        L.info (fun m -> m "Assigning %a <- %a" pp_full_ptr ptr pp_rust_val v);
        State.store ptr ty v
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
    | Drop (place, trait_ref) -> (
        let* place_ptr = resolve_place place in
        if not !Config.current.monomorphize_experimental then
          let* () =
            match place.ty with
            | TAdt { id = TAdtId id; _ } -> (
                let adt = Crate.get_adt id in
                match
                  (adt.item_meta.lang_item, List.last adt.item_meta.name)
                with
                | Some "owned_box", PeMonomorphized { types = [ _; _ ]; _ } -> (
                    let* box = State.load place_ptr place.ty in
                    match box with
                    | Struct [ Struct [ Struct [ Ptr ptr ]; _ ]; _ ] ->
                        State.free ptr
                    | _ -> ok ())
                | _ -> ok ())
            | _ -> ok ()
          in
          State.uninit place_ptr place.ty
        else
          match trait_ref.kind with
          | TraitImpl impl_ref ->
              let impl = Crate.get_trait_impl impl_ref.id in
              (* The Drop trait will only have the drop function *)
              let _, drop_ref = List.hd impl.methods in
              let drop = Crate.get_fun drop_ref.binder_value.id in
              let fun_exec =
                with_extra_call_trace ~loc ~msg:"Drop"
                @@ with_env ~env:()
                @@ exec_fun drop [ Ptr place_ptr ]
              in
              State.unwind_with fun_exec
                ~f:(fun _ -> ok ())
                ~fe:(fun err -> error_raw err)
          | _ -> State.uninit place_ptr place.ty)
    | Assert { cond; expected; on_failure } -> (
        let* cond = eval_operand cond in
        let cond_int = as_base TBool cond in
        let cond_bool = BV.to_bool cond_int in
        let cond_bool =
          if expected = true then cond_bool else Typed.not cond_bool
        in
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
    let { span = loc; kind = term; _ } : UllbcAst.terminator = terminator in
    let@ () = with_loc ~loc in
    match term with
    | Call ({ func; args; dest = { ty; _ } as place }, target, on_unwind) ->
        let in_tys = List.map type_of_operand args in
        let out_ty = ty in
        let* exec_fun = resolve_function ~in_tys ~out_ty func in
        let* args = eval_operand_list args in
        L.info (fun g ->
            g "Executing function with arguments [%a]"
              Fmt.(list ~sep:(any ", ") pp_rust_val)
              args);
        let fun_exec =
          with_extra_call_trace ~loc ~msg:"Call trace"
          @@ with_env ~env:()
          @@ exec_fun args
        in
        State.unwind_with fun_exec
          ~f:(fun v ->
            let* ptr = resolve_place place in
            L.info (fun m ->
                m "Returned %a <- %a from %a" Crate.pp_place place pp_rust_val v
                  Crate.pp_fn_operand func);
            let* () = State.store ptr ty v in
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
            let* block =
              (* if a base value, compare with 0 -- if a pointer, check for null *)
              match discr with
              | Base discr ->
                  let discr, _ = Typed.cast_int discr in
                  if%sat [@lname "else case"] [@rname "if case"]
                    BV.to_bool discr
                  then ok if_block
                  else ok else_block
              | Ptr (ptr, _) ->
                  if%sat [@lname "else case"] [@rname "if case"]
                    Sptr.is_at_null_loc ptr
                  then ok else_block
                  else ok if_block
              | _ ->
                  Fmt.kstr not_impl
                    "Expected base value for discriminant, got %a" pp_rust_val
                    discr
            in
            let block = UllbcAst.BlockId.nth body.body block in
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
              | Base discr -> fun (v, _) -> discr ==@ BV.of_literal v
              | Ptr (ptr, _) ->
                  fun (v, _) ->
                    if Z.equal Z.zero (z_of_literal v) then
                      Sptr.is_at_null_loc ptr
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
    | Abort kind -> (
        match kind with
        | UndefinedBehavior -> error `UBAbort
        | UnwindTerminate -> error `UnwindTerminate
        | Panic name ->
            let name = Option.map (Fmt.to_to_string Crate.pp_name) name in
            error (`Panic name))
    | UnwindResume -> State.pop_error ()

  and exec_fun (fundef : UllbcAst.fun_decl) args : (rust_val, unit) InterpM.t =
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let* body =
      match body with
      | None -> Fmt.kstr not_impl "Function %a is opaque" Crate.pp_name name
      | Some body -> ok body
    in
    let@@ () = with_env ~env:Store.empty in
    let@ () = with_loc ~loc in
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
          | TRef (RStatic, _, RShared), Ptr (addr, _) -> Some addr
          | _ -> None
        in
        let+ () = dealloc_store ?protected_address protected in
        value)
      ~fe:(fun err ->
        let* () = dealloc_store protected in
        error_raw err)

  (* re-define this for the export, nowhere else: *)
  let exec_fun ~args ~state (fundef : UllbcAst.fun_decl) =
    let@ () = InterpM.run ~env:() ~state in
    let@@ () =
      with_extra_call_trace ~loc:fundef.item_meta.span ~msg:"Entry point"
    in
    let* value = exec_fun fundef args in
    if !Config.current.ignore_leaks then ok value
    else
      let@ () = with_loc ~loc:fundef.item_meta.span in
      let+ () = State.leak_check () in
      value
end
