open Soteria_symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
open Typed.Infix
open Typed.Syntax
open Charon
open Charon_util
module T = Typed.T

module InterpM (State : State_intf.S) = struct
  type full_ptr = State.Sptr.t Charon_util.full_ptr
  type store = (full_ptr option * Types.ty) Store.t

  type 'a t =
    store ->
    State.t ->
    ( 'a * store * State.t,
      Error.t State.err * State.t,
      State.serialized list )
    Result.t

  let ok x : 'a t = fun store state -> Result.ok (x, store, state)
  let error err : 'a t = fun _store state -> State.error err state
  let error_raw err : 'a t = fun _store state -> Result.error (err, state)
  let not_impl str : 'a t = fun _store _state -> Rustsymex.not_impl str
  let get_store () = fun store state -> Result.ok (store, store, state)

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
   fun store state ->
    let** y, store, state = x store state in
    (f y) store state

  let map (x : 'a t) (f : 'a -> 'b) : 'b t =
   fun store state ->
    let++ y, store, state = x store state in
    (f y, store, state)

  let fold_list x ~init ~f =
    Monad.foldM ~bind ~return:ok ~fold:Foldable.List.fold x ~init ~f

  let map_store f = fun store state -> Result.ok ((), f store, state)

  let[@inline] lift_state_op f =
   fun store state ->
    let++ v, state = f state in
    (v, store, state)

  let[@inline] lift_symex (s : 'a Rustsymex.t) : 'a t =
   fun store state ->
    let+ s = s in
    Ok (s, store, state)

  let of_opt_not_impl ~msg x = lift_symex (of_opt_not_impl ~msg x)

  let with_loc ~loc f =
    let old_loc = !Rustsymex.current_loc in
    Rustsymex.current_loc := loc;
    map (f ()) @@ fun res ->
    current_loc := old_loc;
    res

  let with_extra_call_trace ~loc ~msg (x : 'a t) : 'a t =
   fun store state ->
    let+ res = x store state in
    match res with
    | Ok triple -> Ok triple
    | Error (e, st) ->
        let elem = Soteria_terminal.Call_trace.mk_element ~loc ~msg () in
        Error (State.add_to_call_trace e elem, st)
    | Missing f -> Missing f

  let run ~store ~state (f : unit -> 'a t) : ('a * State.t, 'e, 'f) Result.t =
    let++ res, _, state = f () store state in
    (res, state)

  (** We painfully lift [Layout.update_ref_tys_in] to make it nicer to use
      without having to define it here. *)
  let update_ref_tys_in
      ~(f :
         'acc -> full_ptr -> Types.ty -> Types.ref_kind -> (full_ptr * 'acc) t)
      ~(init : 'acc) (v : 'a rust_val) (ty : Types.ty) :
      (State.Sptr.t rust_val * 'acc) t =
   fun store state ->
    let f (acc, store, state) ptr ty rk =
      let++ (res, acc), store, state = f acc ptr ty rk store state in
      (res, (acc, store, state))
    in
    let++ res, (acc, store, state) =
      Layout.update_ref_tys_in f (init, store, state) v ty
    in
    ((res, acc), store, state)

  module State = struct
    include State

    let[@inline] load ?is_move ptr ty = lift_state_op (load ?is_move ptr ty)
    let[@inline] store ptr ty v = lift_state_op (store ptr ty v)
    let[@inline] alloc_ty ty = lift_state_op (alloc_ty ty)
    let[@inline] alloc_tys tys = lift_state_op (alloc_tys tys)
    let[@inline] uninit ptr ty = lift_state_op (uninit ptr ty)
    let[@inline] free ptr = lift_state_op (free ptr)
    let[@inline] check_ptr_align ptr ty = lift_state_op (check_ptr_align ptr ty)
    let[@inline] borrow ptr ty mut = lift_state_op (borrow ptr ty mut)
    let[@inline] protect ptr ty mut = lift_state_op (protect ptr ty mut)
    let[@inline] unprotect ptr ty = lift_state_op (unprotect ptr ty)
    let[@inline] tb_load ptr ty = lift_state_op (tb_load ptr ty)
    let[@inline] load_global g = lift_state_op (load_global g)
    let[@inline] store_global g ptr = lift_state_op (store_global g ptr)
    let[@inline] load_str_global str = lift_state_op (load_str_global str)

    let[@inline] store_str_global str ptr =
      lift_state_op (store_str_global str ptr)

    let[@inline] declare_fn fn = lift_state_op (declare_fn fn)
    let[@inline] lookup_fn fn = lift_state_op (lookup_fn fn)
    let[@inline] add_error e = lift_state_op (add_error e)
    let[@inline] pop_error () = lift_state_op pop_error

    let[@inline] unwind_with ~f ~fe x =
     fun store state ->
      unwind_with
        ~f:(fun (x, store, state) -> f x store state)
        ~fe:(fun (e, state) -> fe e store state)
        (x store state)

    let[@inline] is_valid_ptr =
     fun store state -> Result.ok (is_valid_ptr state, store, state)

    let[@inline] lift_err sym =
     fun store state ->
      let++ res = lift_err state sym in
      (res, store, state)
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let^ ) x f = bind (lift_symex x) f
    let ( let^+ ) x f = map (lift_symex x) f
    let ( let^^ ) x f = bind (State.lift_err x) f
    let ( let^^+ ) x f = map (State.lift_err x) f

    module Symex_syntax = struct
      let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
       fun store state ->
        Rustsymex.branch_on ?left_branch_name ?right_branch_name guard
          ~then_:(fun () -> then_ () store state)
          ~else_:(fun () -> else_ () store state)
    end
  end
end

module Make (State : State_intf.S) = struct
  module InterpM = InterpM (State)
  module Core = Core.M (State)
  module Std_funs = Builtins.Eval.M (State)
  module Sptr = State.Sptr
  module Encoder = Encoder.Make (Sptr)

  let pp_rust_val = pp_rust_val Sptr.pp

  exception Unsupported of (string * Meta.span)

  type full_ptr = Sptr.t Charon_util.full_ptr
  type state = State.t
  type store = (full_ptr option * Types.ty) Store.t

  open InterpM
  open InterpM.Syntax

  let pp_full_ptr = Charon_util.pp_full_ptr Sptr.pp

  type 'err fun_exec =
    args:Sptr.t rust_val list ->
    state ->
    (Sptr.t rust_val * state, 'err, State.serialized list) Result.t

  let get_variable var_id =
    let* store = get_store () in
    match Store.find_value var_id store with
    | Some ptr ->
        L.debug (fun m ->
            m "Variable %a has pointer %a" Expressions.pp_var_id var_id
              pp_full_ptr ptr);
        ok ptr
    | None -> error `DeadVariable

  let get_variable_and_ty var_id =
    let* store = get_store () in
    match Store.find_opt var_id store with
    | Some ptr_and_ty -> ok ptr_and_ty
    | None -> error `DeadVariable

  let alloc_stack (locals : GAst.locals) args : (full_ptr * Types.ty) list t =
    if List.compare_length_with args locals.arg_count <> 0 then
      failwith
        "Function called with wrong arg count, should have been caught before";
    (* create a store with all types *)
    let* () =
      map_store @@ fun store ->
      List.fold_left
        (fun st (local : GAst.local) ->
          Store.add local.index (None, local.var_ty) st)
        store locals.locals
    in
    (* allocate arguments and return value, updating store *)
    let alloc_locs = List.take (1 + locals.arg_count) locals.locals in
    let tys =
      List.map (fun ({ var_ty; _ } : GAst.local) -> var_ty) alloc_locs
    in
    let* ptrs = State.alloc_tys tys in
    let tys_ptrs = List.combine alloc_locs ptrs in
    let* () =
      map_store @@ fun store ->
      List.fold_left
        (fun store ((local : GAst.local), ptr) ->
          Store.add local.index (Some ptr, local.var_ty) store)
        store tys_ptrs
    in
    (* store values for the arguments *)
    let tys_ptrs = List.tl tys_ptrs in
    let+ protected =
      fold_list tys_ptrs ~init:[]
        ~f:(fun protected ({ index; var_ty = ty; _ }, ptr) ->
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
    let* store = get_store () in
    fold_list (Store.bindings store) ~init:() ~f:(fun () (_, (ptr, _)) ->
        match (ptr, protected_address) with
        | None, _ -> ok ()
        | Some ptr, None -> State.free ptr
        | Some ((ptr, _) as fptr), Some protect ->
            if%sat Sptr.sem_eq ptr protect then ok () else State.free fptr)

  let resolve_constant (const : Expressions.constant_expr) =
    match const.value with
    | CLiteral (VScalar scalar) -> ok (Base (value_of_scalar scalar))
    | CLiteral (VBool b) -> ok (Base (Typed.int_of_bool (Typed.bool b)))
    | CLiteral (VChar c) -> ok (Base (Typed.int (Uchar.to_int c)))
    | CLiteral (VFloat { float_value; float_ty }) ->
        let fp = float_precision float_ty in
        ok (Base (Typed.float fp float_value))
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
                   (fun l c -> Base (Typed.int (Char.code c)) :: l)
                   []
              |> List.rev
            in
            let char_arr = Array chars in
            let str_ty : Types.ty = mk_array_ty (TLiteral (TUInt U8)) len in
            let* ptr, _ = State.alloc_ty str_ty in
            let ptr = (ptr, Some (Typed.int len)) in
            let* () = State.store ptr str_ty char_arr in
            let+ () = State.store_str_global str ptr in
            Ptr ptr)
    | CFnPtr fn_ptr -> ok (ConstFn fn_ptr)
    | CLiteral (VByteStr _) -> not_impl "TODO: resolve const ByteStr"
    (* FIXME: this is hacky, but until we get proper monomorphisation this isn't too bad *)
    | CTraitConst (tref, "IS_ZST") ->
        let ty = List.hd tref.trait_decl_ref.binder_value.generics.types in
        let^+ size = Layout.size_of_s ty in
        Base (Typed.int_of_bool (size ==@ 0s))
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
        let value, _ =
          List.fold_left
            (fun (acc, i) x ->
              let x = Z.shift_left (Z.of_int x) (8 * i) in
              (Z.add acc x, i + 1))
            (Z.zero, 0) bytes
        in
        ok (Base (Typed.int_z value))
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
    (* Dereference a pointer *)
    | PlaceProjection (base, Deref) -> (
        let* ptr = resolve_place base in
        L.debug (fun f ->
            f "Dereferencing ptr %a of %a" pp_full_ptr ptr pp_ty base.ty);
        let* v = State.load ptr base.ty in
        match v with
        | Ptr v -> (
            L.debug (fun f ->
                f "Dereferenced pointer %a to pointer %a" pp_full_ptr ptr
                  pp_full_ptr v);
            let pointee = Charon_util.get_pointee base.ty in
            match base.ty with
            | TRef _ | TAdt { id = TBuiltin TBox; _ } ->
                let+ () = State.check_ptr_align (fst v) pointee in
                v
            | _ -> ok v)
        | Base off ->
            let+ off = lift_symex @@ cast_checked ~ty:Typed.t_int off in
            let ptr = Sptr.null_ptr_of off in
            (ptr, None)
        | _ -> not_impl "Unexpected value when dereferencing place")
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
              Typed.int @@ Charon_util.int_of_const_generic len
          | Some len, TAdt { id = TBuiltin TSlice; _ } -> Typed.cast len
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let* idx = eval_operand idx in
        let idx = as_base_of ~ty:Typed.t_int idx in
        let idx = if from_end then len -@ idx else idx in
        if%sat 0s <=@ idx &&@ (idx <@ len) then (
          let^^+ ptr' = Sptr.offset ~ty ptr idx in
          L.debug (fun f ->
              f "Projected %a, index %a, to pointer %a" Sptr.pp ptr Typed.ppa
                idx Sptr.pp ptr');
          (ptr', None))
        else error `OutOfBounds
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
              (ty, Typed.int @@ Charon_util.int_of_const_generic len)
          | ( Some len,
              TAdt { id = TBuiltin TSlice; generics = { types = [ ty ]; _ } } )
            ->
              (ty, Typed.cast len)
          | _ -> Fmt.failwith "Index projection: unexpected arguments"
        in
        let* from = eval_operand from in
        let* to_ = eval_operand to_ in
        let from = as_base_of ~ty:Typed.t_int from in
        let to_ = as_base_of ~ty:Typed.t_int to_ in
        let to_ = if from_end then len -@ to_ else to_ in
        if%sat 0s <=@ from &&@ (from <=@ to_) &&@ (to_ <=@ len) then (
          let^^+ ptr' = Sptr.offset ~ty ptr from in
          let slice_len = to_ -@ from in
          L.debug (fun f ->
              f "Projected %a, slice %a..%a%s, to pointer %a, len %a" Sptr.pp
                ptr Typed.ppa from Typed.ppa to_
                (if from_end then "(from end)" else "")
                Sptr.pp ptr' Typed.ppa slice_len);
          (ptr', Some slice_len))
        else error `OutOfBounds

  (** Resolve a function operand, returning a callable symbolic function to
      execute it.

      This function also handles validating the call; given the input types it
      will be called with and the output type expected, it will make sure these
      are the right types and in the right amount. *)
  and resolve_function ~in_tys ~out_ty : GAst.fn_operand -> 'err fun_exec t =
    function
    (* For static calls we don't need to check types, that's what the type checker does. *)
    | FnOpRegular { func = FunId (FRegular fid); _ }
    | FnOpRegular { func = TraitMethod (_, _, fid); _ } -> (
        try
          let fundef = Crate.get_fun fid in
          L.info (fun g ->
              g "Resolved function call to %a" Crate.pp_name
                fundef.item_meta.name);
          match Std_funs.std_fun_eval fundef exec_fun with
          | Some fn -> ok fn
          | None -> ok (exec_fun fundef)
        with Crate.MissingDecl _ -> not_impl "Missing function declaration")
    | FnOpRegular { func = FunId (FBuiltin fn); generics } ->
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
          match fn.func with
          | FunId (FRegular fid) | TraitMethod (_, _, fid) ->
              let fn = Crate.get_fun fid in
              let rec check_tys l r =
                match (l, r) with
                | [], [] -> ok ()
                | ty1 :: l, ty2 :: r ->
                    if Layout.is_abi_compatible ty1 ty2 then check_tys l r
                    else error `InvalidFnArgTys
                | _ -> error `InvalidFnArgCount
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
        let fundef = Crate.get_fun decl.body in
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
        let* v = lift_state_op @@ global_fn ~args:[] in
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
    | RvRef (place, borrow) ->
        let* ((rptr, _) as ptr) = resolve_place place in
        let* () = State.check_ptr_align rptr place.ty in
        let+ ptr' = State.borrow ptr place.ty borrow in
        Ptr ptr'
    | Global { id; _ } ->
        let* ptr = resolve_global id in
        let decl = Crate.get_global id in
        State.load ptr decl.ty
    | GlobalRef ({ id; _ }, _) ->
        (* References to globals don't reborrow; otherwise this test fails:
          https://github.com/rust-lang/miri/blob/9d77dd818c01240647004361c1201c66ec061c08/tests/pass/static_mut.rs *)
        let+ ptr = resolve_global id in
        Ptr ptr
    | UnaryOp (op, e) -> (
        let* v = eval_operand e in
        match op with
        | Not -> (
            let v = as_base_of ~ty:Typed.t_int v in
            match type_of_operand e with
            | TLiteral TBool -> ok (Base (Typed.not_int_bool v))
            | TLiteral ((TInt _ | TUInt _) as i_ty) ->
                let size = Layout.size_of_literal_ty i_ty * 8 in
                let signed = Layout.is_signed i_ty in
                let v = Typed.bit_not ~size ~signed v in
                ok (Base v)
            | ty ->
                Fmt.kstr not_impl "Unexpect type in UnaryOp.Neg: %a" pp_ty ty)
        | Neg _ -> (
            match type_of_operand e with
            | TLiteral (TInt _ | TUInt _) ->
                let v = as_base_of ~ty:Typed.t_int v in
                ok (Base ~-v)
            | TLiteral (TFloat _) ->
                let+ v =
                  of_opt_not_impl ~msg:"Expected a float type"
                  @@ Typed.cast_float (as_base v)
                in
                Base (Typed.float_like v 0.0 -.@ v)
            | _ -> not_impl "Invalid type for Neg")
        | PtrMetadata -> (
            match v with
            | Ptr (_, None) | Base _ -> ok (Tuple [])
            | Ptr (_, Some v) -> ok (Base v)
            | _ -> not_impl "Invalid value for PtrMetadata")
        | Cast (CastRawPtr (_from, _to)) -> ok v
        | Cast (CastTransmute (from_ty, to_ty)) ->
            let* verify_ptr = State.is_valid_ptr in
            State.lift_err @@ Encoder.transmute ~verify_ptr ~from_ty ~to_ty v
        | Cast (CastScalar (from_ty, to_ty)) ->
            let* verify_ptr = State.is_valid_ptr in
            State.lift_err
            @@ Encoder.transmute ~verify_ptr ~from_ty:(TLiteral from_ty)
                 ~to_ty:(TLiteral to_ty) v
        | Cast (CastUnsize (_, _, MetaVTablePtr _)) ->
            not_impl "Unsupported: dyn"
        | Cast (CastUnsize (_, _, MetaUnknown)) ->
            not_impl "Unknown unsize kind"
        | Cast (CastUnsize (_, _, MetaLength length)) ->
            let rec with_ptr_meta meta : Sptr.t rust_val -> Sptr.t rust_val t =
              function
              | Ptr (v, _) -> ok (Ptr (v, Some meta))
              | ( Struct (_ :: _ as fs)
                | Array (_ :: _ as fs)
                | Tuple (_ :: _ as fs) ) as v -> (
                  match List.rev fs with
                  | last :: rest -> (
                      let+ last = with_ptr_meta meta last in
                      let fs = List.rev (last :: rest) in
                      match v with
                      | Struct _ -> Struct fs
                      | Array _ -> Array fs
                      | Tuple _ -> Tuple fs
                      | _ -> assert false)
                  | [] -> not_impl "Couldn't set pointer meta in CastUnsize")
              | _ -> not_impl "Couldn't set pointer meta in CastUnsize"
            in
            let size = Typed.int_z @@ z_of_const_generic length in
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
                let^ v1, v2, ty = cast_checked2 v1 v2 in
                match Typed.untype_type ty with
                | TInt ->
                    let op =
                      match op with
                      | Ge -> Typed.geq
                      | Gt -> Typed.gt
                      | Lt -> Typed.lt
                      | Le -> Typed.leq
                      | _ -> assert false
                    in
                    let v = op v1 v2 |> Typed.int_of_bool in
                    ok (Base v)
                | TFloat _ ->
                    let op =
                      match op with
                      | Ge -> Typed.geq_f
                      | Gt -> Typed.gt_f
                      | Lt -> Typed.lt_f
                      | Le -> Typed.leq_f
                      | _ -> assert false
                    in
                    let v1, v2 = (Typed.cast v1, Typed.cast v2) in
                    let v = op v1 v2 |> Typed.int_of_bool in
                    ok (Base v)
                | TPointer -> error `UBPointerComparison
                | _ -> assert false)
            | Eq | Ne ->
                let^ v1, v2, _ = cast_checked2 v1 v2 in
                let^^+ res = Core.equality_check v1 v2 in
                let res = if op = Eq then res else Typed.not_int_bool res in
                Base (res :> T.cval Typed.t)
            | Add om | Sub om | Mul om | Div om | Rem om | Shl om | Shr om -> (
                match (om, type_of_operand e1) with
                | OWrap, TLiteral ((TInt _ | TUInt _) as ty) ->
                    let^^ v = Core.safe_binop op ty v1 v2 in
                    let^+ res = Core.wrap_value ty v in
                    Base res
                | _, TLiteral ty ->
                    let^^+ res = Core.eval_lit_binop op ty v1 v2 in
                    Base res
                | _, _ -> not_impl "Unexpected type in binop")
            | AddChecked | SubChecked | MulChecked ->
                let ty =
                  match type_of_operand e1 with
                  | TLiteral ty -> ty
                  | ty -> Fmt.failwith "Unexpected type in binop: %a" pp_ty ty
                in
                State.lift_err @@ Core.eval_checked_lit_binop op ty v1 v2
            | Cmp ->
                let^ v1, v2, ty = cast_checked2 v1 v2 in
                if Typed.equal_ty ty Typed.t_ptr then error `UBPointerComparison
                else
                  let v = Typed.minus v1 v2 in
                  let^+ cmp = Core.cmp_of_int v in
                  Base cmp
            | Offset ->
                (* non-zero offset on integer pointer is not permitted, as these are always
                   dangling *)
                let^ v2 = cast_checked ~ty:Typed.t_int v2 in
                if%sat v2 ==@ 0s then ok (Base v1) else error `UBDanglingPointer
            | BitOr | BitAnd | BitXor ->
                let^ ity =
                  match type_of_operand e1 with
                  | TLiteral ((TInt _ | TUInt _) as ity) -> return ity
                  | TLiteral TBool -> return (Values.TUInt U8)
                  | TLiteral TChar -> return (Values.TUInt U32)
                  | ty ->
                      Fmt.kstr Rustsymex.not_impl
                        "Unsupported type for bitwise operation: %a" pp_ty ty
                in
                let size = 8 * Layout.size_of_literal_ty ity in
                let signed = Layout.is_signed ity in
                let^ v1 = cast_checked ~ty:Typed.t_int v1 in
                let^+ v2 = cast_checked ~ty:Typed.t_int v2 in
                let op =
                  match op with
                  | BitOr -> Typed.bit_or
                  | BitAnd -> Typed.bit_and
                  | BitXor -> Typed.bit_xor
                  | _ -> assert false
                in
                Base (op ~size ~signed v1 v2))
        | ((Ptr _ | Base _) as p1), ((Ptr _ | Base _) as p2) -> (
            match op with
            | Offset ->
                let^ p, meta, v =
                  match (p1, p2) with
                  | Ptr (p, meta), Base v | Base v, Ptr (p, meta) ->
                      return (p, meta, v)
                  | _ -> Rustsymex.not_impl "Invalid operands in offset"
                in
                let ty = Charon_util.get_pointee (type_of_operand e1) in
                let^ v = cast_checked ~ty:Typed.t_int v in
                let^^+ p' = Sptr.offset ~ty p v in
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
            ok (Base (Typed.int_of_bool Typed.v_false))
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
        let* loc, _ = resolve_place place in
        let enum, _ = TypesUtils.ty_as_custom_adt place.ty in
        let variants = Crate.as_enum enum in
        match variants with
        (* enums with one fieldless variant are ZSTs, so we can't load their discriminant! *)
        | [ { fields = []; discriminant; _ } ] ->
            ok (Base (value_of_scalar discriminant))
        | var :: _ ->
            let layout = Layout.of_variant enum var in
            let discr_ofs = Typed.int @@ Array.get layout.members_ofs 0 in
            let discr_ty = Layout.enum_discr_ty enum in
            let^^ loc = Sptr.offset loc discr_ofs in
            State.load (loc, None) discr_ty
        | [] -> Fmt.kstr not_impl "Unsupported discriminant for empty enums")
    (* Enum aggregate *)
    | Aggregate (AggregatedAdt ({ id = TAdtId t_id; _ }, Some v_id, None), vals)
      ->
        let variants = Crate.as_enum t_id in
        let variant = Types.VariantId.nth variants v_id in
        let discr = value_of_scalar variant.discriminant in
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
              let^+ v = cast_checked ~ty:Typed.t_int v in
              Sptr.null_ptr_of v
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
        let els = List.init len (fun _ -> value) in
        Array els
    (* Shallow init box -- just casts a ptr into a box *)
    | ShallowInitBox (ptr, _) -> eval_operand ptr
    (* Raw pointer *)
    | RawPtr (place, _kind) ->
        let+ ptr = resolve_place place in
        Ptr ptr
    (* Length of a &[T;N] or &[T] *)
    | Len (place, _, size_opt) ->
        let* _, meta = resolve_place place in
        let^+ len =
          match (meta, size_opt) with
          | _, Some size -> return (Typed.int @@ int_of_const_generic size)
          | Some len, None -> return len
          | _ -> Rustsymex.not_impl "Unexpected len rvalue"
        in
        Base len

  and exec_stmt stmt : unit t =
    L.info (fun m -> m "Statement: %a" Crate.pp_statement stmt);
    L.trace (fun m ->
        m "Statement full:@.%a" UllbcAst.pp_raw_statement stmt.content);
    let { span = loc; content = stmt; _ } : UllbcAst.statement = stmt in
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
        map_store (Store.add local (Some ptr, ty))
    | StorageDead local -> (
        let* ptr, ty = get_variable_and_ty local in
        match ptr with
        | Some ptr ->
            let* () = State.free ptr in
            map_store (Store.add local (None, ty))
        | None -> ok ())
    | Drop (place, _trait) ->
        (* TODO: We now have the trait to get the Drop implementation, however we currently
           don't monomorphise trait implementations, so this is useless. *)
        let* place_ptr = resolve_place place in
        let* () =
          match place.ty with
          | TAdt { id = TAdtId id; _ } -> (
              let adt = Crate.get_adt id in
              match (adt.item_meta.lang_item, List.last adt.item_meta.name) with
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
    | Assert { cond; expected; on_failure } -> (
        let* cond = eval_operand cond in
        let^ cond_int =
          match cond with
          | Base cond -> cast_checked cond ~ty:Typed.t_int
          | _ -> Rustsymex.not_impl "Expected a base Rust value in assert"
        in
        let cond_bool = Typed.bool_of_int cond_int in
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
        let+ _ =
          lift_state_op
          @@ Std_funs.Intrinsics.copy_nonoverlapping ~t:ty ~src ~dst ~count
        in
        ()
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
    let { span = loc; content = term; _ } : UllbcAst.terminator = terminator in
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
          @@ lift_state_op
          @@ exec_fun ~args
        in
        State.unwind_with fun_exec
          ~f:(fun v ->
            let* ptr = resolve_place place in
            L.info (fun m ->
                m "Returned %a from %a" pp_rust_val v Crate.pp_fn_operand func);
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
        let* ptr =
          of_opt_not_impl ~msg:"Return value unset, but returned" ptr
        in
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
                  if%sat [@lname "else case"] [@rname "if case"] discr ==@ 0s
                  then ok else_block
                  else ok if_block
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
                    if Z.equal Z.zero (z_of_scalar v) then
                      Sptr.is_at_null_loc ptr
                    else failwith "Can't compare pointer with non-0 scalar"
              | _ ->
                  fun (v, _) ->
                    Fmt.failwith
                      "Didn't know how to compare discriminant %a with scalar \
                       %s"
                      pp_rust_val discr
                      (PrintValues.scalar_value_to_string v)
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

  and exec_fun (fundef : UllbcAst.fun_decl) ~args state :
      (Sptr.t rust_val * State.t, 'e, 'f) Result.t =
    let open Rustsymex.Syntax in
    (* Put arguments in store *)
    let GAst.{ item_meta = { span = loc; name; _ }; body; _ } = fundef in
    let* body =
      match body with
      | None ->
          Fmt.kstr Rustsymex.not_impl "Function %a is opaque" Crate.pp_name name
      | Some body -> return body
    in
    let open InterpM.Syntax in
    let store = Store.empty in
    let@ () = run ~store ~state in
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
  let exec_fun ~args ~state fundef =
    let open Rustsymex.Syntax in
    let+- err, _ =
      let** value, state = exec_fun ~args fundef state in
      if !Config.current.ignore_leaks then Result.ok (value, state)
      else
        let@ () = Rustsymex.with_loc ~loc:fundef.item_meta.span in
        let++ (), state = State.leak_check state in
        (value, state)
    in
    State.add_to_call_trace err
      (Soteria_terminal.Call_trace.mk_element ~loc:fundef.item_meta.span
         ~msg:"Entry point" ())
end
