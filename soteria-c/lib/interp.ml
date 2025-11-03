open Soteria.Symex.Compo_res
open Csymex
open Csymex.Syntax
open Typed.Infix
open Typed.Syntax
open Ail_tys
module Ctype = Cerb_frontend.Ctype
module AilSyntax = Cerb_frontend.AilSyntax
module T = Typed.T
module BV = Typed.BitVec
module Agv = Aggregate_val

module InterpM (State : State_intf.S) = struct
  type 'a t =
    Store.t ->
    State.t ->
    ('a * Store.t * State.t, Error.t State.err, State.serialized) Result.t

  let ok x : 'a t = fun store state -> Result.ok (x, store, state)
  let error err : 'a t = fun _store state -> State.error err state
  let not_impl str : 'a t = fun _store _state -> Csymex.not_impl str
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

  let lift_state_op f =
   fun store state ->
    let++ v, state = f state in
    (v, store, state)

  let lift_symex (s : 'a Csymex.t) : 'a t =
   fun store state ->
    let+ s = s in
    Ok (s, store, state)

  let lift_symex_res (s : ('a, Error.t State.err, State.serialized) Result.t) :
      'a t =
   fun store state ->
    let++ s = s in
    (s, store, state)

  let of_opt_not_impl ~msg x = lift_symex (of_opt_not_impl ~msg x)

  let assert_or_error cond (err : 'e) : unit t =
   fun store state ->
    let* res = Csymex.assert_or_error cond err in
    match res with
    | Ok () -> return (Ok ((), store, state))
    | Error e -> State.error e state
    | Missing f -> return (Missing f)

  let with_loc ~loc f =
    let old_loc = !Csymex.current_loc in
    Csymex.current_loc := loc;
    map (f ()) @@ fun res ->
    current_loc := old_loc;
    res

  let with_extra_call_trace ~loc ~msg (x : 'a t) : 'a t =
   fun store state ->
    let+ res = x store state in
    match res with
    | Ok triple -> Ok triple
    | Error e ->
        let elem = Soteria.Terminal.Call_trace.mk_element ~loc ~msg () in
        Error (State.add_to_call_trace e elem)
    | Missing f -> Missing f

  let if_not_ok do_this (x : 'a t) : 'a t =
   fun store state ->
    let+ res = x store state in
    match res with Ok _ -> res | _ -> do_this ()

  let run ~store ~state f = f store state

  module IState = struct
    let load ptr ty = lift_state_op (State.load ptr ty)
    let store ptr ty v = lift_state_op (State.store ptr ty v)
    let load_aggregate ptr ty = lift_state_op (State.load_aggregate ptr ty)

    let store_aggregate ptr ty v =
      lift_state_op (State.store_aggregate ptr ty v)

    let alloc ?zeroed size = lift_state_op (State.alloc ?zeroed size)
    let alloc_ty ty = lift_state_op (State.alloc_ty ty)

    let get_global id =
      lift_state_op (fun state ->
          let+ v, state = State.get_global id state in
          Ok (Agv.Basic v, state))
  end

  module IStore = struct
    let find_opt sym =
     fun store state ->
      let binding = Store.find_opt sym store in
      Result.ok (binding, store, state)
  end

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( let^ ) x f = bind (lift_symex x) f

    module Symex_syntax = struct
      let branch_on ?left_branch_name ?right_branch_name guard ~then_ ~else_ =
       fun store state ->
        Csymex.branch_on ?left_branch_name ?right_branch_name guard
          ~then_:(fun () -> then_ () store state)
          ~else_:(fun () -> else_ () store state)
    end
  end
end

(* It's a good occasion to play with effects: we have a global immutable state,
   and want to avoid passing it to every single function in the interpreter.
   TODO: If this works well, do the same thing for prog.
   *)
type _ Effect.t += Get_fun_ctx : Fun_ctx.t Effect.t

let get_fun_ctx () = Effect.perform Get_fun_ctx

module Make (State : State_intf.S) = struct
  module InterpM = InterpM (State)
  open InterpM.Syntax
  module Stubs = Stubs.M (State)

  exception Unsupported of (string * Cerb_location.t)

  let type_of expr = Cerb_frontend.Translation_aux.ctype_of expr
  let unwrap_ctype (Ctype.Ctype (_, ty)) = ty

  let pointer_inner (Ctype.Ctype (_, ty)) =
    match ty with Pointer (_, ty) -> Some ty | _ -> None

  type state = State.t

  let cast_aggregate_to_ptr (x : Agv.t) : [< T.sptr ] Typed.t Csymex.t =
    let open Csymex.Syntax in
    let* x = Agv.basic_or_unsupported ~msg:"cast_aggregate_to_ptr" x in
    match Typed.get_ty x with
    | TBitVector _ ->
        (* We can cast an integer to a pointer by assigning the "null" location *)
        Csymex.return (Typed.Ptr.mk Typed.Ptr.null_loc (Typed.cast x))
    | TPointer _ ->
        (* Already a pointer *)
        Csymex.return (Typed.cast x)
    | _ -> Csymex.not_impl "Cannot cast to pointer"

  let cast_to_int (x : [< T.cval ] Typed.t) : [> T.sint ] Typed.t Csymex.t =
    let open Csymex.Syntax in
    match Typed.get_ty x with
    | TBitVector _ -> Csymex.return (Typed.cast x)
    | TPointer _ ->
        let x = Typed.cast x in
        if%sat Typed.Ptr.is_at_null_loc x then Csymex.return (Typed.Ptr.ofs x)
        else
          Fmt.kstr Csymex.not_impl
            "Pointer to int that is not at null loc %a at %a" Typed.ppa x
            Fmt_ail.pp_loc (get_loc ())
    | _ -> Csymex.not_impl "Cannot cast to int"

  let cast_aggregate_to_int (x : Agv.t) : [> T.sint ] Typed.t Csymex.t =
    let open Csymex.Syntax in
    let* x = Agv.basic_or_unsupported ~msg:"cast_aggregate_to_int" x in
    cast_to_int x

  let cast_to_bool (x : [< T.cval ] Typed.t) : [> T.sbool ] Typed.t Csymex.t =
    let open Typed in
    match get_ty x with
    | TBitVector _ -> Csymex.return (BV.to_bool (cast x))
    | TPointer _ -> Csymex.return (not (Ptr.is_null (cast x)))
    | _ -> Csymex.not_impl "Cannot cast to bool"

  let cast_aggregate_to_bool (x : Agv.t) : [> T.sbool ] Typed.t Csymex.t =
    let open Csymex.Syntax in
    let* x = Agv.basic_or_unsupported ~msg:"cast_aggregate_to_bool" x in
    cast_to_bool x

  type 'err fun_exec =
    args:Agv.t list -> state -> (Agv.t * state, 'err, State.serialized) Result.t

  let get_param_tys name =
    let ptys = Ail_helpers.get_param_tys name in
    Csymex.of_opt_not_impl ~msg:"Couldn't find function prototype" ptys

  let fold_bindings (bindings : AilSyntax.bindings) ~init ~f =
    ListLabels.fold_left bindings ~init
      ~f:(fun acc (pname, ((loc, duration, _is_register), align, _quals, ty)) ->
        (match duration with
        | AilSyntax.Static | Thread -> raise (Unsupported ("static/tread", loc))
        | _ -> ());
        if Option.is_some align then raise (Unsupported ("align", loc));
        f acc (pname, ty))

  let try_with_unsupported f store state =
    try (InterpM.map_store f) store state
    with Unsupported (msg, loc) ->
      let@ () = with_loc ~loc in
      Csymex.not_impl msg

  let attach_bindings (bindings : AilSyntax.bindings) =
    try_with_unsupported @@ fun store ->
    fold_bindings bindings ~init:store ~f:(fun store (pname, ty) ->
        Store.reserve pname ty store)

  let remove_bindings (bindings : AilSyntax.bindings) =
    try_with_unsupported @@ fun store ->
    fold_bindings bindings ~init:store ~f:(fun store (pname, _) ->
        Store.remove pname store)

  (* We're assuming all bindings declared, and they have already been removed from the store. *)
  let free_bindings (bindings : AilSyntax.bindings) : unit InterpM.t =
   fun store state ->
    let++ state =
      Result.fold_list bindings ~init:state
        ~f:(fun state (pname, ((loc, _, _), _, _, _)) ->
          let@ () = with_loc_immediate ~loc in
          match Store.find_opt pname store with
          | Some { kind = Stackptr ptr; _ } ->
              let++ (), state = State.free ptr state in
              state
          | _ -> Result.ok state)
    in
    ((), store, state)

  let pp_bindings : AilSyntax.bindings Fmt.t =
    Fmt.Dump.iter List.iter Fmt.nop (Fmt.pair Fmt_ail.pp_sym Fmt.nop)

  let remove_and_free_bindings (bindings : AilSyntax.bindings) =
    let* () = free_bindings bindings in
    remove_bindings bindings

  let mk_store params =
    ListLabels.fold_left params ~init:Store.empty
      ~f:(fun store (pname, ty, value) ->
        L.trace (fun m ->
            m "Putting variable to the store: %a" Fmt_ail.pp_sym pname);
        Store.add_value pname value ty store)

  let dealloc_store store state =
    Result.fold_list (Store.bindings store) ~init:state
      ~f:(fun state (_, { kind; _ }) ->
        match kind with
        | Stackptr ptr ->
            let++ (), st = State.free ptr state in
            st
        | _ -> Result.ok state)

  let get_stack_address (sym : Ail_tys.sym) : T.sptr Typed.t option InterpM.t =
    let* binding = InterpM.IStore.find_opt sym in
    match binding with
    | None -> InterpM.ok None
    | Some { kind = Stackptr ptr; _ } -> InterpM.ok (Some ptr)
    | Some { kind = other; ty } ->
        let* ptr = InterpM.IState.alloc_ty ty in
        let* () =
          match other with
          | Value v -> InterpM.IState.store_aggregate ptr ty v
          | _ -> InterpM.ok ()
        in
        let+ () = InterpM.map_store (Store.add_stackptr sym ptr ty) in
        Some ptr

  let try_immediate_load e (store : Store.t) state =
    let (AilSyntax.AnnotatedExpression (_, _, _, e)) = e in
    match e with
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym id in
        match Store.find_opt id store with
        | Some { kind = Value v; _ } ->
            L.trace (fun m -> m "Immediate load: %a" Agv.pp v);
            Result.ok (Some v, store, state)
        | Some { kind = Uninit; _ } ->
            State.error `UninitializedMemoryAccess state
        | _ -> Result.ok (None, store, state))
    | _ -> Result.ok (None, store, state)

  let try_immediate_store lvalue rval (store : Store.t) state =
    let (AilSyntax.AnnotatedExpression (_, _, _, lvalue)) = lvalue in
    match lvalue with
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym id in
        L.trace (fun m -> m "Trying immediate store at %a" Fmt_ail.pp_sym id);
        match Store.find_opt id store with
        | Some { kind = Value _ | Uninit; ty } ->
            let store = Store.add_value id rval ty store in
            Result.ok (`Success, store, state)
        | _ -> Result.ok (`NotImmediate, store, state))
    | _ -> Result.ok (`NotImmediate, store, state)

  let rec aggregate_of_constant_exn ~ty (c : constant) : Agv.t =
    let unsupported msg = raise (Unsupported (msg, get_loc ())) in
    match c with
    | ConstantInteger (IConstant (z, _basis, _suff)) ->
        let size =
          match ty with
          | Ctype.Ctype (_, Basic (Integer inty)) -> Layout.size_of_int_ty inty
          | _ -> unsupported "integer constant with non-integer type"
        in
        let size =
          match size with
          | Some size -> size
          | None -> unsupported "integer constant with unknown size"
        in
        Agv.int_z size z
    | ConstantNull -> Agv.null
    | ConstantCharacter (pref, char) -> (
        if Option.is_some pref then unsupported "char prefix";
        match (Constants.string_to_char char, Layout.size_of_int_ty Char) with
        | Some char, Some size -> Agv.int size char
        | None, Some _ -> unsupported ("char constant: " ^ char)
        | _, None -> unsupported "char constant with unknown size")
    | ConstantStruct (tag, fields) ->
        let members =
          match Layout.get_struct_fields tag with
          | Some (members, None) -> members
          | Some (_, Some _) -> unsupported "flexible array member"
          | None -> unsupported "unknown struct tag"
        in
        let fields =
          List.map2
            (fun (_, (_, _, _, ty)) (_, v) -> aggregate_of_constant_exn ~ty v)
            members fields
        in
        Struct fields
    | ConstantFloating (str, _suff) ->
        let precision : Svalue.FloatPrecision.t =
          match ty with
          | Ctype.Ctype (_, Basic (Floating fty)) -> Layout.precision fty
          | _ ->
              Fmt.failwith "float is not of float type: %a of type %a at %a"
                Fmt_ail.pp_constant c Fmt_ail.pp_ty ty Fmt_ail.pp_loc
                (get_loc ())
        in
        let f = Typed.Float.mk precision str in
        Agv.Basic f
    | ConstantInteger _ | ConstantIndeterminate _ | ConstantPredefined _
    | ConstantArray (_, _)
    | ConstantUnion (_, _, _) ->
        let msg = Fmt.str "value of constant? %a" Fmt_ail.pp_constant c in
        unsupported msg

  let aggregate_of_constant ~ty (c : constant) : Agv.t Csymex.t =
    try Csymex.return (aggregate_of_constant_exn ~ty c)
    with Unsupported (msg, loc) ->
      let@ () = with_loc ~loc in
      Csymex.not_impl msg

  let unwrap_expr (AnnotatedExpression (_, _, _, e) : expr) = e

  let cast ~old_ty ~new_ty (v : Agv.t) : Agv.t Csymex.t =
    let open Csymex.Syntax in
    let open Typed in
    if Ctype.ctypeEqual old_ty new_ty then return v
    else
      let (Ctype.Ctype (_, old_ty)) = old_ty in
      let (Ctype.Ctype (_, new_ty)) = new_ty in
      let* v =
        match v with
        | Basic v -> return v
        | Struct _ -> Fmt.kstr not_impl "Cannot cast %a" Agv.pp v
      in
      let+ res =
        match (old_ty, new_ty) with
        | Ctype.Basic (Integer _), Ctype.Pointer (_quals, _ty) -> (
            match get_ty v with
            | TBitVector _ -> return (Ptr.mk Ptr.null_loc (Typed.cast v))
            | TPointer _ -> return v
            | _ -> Fmt.failwith "BUG: not a valid C value: %a" Typed.ppa v)
        | Ctype.Pointer (_, _), Ctype.Pointer (_, _) -> return v
        | Ctype.Basic (Integer ity_old), Ctype.Basic (Integer ity_new) ->
            let* v = cast_to_int v in
            let* size_old = Layout.size_of_int_ty_unsupported ity_old in
            let+ size_new = Layout.size_of_int_ty_unsupported ity_new in
            if size_old > size_new then BV.extract 0 (size_new - 1) v
            else if size_old < size_new then
              BV.extend ~signed:false (size_new - size_old) v
            else (v :> T.cval Typed.t)
        | _, Ctype.Void -> return U8.(0s)
        | _ ->
            Fmt.kstr Csymex.not_impl "Cast %a -> %a" Fmt_ail.pp_ty_ old_ty
              Fmt_ail.pp_ty_ new_ty
      in
      Agv.Basic res

  open InterpM

  let rec equality_check (v1 : [< T.cval ] Typed.t) (v2 : [< T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TBitVector _, TBitVector _ | TPointer _, TPointer _ ->
        ok (v1 ==@ v2 |> BV.of_bool)
    | TFloat fp1, TFloat fp2 when Svalue.FloatPrecision.equal fp1 fp2 ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        ok (v1 ==.@ v2 |> BV.of_bool)
    | TPointer _, TBitVector _ ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2, size = Option.get @@ Typed.cast_int v2 in
        let+ () = assert_or_error (v2 ==@ BV.zero size) `UBPointerComparison in
        v1 ==@ Typed.Ptr.null |> BV.of_bool
    | TBitVector _, TPointer _ -> equality_check v2 v1
    | _ ->
        Fmt.kstr not_impl "Unexpected types in cval equality: %a and %a"
          Typed.ppa v1 Typed.ppa v2

  let aggregate_equality_check (v1 : Agv.t) (v2 : Agv.t) =
    let^ v1 = Agv.basic_or_unsupported ~msg:"aggregate_equality_check" v1 in
    let^ v2 = Agv.basic_or_unsupported ~msg:"aggregate_equality_check" v2 in
    equality_check v1 v2

  let rec arith_add ~signed (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TBitVector _, TBitVector _ ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        let res, ovf = if signed then v1 +$?@ v2 else v1 +?@ v2 in
        let+ () = assert_or_error (Typed.not ovf) `Overflow in
        res
    | TPointer _, TBitVector _ ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs, ovf = Typed.Ptr.ofs v1 +$?@ v2 in
        let+ () = assert_or_error (Typed.not ovf) `Overflow in
        Typed.Ptr.mk loc ofs
    | TBitVector _, TPointer _ -> arith_add ~signed v2 v1
    | TPointer _, TPointer _ -> error `UBPointerArithmetic
    | _ ->
        Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
          Typed.ppa v2

  let arith_sub ~signed (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TBitVector _, TBitVector _ ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        let res, ovf = if signed then v1 -$?@ v2 else v1 -?@ v2 in
        let+ () = assert_or_error (Typed.not ovf) `Overflow in
        res
    | TPointer _, TBitVector _ ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sint Typed.t = Typed.cast v2 in
        let loc = Typed.Ptr.loc v1 in
        let ofs, res = Typed.Ptr.ofs v1 -$?@ v2 in
        let+ () = assert_or_error (Typed.not res) `Overflow in
        Typed.Ptr.mk loc ofs
    | TPointer _, TPointer _ ->
        let v1 : T.sptr Typed.t = Typed.cast v1 in
        let v2 : T.sptr Typed.t = Typed.cast v2 in
        if%sat Typed.Ptr.loc v1 ==@ Typed.Ptr.loc v2 then
          let res, ovf = Typed.Ptr.ofs v1 -$?@ Typed.Ptr.ofs v2 in
          let+ () = assert_or_error (Typed.not ovf) `Overflow in
          res
        else error `UBPointerArithmetic
    | _ ->
        Fmt.kstr not_impl "Unexpected types in addition: %a and %a" Typed.ppa v1
          Typed.ppa v2

  let arith_mul ~signed (v1 : [< Typed.T.cval ] Typed.t)
      (v2 : [< Typed.T.cval ] Typed.t) =
    match (Typed.get_ty v1, Typed.get_ty v2) with
    | TBitVector _, TBitVector _ ->
        let v1 = Typed.cast v1 in
        let v2 = Typed.cast v2 in
        let res, ovf = if signed then v1 *$?@ v2 else v1 *?@ v2 in
        let+ () = assert_or_error (Typed.not ovf) `Overflow in
        res
    | TPointer _, _ | _, TPointer _ -> error `UBPointerArithmetic
    | ty1, ty2 ->
        Fmt.kstr not_impl "Unexpected types in multiplication: %a and %a"
          Svalue.pp_ty ty1 Svalue.pp_ty ty2

  let arith (v1, t1) a_op (v2, t2) : [> T.sint ] Typed.t InterpM.t =
    match (a_op : AilSyntax.arithmeticOperator) with
    | Div -> (
        let t1 =
          match t1 with
          | Ctype.Ctype (_, Basic (Integer inty)) -> inty
          | _ -> failwith "Unreachable: Div with non-integer type"
        in
        let signed = Layout.is_int_ty_signed t1 in
        let^ v1 = cast_to_int v1 in
        let^ v2 = Csymex.bind (cast_to_int v2) Csymex.check_nonzero in
        match v2 with
        | Ok v2 -> ok (Typed.cast @@ BV.div ~signed v1 v2)
        | Error `NonZeroIsZero -> error `DivisionByZero
        | Missing _ -> failwith "Unreachable: check_nonzero returned miss")
    | Mul -> (
        match t1 with
        | Ctype.Ctype (_, Basic (Integer inty)) ->
            let signed = Layout.is_int_ty_signed inty in
            arith_mul ~signed v1 v2
        | _ -> not_impl "Mul with non-integer type")
    | Add -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | Some _, Some _ -> error `UBPointerArithmetic
        | Some ty, None ->
            let^ factor = Layout.size_of_s ty in
            let* v2 = arith_mul ~signed:true v2 factor in
            arith_add ~signed:true v1 v2
        | None, Some ty ->
            let^ factor = Layout.size_of_s ty in
            let* v1 = arith_mul ~signed:true v1 factor in
            arith_add ~signed:true v2 v1
        | None, None -> (
            match t1 with
            | Ctype.Ctype (_, Basic (Integer inty)) ->
                let signed = Layout.is_int_ty_signed inty in
                arith_add ~signed v1 v2
            | _ -> not_impl "Add with non-integer type"))
    | Sub -> (
        match (t1 |> pointer_inner, t2 |> pointer_inner) with
        | Some ty, None ->
            let^ factor = Layout.size_of_s ty in
            let* v2 = arith_mul ~signed:true v2 factor in
            arith_sub ~signed:true v1 v2
        | None, Some _ -> error `UBPointerArithmetic
        | Some _, Some _ -> arith_sub ~signed:true v1 v2
        | None, None -> (
            match t1 with
            | Ctype.Ctype (_, Basic (Integer inty)) ->
                let signed = Layout.is_int_ty_signed inty in
                arith_sub ~signed v1 v2
            | _ -> not_impl "Sub with non-integer type"))
    | Mod -> (
        let t1 =
          match t1 with
          | Ctype.Ctype (_, Basic (Integer inty)) -> inty
          | _ -> failwith "Unreachable: Div with non-integer type"
        in
        let signed = Layout.is_int_ty_signed t1 in
        let^ v1 = cast_to_int v1 in
        let^ v2 = Csymex.bind (cast_to_int v2) Csymex.check_nonzero in
        match v2 with
        | Ok v2 -> ok (Typed.cast @@ BV.rem ~signed v1 v2)
        | Error `NonZeroIsZero -> error `DivisionByZero
        | Missing _ -> failwith "Unreachable: check_nonzero returned miss")
    | (Band | Shl | Shr | Bxor | Bor) as a_op ->
        let* { signed; _ } =
          Layout.bv_info t1 |> of_opt_not_impl ~msg:"bv_info"
        in
        let^ v1 = cast_to_int v1 in
        let^ v2 = cast_to_int v2 in
        let op =
          match a_op with
          | Band -> Typed.BitVec.and_
          | Bxor -> Typed.BitVec.xor
          | Bor -> Typed.BitVec.or_
          | Shl -> Typed.BitVec.shl
          | Shr -> if signed then Typed.BitVec.ashr else Typed.BitVec.lshr
          | _ -> failwith "unreachable: bit operator is not bit operator?"
        in
        ok (op v1 v2)

  let try_immediate_postfix_op ~apply_op lvalue =
    let* v_opt = try_immediate_load lvalue in
    match v_opt with
    | Some v -> (
        (* Optimisation *)
        (* If the value is in direct access, we can just increment it and
          immediately update it in the store. The writing cannot fail. *)
        let* res = apply_op v in
        let* store = try_immediate_store lvalue res in
        match store with
        | `NotImmediate ->
            failwith "Immediate store failed after immediate load succeeded"
        | `Success -> InterpM.ok (Some res))
    | None -> InterpM.ok None

  (* We do this in the untyped world *)
  let ineq_comparison ~int_cmp_op ~float_cmp_op left right =
    let+ res =
      let^ left = Agv.basic_or_unsupported ~msg:"ineq_comparison" left in
      let^ right = Agv.basic_or_unsupported ~msg:"ineq_comparison" right in
      let int_cmp_op left right = int_cmp_op left right |> BV.of_bool in
      match (Typed.get_ty left, Typed.get_ty right) with
      | TBitVector _, TBitVector _ ->
          let left = Typed.cast left in
          let right = Typed.cast right in
          ok (int_cmp_op left right)
      | TFloat fp1, TFloat fp2 when Svalue.FloatPrecision.equal fp1 fp2 ->
          let left = Typed.cast left in
          let right = Typed.cast right in
          ok (float_cmp_op left right |> BV.of_bool)
      | TPointer _, TPointer _ ->
          let left = Typed.cast left in
          let right = Typed.cast right in
          if%sat Typed.Ptr.loc left ==@ Typed.Ptr.loc right then
            ok (int_cmp_op (Typed.Ptr.ofs left) (Typed.Ptr.ofs right))
          else error `UBPointerComparison
      | _, TPointer _ | TPointer _, _ -> error `UBPointerComparison
      | _ ->
          Fmt.kstr not_impl "Unsupported comparison: %a and %a" Typed.ppa left
            Typed.ppa right
    in
    Agv.Basic res

  module Stmt_exec_result = struct
    type t =
      | Normal
      | Continue
      | Break
      | Goto of Symbol_std.t
      | Case of T.sint Typed.t
      | Returned of Agv.t
    [@@deriving show { with_path = false }]
  end

  let rec resolve_function fexpr :
      (Error.t State.err fun_exec * Stubs.Arg_filter.t) InterpM.t =
    let* loc, fname =
      let (AilSyntax.AnnotatedExpression (_, _, loc, inner_expr)) = fexpr in
      match inner_expr with
      (* Special case when we can immediately resolve the function name *)
      | AilEfunction_decay (AnnotatedExpression (_, _, _, AilEident fname)) ->
          ok (loc, fname)
      | _ ->
          (* Some function pointer *)
          L.trace (fun m ->
              m "Resolving function pointer: %a" Fmt_ail.pp_expr fexpr);
          let* fptr = eval_expr fexpr in
          L.trace (fun m -> m "Function pointer is value: %a" Agv.pp fptr);
          let^ fptr = cast_aggregate_to_ptr fptr in
          if%sat
            Typed.not (Typed.Ptr.ofs fptr ==@ Usize.(0s))
            ||@ Typed.Ptr.is_at_null_loc fptr
          then error `InvalidFunctionPtr
          else
            let fctx = get_fun_ctx () in
            let^ sym = Fun_ctx.get_sym (Typed.Ptr.loc fptr) fctx in
            ok (loc, sym)
    in
    let@ () = with_loc ~loc in
    let fundef_opt = Ail_helpers.find_fun_def fname in
    match fundef_opt with
    | Some fundef -> ok (exec_fun fundef, Stubs.Arg_filter.no_filter)
    | None -> (
        match Stubs.find_stub fname with
        | Some (stub, filter) -> ok (stub, filter)
        | None ->
            if (Config.current ()).havoc_undefined_funs then
              let return_ty = Ail_helpers.get_return_ty fname in
              ok (Stubs.havoc ~return_ty, None)
            else
              Fmt.kstr not_impl "Cannot call external function: %a"
                Fmt_ail.pp_sym fname)

  and eval_expr_list (el : expr list) =
    let+ vs =
      InterpM.fold_list el ~init:[] ~f:(fun acc e ->
          let+ new_res = eval_expr e in
          new_res :: acc)
    in
    List.rev vs

  and eval_expr (aexpr : expr) : Agv.t InterpM.t =
    let open InterpM.IState in
    let (AnnotatedExpression (_, _, loc, expr)) = aexpr in
    let@ () = with_loc ~loc in
    match expr with
    | AilEconst c ->
        let ty = type_of aexpr in
        let^ v = aggregate_of_constant ~ty c in
        ok v
    | AilEcall (f, args) ->
        let* exec_fun, filter = resolve_function f in
        let* args = eval_expr_list (Stubs.Arg_filter.apply filter args) in
        let+ v =
          with_extra_call_trace ~loc ~msg:"Called from here"
          @@ lift_state_op
          @@ exec_fun ~args
        in
        L.debug (fun m -> m "returned %a from %a" Agv.pp v Fmt_ail.pp_expr f);
        v
    | AilEunary (Address, e) -> (
        match unwrap_expr e with
        | AilEunary (Indirection, e) -> (* &*e <=> e *) eval_expr e
        | AilEident id -> (
            let id = Ail_helpers.resolve_sym id in
            let* ptr_opt = get_stack_address id in
            match ptr_opt with
            | Some ptr -> ok (Agv.Basic (ptr :> T.cval Typed.t))
            | None -> get_global id)
        | AilEmemberofptr (ptr, member) ->
            let* ptr_v = eval_expr ptr in
            let^ ptr_v =
              Agv.basic_or_unsupported ~msg:"AilEmemberofptr" ptr_v
            in
            let^ ty_pointee =
              type_of ptr
              |> Cerb_frontend.AilTypesAux.referenced_type
              |> Csymex.of_opt_not_impl
                   ~msg:"Member of Pointer that isn't of type pointer"
            in
            let^ mem_ofs = Layout.member_ofs member ty_pointee in
            let+ res = arith_add ~signed:true ptr_v mem_ofs in
            Agv.Basic res
        | _ -> Fmt.kstr not_impl "Unsupported address_of: %a" Fmt_ail.pp_expr e)
    | AilEunary (((PostfixIncr | PostfixDecr) as op), e) -> (
        let apply_op v =
          let^ v = Agv.basic_or_unsupported ~msg:"Postfix operator" v in
          let ty = type_of e in
          let* operand, signed =
            match (pointer_inner ty, ty) with
            | Some ty, _ ->
                let^ operand = Layout.size_of_s ty in
                ok (operand, true)
            | None, Ctype.Ctype (_, Basic (Integer inty)) ->
                let^ size = Layout.size_of_int_ty_unsupported inty in
                ok (BV.one size, Layout.is_int_ty_signed inty)
            | _ -> not_impl "Postfix operator on unsupported type"
          in
          let+ res =
            match op with
            | PostfixIncr -> arith_add ~signed v operand
            | PostfixDecr -> arith_sub ~signed v operand
            | _ -> failwith "unreachable: postfix is not postfix??"
          in
          Agv.Basic res
        in
        let* res_opt = try_immediate_postfix_op ~apply_op e in
        match res_opt with
        | Some v -> ok v
        | None ->
            let* v = eval_expr e in
            let^ ptr = cast_aggregate_to_ptr v in
            let* v = load_aggregate ptr (type_of e) in
            let* v_incr = apply_op v in
            let+ () = store_aggregate ptr (type_of e) v_incr in
            v)
    | AilEunary (op, e) -> (
        let* v = eval_expr e in
        match op with
        | Indirection -> ok v
        | Address -> failwith "unreachable: address_of already handled"
        | Minus ->
            let^ v = cast_aggregate_to_int v in
            let res = Typed.cast @@ BV.neg v in
            ok (Agv.Basic res)
        | AilSyntax.Bnot ->
            let^ v = cast_aggregate_to_int v in
            let res = Typed.BitVec.not v in
            ok (Agv.Basic res)
        | AilSyntax.Plus | AilSyntax.PostfixIncr | AilSyntax.PostfixDecr ->
            Fmt.kstr not_impl "Unsupported unary operator %a" Fmt_ail.pp_unop op
        )
    | AilEbinary (e1, Or, e2) ->
        (* Or is short-circuiting. In case of side-effects on the RHS,
           not doing this properly might lead to unsoundnesses.
           We still optimize by returning a disjunction of the two
           expressions if the RHS is side-effect free. *)
        let* v1 = eval_expr e1 in
        if Ail_helpers.sure_side_effect_free e2 then
          let* v2 = eval_expr e2 in
          let^ v1 = cast_aggregate_to_bool v1 in
          let^ v2 = cast_aggregate_to_bool v2 in
          let b_res = v1 ||@ v2 in
          ok (Agv.Basic (BV.of_bool b_res))
        else
          let^ v1 = cast_aggregate_to_bool v1 in
          if%sat v1 then ok (Agv.Basic U8.(1s))
          else
            let* v2 = eval_expr e2 in
            let^ b_res = cast_aggregate_to_bool v2 in
            ok (Agv.Basic (BV.of_bool b_res))
    | AilEbinary (e1, And, e2) ->
        (* Same as Or, we need to short-circuit *)
        let* v1 = eval_expr e1 in
        if Ail_helpers.sure_side_effect_free e2 then
          let* v2 = eval_expr e2 in
          let^ v2 = cast_aggregate_to_bool v2 in
          let^ v1 = cast_aggregate_to_bool v1 in
          let b_res = v1 &&@ v2 in
          ok (Agv.Basic (BV.of_bool b_res))
        else
          let^ v1 = cast_aggregate_to_bool v1 in
          if%sat v1 then
            let* v2 = eval_expr e2 in
            let^ b_res = cast_aggregate_to_bool v2 in
            ok (Agv.Basic (BV.of_bool b_res))
          else ok (Agv.Basic U8.(0s))
    | AilEbinary (e1, op, e2) -> (
        let* v1 = eval_expr e1 in
        let* v2 = eval_expr e2 in
        match op with
        | Ge -> ineq_comparison ~int_cmp_op:( >=@ ) ~float_cmp_op:( >=.@ ) v1 v2
        | Gt -> ineq_comparison ~int_cmp_op:( >@ ) ~float_cmp_op:( >.@ ) v1 v2
        | Lt -> ineq_comparison ~int_cmp_op:( <@ ) ~float_cmp_op:( <.@ ) v1 v2
        | Le -> ineq_comparison ~int_cmp_op:( <=@ ) ~float_cmp_op:( <=.@ ) v1 v2
        | Eq ->
            let+ res = aggregate_equality_check v1 v2 in
            Agv.Basic res
        | Ne ->
            (* TODO: Semantics of Ne might be different from semantics of not eq? *)
            let+ res = aggregate_equality_check v1 v2 in
            Agv.Basic (BV.not_bool res)
        | Or | And -> failwith "Unreachable, handled earlier."
        | Arithmetic a_op ->
            let^ v1 = Agv.basic_or_unsupported ~msg:"Arithmetics" v1 in
            let^ v2 = Agv.basic_or_unsupported ~msg:"Arithmetics" v2 in
            let+ res = arith (v1, type_of e1) a_op (v2, type_of e2) in
            Agv.Basic res
        | Comma -> ok v2)
    | AilErvalue e -> (
        (* Optimisation: If the expression to load is a variable that is
           immediately in the store (without heap indirection),
           we can just access it without heap shenanigans. *)
        let* v_opt = try_immediate_load e in
        match v_opt with
        | Some v -> ok v
        | None ->
            (* The value is not an immediate store load *)
            let* lvalue = eval_expr e in
            let ty = type_of e in
            (* At this point, lvalue must be a pointer (including to the stack) *)
            let^ lvalue = cast_aggregate_to_ptr lvalue in
            load_aggregate lvalue ty)
    | AilEident id -> (
        let id = Ail_helpers.resolve_sym id in
        let* ptr_opt = get_stack_address id in
        match ptr_opt with
        | Some v ->
            (* A pointer is a value *)
            let v = (v :> T.cval Typed.t) in
            ok (Agv.Basic v)
        | None ->
            (* If the variable isn't in the store, it must be a global variable. *)
            get_global id)
    | AilEassign (lvalue, rvalue) -> (
        (* Evaluate rvalue first *)
        let* rval = eval_expr rvalue in
        (* Optimisation: if the lvalue is a variable to which we assign directly,
           we don't need to do anything with the heap, we can simply immediately assign,
           obtaining a new store.  *)
        let* im_store_res = try_immediate_store lvalue rval in
        match im_store_res with
        | `Success -> ok rval
        | `NotImmediate ->
            let* ptr = eval_expr lvalue in
            let^ ptr = cast_aggregate_to_ptr ptr in
            let ty = type_of lvalue in
            let+ () = store_aggregate ptr ty rval in
            rval)
    | AilEcompoundAssign (lvalue, op, rvalue) -> (
        let* rval = eval_expr rvalue in
        let rty = type_of rvalue in
        let lty = type_of lvalue in
        let apply_op v =
          let^ v = Agv.basic_or_unsupported ~msg:"compound assign" v in
          let^ rval = Agv.basic_or_unsupported ~msg:"compound assign" rval in
          let+ res = arith (v, lty) op (rval, rty) in
          Agv.Basic res
        in
        let* immediate_result = try_immediate_postfix_op ~apply_op lvalue in
        match immediate_result with
        | Some v -> ok v
        | None ->
            (* Otherwise we proceed as normal *)
            let* ptr = eval_expr lvalue in
            (* At this point, lvalue must be a pointer (including to the stack) *)
            let^ ptr = cast_aggregate_to_ptr ptr in
            let* operand = load_aggregate ptr lty in
            let* res = apply_op operand in
            let+ () = store_aggregate ptr lty res in
            res)
    | AilEsizeof (_quals, ty) ->
        let^ res = Layout.size_of_s ty in
        ok (Agv.Basic res)
    | AilEalignof (_quals, ty) ->
        let^ res = Layout.align_of_s ty in
        ok (Agv.Basic res)
    | AilEmemberofptr (ptr, member) ->
        let* ptr_v = eval_expr ptr in
        let^ ptr_v = Agv.basic_or_unsupported ~msg:"memberofptr" ptr_v in
        let^ ty_pointee =
          type_of ptr
          |> Cerb_frontend.AilTypesAux.referenced_type
          |> Csymex.of_opt_not_impl
               ~msg:"Member of Pointer that isn't of type pointer"
        in
        let^ mem_ofs = Layout.member_ofs member ty_pointee in
        let+ res = arith_add ~signed:true ptr_v mem_ofs in
        Agv.Basic res
    | AilEmemberof (obj, member) ->
        let* ptr_v = eval_expr obj in
        let^ ptr_v = Agv.basic_or_unsupported ~msg:"memberof" ptr_v in
        let ty_obj = type_of obj in
        let^ mem_ofs = Layout.member_ofs member ty_obj in
        let+ res = arith_add ~signed:true ptr_v mem_ofs in
        Agv.Basic res
    | AilEcast (_quals, new_ty, expr) ->
        let old_ty = type_of expr in
        let* v = eval_expr expr in
        lift_symex @@ cast ~old_ty ~new_ty v
    | AilEfunction_decay (AnnotatedExpression (_, _, _, fexpr) as outer_fexpr)
      -> (
        match fexpr with
        | AilEident id ->
            let id = Ail_helpers.resolve_sym id in
            let ctx = get_fun_ctx () in
            let^ floc = Fun_ctx.decay_fn_sym id ctx in
            ok (Agv.Basic (Typed.Ptr.mk floc Usize.(0s)))
        | _ ->
            Fmt.kstr not_impl "Unsupported function decay: %a" Fmt_ail.pp_expr
              outer_fexpr)
    | AilEinvalid (_ty, reason) ->
        Fmt.kstr not_impl "Cerberus could not parse an expression because %a"
          Fmt_ail.pp_invalid_reason reason
    | AilEcond (guard, Some t, e) ->
        let* guard = eval_expr guard in
        let^ guard_bool = cast_aggregate_to_bool guard in
        if%sat guard_bool then eval_expr t else eval_expr e
    | AilEstruct (_tag, fields) ->
        let+ fields_rev =
          fold_list fields ~init:[] ~f:(fun acc (_, e_opt) ->
              match e_opt with
              | None -> not_impl "Partial field initialization"
              | Some e ->
                  let+ new_res = eval_expr e in
                  new_res :: acc)
        in
        Agv.Struct (List.rev fields_rev)
    | AilEcond (_, None, _) -> not_impl "GNU ?:"
    | AilEarray_decay _ -> not_impl "Array decay"
    | AilEassert _
    | AilEoffsetof (_, _)
    | AilEgeneric (_, _)
    | AilEarray (_, _, _)
    | AilEunion (_, _, _)
    | AilEcompound (_, _, _)
    | AilEbuiltin _ | AilEstr _ | AilEsizeof_expr _
    | AilEannot (_, _)
    | AilEva_start (_, _)
    | AilEva_arg (_, _)
    | AilEva_copy (_, _)
    | AilEva_end _ | AilEprint_type _ | AilEbmc_assume _ | AilEreg_load _
    | AilEatomic _
    | AilEgcc_statement (_, _) ->
        Fmt.kstr not_impl "Unsupported expr: %a" Fmt_ail.pp_expr aexpr

  and exec_body (body : stmt) : Agv.t InterpM.t =
    let open Stmt_exec_result in
    let rec aux res =
      L.trace (fun m -> m "Body execution result: %a" Stmt_exec_result.pp res);
      match res with
      | Returned v -> InterpM.ok v
      | Normal ->
          (* Function didn't return, we return void (encoded as 0) *)
          InterpM.ok Agv.void
      | Goto label ->
          L.trace (fun m ->
              m "Body terminated with Goto %a" Fmt_ail.pp_sym label);
          let* res = exec_goto label body in
          aux res
      | Break | Continue | Case _ ->
          failwith
            "Unreachable: terminated function body with Continue/Break/Case"
    in
    let* first_exec = exec_stmt body in
    (* We execute until we stop getting a goto. *)
    aux first_exec

  and exec_stmt_list (init : Stmt_exec_result.t) (stmtl : stmt list) :
      Stmt_exec_result.t InterpM.t =
    let open Stmt_exec_result in
    InterpM.fold_list stmtl ~init ~f:(fun res stmt ->
        match res with
        | Normal -> exec_stmt stmt
        | Goto label -> exec_goto label stmt
        | Case guard -> exec_case guard stmt
        | Break | Continue | Returned _ -> InterpM.ok res)

  (** Executing a goto statement, i.e. jumping to a label, returns the result of
      executing the label's target statement. *)

  and exec_goto (label : sym) (astmt : stmt) : Stmt_exec_result.t InterpM.t =
    let open Stmt_exec_result in
    L.trace (fun m ->
        m "Trying to find label %a, currently at %a" Fmt_ail.pp_sym label
          Fmt_ail.pp_stmt astmt);
    let AilSyntax.{ node = stmt; _ } = astmt in
    match stmt with
    | AilSlabel (label', stmt, _annot) when Symbol_std.equal label label' ->
        exec_stmt stmt
    | AilSblock (bindings, stmtl) ->
        let* () = attach_bindings bindings in
        let* res = exec_stmt_list (Goto label) stmtl in
        let+ () = remove_and_free_bindings bindings in
        res
    | AilSif (_, then_stmt, else_stmt) -> (
        let* then_res = exec_goto label then_stmt in
        match then_res with
        | Goto l -> exec_goto l else_stmt
        | Normal | Break | Continue | Returned _ -> ok then_res
        | Case _ -> failwith "SOTERIA BUG: Case in if branch")
    | AilSwhile (_, body, _) -> (
        let* res = exec_goto label body in
        match res with
        | Goto _ | Break | Returned _ -> ok res
        | Normal | Continue -> exec_stmt astmt
        | Case _ -> failwith "SOTERIA BUG: Case in while body")
    | AilSdo (body, e, _) -> (
        let* res = exec_goto label body in
        match res with
        | Goto _ | Break | Returned _ -> ok res
        | Normal | Continue ->
            let* guard = eval_expr e in
            let^ guard_bool = cast_aggregate_to_bool guard in
            if%sat guard_bool then exec_stmt astmt else ok Normal
        | Case _ -> failwith "SOTERIA BUG: Case in do body")
    | AilSswitch (_, body) -> exec_goto label body
    | AilSmarker (_, stmt) -> exec_goto label stmt
    | _ -> ok (Goto label)

  and exec_case (guard : T.sint Typed.t) (astmt : stmt) :
      Stmt_exec_result.t InterpM.t =
    let fail_if_different_case guard' =
      if guard != guard' then failwith "Returned a different case?"
    in
    let open Stmt_exec_result in
    L.trace (fun m ->
        m "Trying to find case corresponding to guard %a, currently at %a"
          Typed.ppa guard Fmt_ail.pp_stmt astmt);
    let AilSyntax.{ node = stmt; _ } = astmt in
    match stmt with
    | AilScase (case, stmt) ->
        let guard_size = Typed.size_of_int guard in
        if%sat guard ==@ BV.mk guard_size case then exec_stmt stmt
        else exec_case guard stmt
    | AilSdefault stmt -> exec_stmt stmt
    | AilSlabel (_, stmt, _) -> exec_case guard stmt
    | AilSblock (bindings, stmtl) ->
        let* () = attach_bindings bindings in
        let* res = exec_stmt_list (Case guard) stmtl in
        let+ () = remove_and_free_bindings bindings in
        res
    | AilSif (_, then_stmt, else_stmt) -> (
        let* then_res = exec_case guard then_stmt in
        match then_res with
        | Case guard' ->
            fail_if_different_case guard';
            exec_case guard else_stmt
        | Normal | Break | Continue | Returned _ | Goto _ -> ok then_res)
    | AilSwhile (_, body, _) -> (
        let* res = exec_case guard body in
        match res with
        | Goto _ | Break | Returned _ -> ok res
        | Case guard' ->
            fail_if_different_case guard';
            ok res
        | Normal | Continue -> exec_stmt astmt)
    | AilSdo (body, e, _) -> (
        let* res = exec_case guard body in
        match res with
        | Goto _ | Break | Returned _ -> ok res
        | Case guard' ->
            fail_if_different_case guard';
            ok res
        | Normal | Continue ->
            let* guard = eval_expr e in
            let^ guard_bool = cast_aggregate_to_bool guard in
            if%sat guard_bool then exec_stmt astmt else ok Normal)
    | AilSmarker (_, stmt) -> exec_case guard stmt
    | AilSswitch (_, _stmt) ->
        (* We make this case explicitly separate for clarity:
           If one has a nested switch, we don't look for the case
           in the nested switch, as the cases in there are for the nested switch's guard. *)
        ok (Case guard)
    | _ -> ok (Case guard)

  (** Executing a statement returns an optional value outcome (if a return
      statement was hit), or *)
  and exec_stmt (astmt : stmt) : Stmt_exec_result.t InterpM.t =
    let open Stmt_exec_result in
    let exec_stmt = exec_stmt in
    let^ () = Csymex.consume_fuel_steps 1 in
    L.debug (fun m -> m "Executing statement: %a" Fmt_ail.pp_stmt astmt);
    let* () =
      let+ store = get_store () in
      L.debug (fun m -> m "@[<v 2>STORE:@ %a@]" Store.pp store)
    in
    let AilSyntax.{ loc; node = stmt; _ } = astmt in
    let@ () = with_loc ~loc in
    match stmt with
    | AilSskip -> ok Normal
    | AilSreturn e ->
        let+ v = eval_expr e in
        L.debug (fun m -> m "Returning: %a" Agv.pp v);
        Returned v
    | AilSreturnVoid -> ok (Returned Agv.void)
    | AilSblock (bindings, stmtl) ->
        let* () = attach_bindings bindings in
        (* Second result, corresponding to the block-scoped store, is discarded *)
        let* res = exec_stmt_list Normal stmtl in
        (* Cerberus is nice here, symbols inside the block have different names than
           the ones outside the block if there is shadowing going on.
           I.e. int x = 12; { int x = 13; }, the two `x`s have different symbols.
           So we can just remove the bindings of the inner block from the store entirely. *)
        let+ () = remove_and_free_bindings bindings in
        res
    | AilSexpr e ->
        let+ _ = eval_expr e in
        Normal
    | AilSif (cond, then_stmt, else_stmt) ->
        let* v = eval_expr cond in
        let^ v = cast_aggregate_to_bool v in
        if%sat v then exec_stmt then_stmt [@name "if branch"]
        else exec_stmt else_stmt [@name "else branch"]
    | AilSwhile (cond, stmt, _loopid) ->
        let rec loop () =
          let* cond_v = eval_expr cond in
          let^ cond_v = cast_aggregate_to_bool cond_v in
          let neg_cond = Typed.not cond_v in
          if%sat neg_cond then ok Normal
          else
            let () = L.trace (fun m -> m "Condition is SAT!") in
            let* res = exec_stmt stmt in
            match res with
            | Returned _ | Goto _ -> ok res
            | Break -> ok Normal
            | Normal | Continue -> loop ()
            | Case _ -> failwith "SOTERIA BUG: Case in while body"
        in
        loop ()
    | AilSdo (stmt, cond, _loop_id) ->
        let rec loop () =
          let* res = exec_stmt stmt in
          match res with
          | Returned _ | Goto _ -> ok res
          | Break -> ok Normal
          | Normal | Continue ->
              let* cond_v = eval_expr cond in
              let^ cond_v = cast_aggregate_to_bool cond_v in
              if%sat Typed.not cond_v then ok Normal else loop ()
          | Case _ -> failwith "SOTERIA BUG: Case in do body"
        in
        loop ()
    | AilSlabel (_, stmt, _) | AilScase (_, stmt) -> exec_stmt stmt
    | AilSdeclaration decls ->
        let+ () =
          fold_list decls ~init:() ~f:(fun () (pname, expr) ->
              match expr with
              | None -> (* The thing is already declared *) ok ()
              | Some expr ->
                  let* v = eval_expr expr in
                  map_store (Store.declare_value pname v))
        in
        Normal
    | AilSbreak -> ok Break
    | AilScontinue -> ok Continue
    | AilSgoto label -> ok (Goto label)
    | AilSswitch (guard, stmt) -> (
        let* guard_v = eval_expr guard in
        let^ guard_v = cast_aggregate_to_int guard_v in
        let* res = exec_case guard_v stmt in
        match res with
        | Continue | Returned _ | Normal | Goto _ -> ok res
        | Break -> ok Normal
        | Case _ ->
            (* Case statement finished without finding a match: continue without executing anything *)
            ok Normal)
    | AilScase_rangeGNU (_, _, _)
    | AilSdefault _ | AilSpar _
    | AilSreg_store (_, _)
    | AilSmarker (_, _) ->
        Fmt.kstr not_impl "Unsupported statement: %a" Fmt_ail.pp_stmt astmt

  and exec_fun (fundef : fundef) ~(args : Agv.t list) state =
    let open Csymex in
    let open Csymex.Syntax in
    (* Put arguments in store *)
    let name, (loc, _, _, params, stmt) = fundef in
    let@ () = with_loc ~loc in
    L.debug (fun m -> m "Executing function %a" Fmt_ail.pp_sym name);
    L.trace (fun m -> m "Was given arguments: %a" (Fmt.Dump.list Agv.pp) args);
    let* ptys = get_param_tys name in
    let ps = List.combine3 params ptys args in
    let store = mk_store ps in
    let** res, store, state = exec_body stmt store state in
    let++ state = dealloc_store store state in
    (* We model void as zero, it should never be used anyway *)
    (res, state)

  let init_prog_state (prog : Ail_tys.linked_program) =
    let open Csymex.Syntax in
    (* Produce_zero will be useful when Kayvan allows for knowing when no declaration is given. *)
    let _produce_zero (ptr : [< T.sptr ] Typed.t) ty (state : State.t) =
      let loc = Typed.Ptr.loc ptr in
      let offset = Typed.Ptr.ofs ptr in
      let* len = Layout.size_of_s ty in
      let block =
        With_origin.
          {
            node =
              Freeable.Alive [ Ctree_block.MemVal { offset; len; v = SZeros } ];
            info = None;
          }
      in
      let serialized : State.serialized =
        { heap = [ (loc, block) ]; globs = [] }
      in
      let+ state = State.produce serialized state in
      Ok state
    in
    let produce_value (ptr : [< T.sptr ] Typed.t) ty expr (state : State.t) =
      let** v, _, state = eval_expr expr Store.empty state in
      let+ state = State.produce_aggregate ptr ty v state in
      Ok state
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
