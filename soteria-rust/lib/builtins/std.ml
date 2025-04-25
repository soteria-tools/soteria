open Charon
open Soteria_symex.Compo_res
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Typed.Infix
open Charon_util

module M (Heap : Heap_intf.S) = struct
  module Sptr = Heap.Sptr
  module Core = Core.M (Heap)

  type nonrec rust_val = Sptr.t rust_val

  let pp_rust_val = pp_rust_val Sptr.pp

  let assert_ ~crate:_ ~(args : rust_val list) ~state =
    let open Typed.Infix in
    let* to_assert =
      match args with
      | [ Base t ] -> cast_checked t ~ty:Typed.t_int
      | _ -> not_impl "to_assert with non-one arguments"
    in
    if%sat to_assert ==@ 0s then Heap.error `FailedAssert state
    else Result.ok (Charon_util.unit_, state)

  let assume ~crate:_ ~args ~state =
    let* to_assume =
      match args with
      | [ Base t ] -> cast_checked t ~ty:Typed.t_int
      | _ -> not_impl "assume with non-one arguments"
    in
    L.debug (fun g -> g "Assuming: %a\n" Typed.ppa to_assume);
    let* () = assume [ Typed.bool_of_int to_assume ] in
    Result.ok (Charon_util.unit_, state)

  let kani_nondet (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty = fun_sig.output in
    let* value = Layout.nondet ty in
    Result.ok (value, state)

  let unchecked_op op (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral ty :: _ -> return ty
      | _ -> not_impl "unchecked_op wrong inputs"
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] -> return (left, right)
      | _ -> not_impl "unchecked_op with not two arguments"
    in
    let++ res = Core.eval_lit_binop op ty left right state in
    (Base res, state)

  let wrapping_op op (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ity =
      match fun_sig.inputs with
      | TLiteral (TInteger ity) :: _ -> return ity
      | tys ->
          Fmt.kstr not_impl "wrapping_op invalid type: %a"
            Fmt.(list Types.pp_ty)
            tys
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] -> return (left, right)
      | _ -> not_impl "wrapping_op with not two arguments"
    in
    let** res = Core.safe_binop op left right state in
    let* res = Core.wrap_value ity res in
    Result.ok (Base res, state)

  let checked_op op (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let* ty =
      match fun_sig.inputs with
      | TLiteral ty :: _ -> return ty
      | tys ->
          Fmt.kstr not_impl "wrapping_op invalid type: %a"
            Fmt.(list Types.pp_ty)
            tys
    in
    let* left, right =
      match args with
      | [ Base left; Base right ] -> return (left, right)
      | _ -> not_impl "wrapping_op with not two arguments"
    in
    let++ res = Core.eval_checked_lit_binop op ty left right state in
    (res, state)

  let is_some (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let val_ptr, opt_ty =
      match (args, fun_sig.inputs) with
      | [ Ptr ptr ], [ Types.TRef (_, ty, _) ] -> (ptr, ty)
      | _ ->
          failwith
            "is_some expects a single ptr argument and a single tref input type"
    in
    let** enum_value, state = Heap.load val_ptr opt_ty state in
    let+ discr =
      match enum_value with
      | Enum (discr, _) -> return discr
      | _ ->
          Fmt.kstr not_impl
            "expected value pointed to in is_some to be an enum, got %a"
            pp_rust_val enum_value
    in
    Ok (Base (Typed.int_of_bool (discr ==@ 1s)), state)

  let is_none (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let val_ptr, opt_ty =
      match (args, fun_sig.inputs) with
      | [ Ptr ptr ], [ Types.TRef (_, ty, _) ] -> (ptr, ty)
      | _ ->
          failwith
            "is_none expects a single ptr argument and a single tref input type"
    in
    let** enum_value, state = Heap.load val_ptr opt_ty state in
    let* discr =
      match enum_value with
      | Enum (discr, _) -> return discr
      | _ ->
          Fmt.kstr not_impl
            "expected value pointed to in is_none to be an enum, got %a"
            pp_rust_val enum_value
    in
    Result.ok (Base (Typed.int_of_bool (discr ==@ 0s)), state)

  let unwrap_opt ~crate:_ ~args ~state =
    let* discr, value =
      match args with
      | [ Enum (disc, value) ] -> return (disc, value)
      | _ -> not_impl "is_some expects a single option argument"
    in
    if%sat discr ==@ 0s then Heap.error (`StdErr "Unwrapped none") state
    else
      match value with
      | [ value ] -> Result.ok (value, state)
      | _ -> not_impl "option is some, but doesn't have one value"

  let unwrap_res ~crate:_ ~args ~state =
    let* discr, value =
      match args with
      | [ Enum (disc, value) ] -> return (disc, value)
      | _ -> not_impl "is_some expects a single option argument"
    in
    if%sat discr ==@ 1s then Heap.error (`StdErr "Unwrapped Err") state
    else
      match value with
      | [ value ] -> Result.ok (value, state)
      | _ -> not_impl "Result is Ok, but doesn't have one value"

  let eq_values ~neg (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args ~state =
    let rec aux state left right =
      match (left, right) with
      | Base left, Base right -> Result.ok (left ==@ right, state)
      | Struct lefts, Struct rights
      | Array lefts, Array rights
      | Tuple lefts, Tuple rights ->
          if List.compare_lengths lefts rights <> 0 then
            Result.ok (Typed.v_false, state)
          else aux_list Typed.v_true state lefts rights
      | Enum (l_d, l_vs), Enum (r_d, r_vs) ->
          if List.compare_lengths l_vs r_vs <> 0 then
            Result.ok (Typed.v_false, state)
          else aux_list (l_d ==@ r_d) state l_vs r_vs
      | _ ->
          Fmt.kstr not_impl "Unexpected eq_values pair: %a / %a" pp_rust_val
            left pp_rust_val right
    and aux_list init state lefts rights =
      if init = Typed.v_false then Result.ok (Typed.v_false, state)
      else
        match (lefts, rights) with
        | [], [] -> Result.ok (init, state)
        | l :: lefts, r :: rights ->
            let** b_val, state = aux state l r in
            aux_list (b_val &&@ init) state lefts rights
        | [], _ | _, [] -> Result.ok (Typed.v_false, state)
    in
    let left_ptr, right_ptr =
      match args with
      | [ Ptr left; Ptr right ] -> (left, right)
      | _ -> failwith "eq_values expects two arguments"
    in
    let** left, right, state =
      match fun_sig.inputs with
      | Types.TRef (_, (TRef (_, ty, _) as outer_ty), _) :: _ ->
          (* STD provides an implementation of eq for references (&T), where the arguments are
             thus &&T -- we handle this here by adding an indirection. *)
          let** left, state = Heap.load left_ptr outer_ty state in
          let** right, state = Heap.load right_ptr outer_ty state in
          let left_ptr = as_ptr left in
          let right_ptr = as_ptr right in
          let** left, state = Heap.load left_ptr ty state in
          let++ right, state = Heap.load right_ptr ty state in
          (left, right, state)
      | Types.TRef (_, ty, _) :: _ ->
          let** left, state = Heap.load left_ptr ty state in
          let++ right, state = Heap.load right_ptr ty state in
          (left, right, state)
      | ty :: _ ->
          Fmt.kstr not_impl "Unexpected type for eq_values: %a" Types.pp_ty ty
      | _ -> not_impl "Error: eq_values received no arguments?"
    in
    let++ b_val, state = aux state left right in
    let b_val = if neg then Typed.not b_val else b_val in
    let res = Typed.int_of_bool b_val in
    (Base res, state)

  let bool_not ~crate:_ ~args ~state =
    let b_ptr =
      match args with
      | [ Ptr b ] -> b
      | _ -> failwith "bool_not expects one Ptr argument"
    in
    let++ b_rval, state = Heap.load b_ptr (Types.TLiteral TBool) state in
    let b_int = as_base_of ~ty:Typed.t_int b_rval in
    let b_int' = Typed.not_int_bool b_int in
    (Base b_int', state)

  let zeroed (fun_sig : UllbcAst.fun_sig) ~crate:_ ~args:_ ~state =
    match Layout.zeroed ~null_ptr:Sptr.null_ptr fun_sig.output with
    | Some v -> Result.ok (v, state)
    | None -> Heap.error (`StdErr "Non-zeroable type") state

  let array_repeat (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let rust_val, size =
      match (args, gen_args.const_generics) with
      | [ rust_val ], [ size ] ->
          (rust_val, Charon_util.int_of_const_generic size)
      | args, cgens ->
          Fmt.failwith
            "array_repeat: unexpected params / generic constants: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            cgens
    in
    Result.ok (Array (List.init size (fun _ -> rust_val)), state)

  let array_index (idx_op : Expressions.builtin_index_op)
      (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let ptr, size =
      match (idx_op.is_array, List.hd args, gen_args.const_generics) with
      (* Array with static size *)
      | true, Ptr (ptr, None), [ size ] ->
          (ptr, Typed.int @@ Charon_util.int_of_const_generic size)
      | false, Ptr (ptr, Some size), [] -> (ptr, Typed.cast size)
      | _ ->
          Fmt.failwith "array_index: unexpected arguments: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            gen_args.const_generics
    in
    (* TODO: take into account idx.mutability *)
    let idx = as_base_of ~ty:Typed.t_int (List.nth args 1) in
    if%sat 0s <=@ idx &&@ (idx <@ size) then
      let ty = List.hd gen_args.types in
      let ptr' = Sptr.offset ~ty ptr idx in
      if not idx_op.is_range then Result.ok (Ptr (ptr', None), state)
      else
        let range_end = as_base_of ~ty:Typed.t_int (List.nth args 2) in
        (* range_end is exclusive *)
        if%sat idx <=@ range_end &&@ (range_end <=@ size) then
          let size = range_end -@ idx in
          Result.ok (Ptr (ptr', Some size), state)
        else
          (* not sure this is the right diagnostic *)
          Heap.error `OutOfBounds state
    else Heap.error `OutOfBounds state

  (* Some array accesses are ran on functions, so we handle those here and redirect them.
     Eventually, it would be good to maybe make a Charon pass that gets rid of these before. *)
  let array_index_fn (fun_sig : UllbcAst.fun_sig) ~crate ~args ~state =
    let mode, gargs, range_ty_id =
      match fun_sig.inputs with
      (* Unfortunate, but right now i don't have a better way to handle this... *)
      | [
       TRef (_, TAdt (TBuiltin ((TArray | TSlice) as mode), gargs), _);
       TAdt (TAdtId range_ty_id, _);
      ] ->
          (mode, gargs, range_ty_id)
      | _ -> failwith "Unexpected input type"
    in
    let range_adt =
      Types.TypeDeclId.Map.find range_ty_id UllbcAst.(crate.type_decls)
    in
    let range_name =
      match List.rev range_adt.item_meta.name with
      | PeIdent (name, _) :: _ -> name
      | _ -> failwith "Unexpected range name"
    in
    let size =
      match (args, gargs.const_generics) with
      (* Array with static size *)
      | _, [ size ] -> Typed.int @@ Charon_util.int_of_const_generic size
      | Ptr (_, Some size) :: _, [] -> Typed.cast size
      | _ -> failwith "array_index (fn): couldn't calculate size"
    in
    let ptr, idx_from, idx_to =
      match (args, range_name) with
      | [ ptr; Struct [] ], "RangeFull" -> (ptr, Base 0s, Base size)
      | [ ptr; Struct [ idx_from ] ], "RangeFrom" -> (ptr, idx_from, Base size)
      | [ ptr; Struct [ idx_to ] ], "RangeTo" -> (ptr, Base 0s, idx_to)
      | [ ptr; Struct [ idx_from; idx_to ] ], "Range" -> (ptr, idx_from, idx_to)
      | [ ptr; Struct [ idx_from; idx_to ] ], "RangeInclusive" ->
          let idx_to = as_base_of ~ty:Typed.t_int idx_to in
          (ptr, idx_from, Base (idx_to +@ 1s))
      | [ ptr; Struct [ idx_to ] ], "RangeToInclusive" ->
          let idx_to = as_base_of ~ty:Typed.t_int idx_to in
          (ptr, Base 0s, Base (idx_to +@ 1s))
      | _ -> Fmt.failwith "array_index (fn): unexpected range %s" range_name
    in
    let idx_op : Expressions.builtin_index_op =
      { is_array = mode = TArray; mutability = RShared; is_range = true }
    in
    array_index idx_op gargs ~crate ~args:[ ptr; idx_from; idx_to ] ~state

  let array_slice ~mut:_ (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let size =
      match gen_args.const_generics with
      | [ size ] -> Charon_util.int_of_const_generic size
      | _ -> failwith "array_slice: unexpected generic constants"
    in
    match args with
    | [ Ptr (ptr, None) ] -> Result.ok (Ptr (ptr, Some (Typed.int size)), state)
    | _ -> failwith "array_index: unexpected arguments"

  let slice_len _ ~crate:_ ~args ~state =
    match args with
    | [ Ptr (_, Some size) ] -> Result.ok (Base size, state)
    | _ -> failwith "slice_len: unexpected arguments"

  let discriminant_value (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let value_ptr =
      match args with
      | [ Ptr value_ptr ] -> value_ptr
      | _ -> failwith "discriminant_value: unexpected arguments"
    in
    let value_ty =
      match funsig.inputs with
      | [ TRef (_, value_ty, _) ] -> value_ty
      | _ -> failwith "discriminant_value: unexpected arguments"
    in
    let++ value, state = Heap.load value_ptr value_ty state in
    match value with
    | Enum (discr, _) -> (Base discr, state)
    | _ -> failwith "discriminant_value: unexpected value"

  let to_string _ ~crate:_ ~args ~state =
    match args with
    | [ (Ptr (_, Some len) as slice) ] ->
        Result.ok (Struct [ slice; Base len ], state)
    | _ -> failwith "to_string: unexpected value"

  let str_chars _ ~crate:_ ~args ~state =
    let ptr, len =
      match args with
      | [ Ptr (ptr, Some len) ] -> (ptr, Typed.cast len)
      | _ -> failwith "str_chars: unexpected value"
    in
    let ty = Types.TLiteral TChar in
    let arr_end = Sptr.offset ~ty ptr len in
    Result.ok (Struct [ Ptr (ptr, None); Ptr (arr_end, None) ], state)

  let iter_nth (fun_sig : GAst.fun_sig) ~crate ~args ~state =
    let iter_ptr, idx =
      match args with
      | [ Ptr iter_ptr; Base idx ] -> (iter_ptr, idx)
      | _ -> failwith "iter_nth: unexpected value"
    in
    let* idx = cast_checked idx ~ty:Typed.t_int in
    let iter_ty, sub_ty =
      match fun_sig.inputs with
      | TRef (_, (TAdt (TAdtId adt_id, _) as iter_ty), _) :: _ -> (
          let adt = Std_types.get_adt ~crate adt_id in
          match adt.kind with
          | Struct (_ :: { field_ty = TRawPtr (sub_ty, _); _ } :: _) ->
              (iter_ty, sub_ty)
          | _ -> failwith "iter_nth: unexpected signature")
      | _ -> failwith "iter_nth: unexpected signature"
    in
    let** iter, state = Heap.load iter_ptr iter_ty state in
    let start_ptr, end_ptr =
      match iter with
      | Struct [ Ptr (start_ptr, None); Ptr (end_ptr, None) ] ->
          (start_ptr, end_ptr)
      | _ -> failwith "iter_nth: unexpected iter structure"
    in
    if%sat Sptr.is_same_loc start_ptr end_ptr then
      let ptr = Sptr.offset ~ty:sub_ty start_ptr idx in
      let dist = Sptr.distance ptr end_ptr in
      if%sat dist <@ 0s then
        let iter' = Struct [ Ptr (ptr, None); Ptr (end_ptr, None) ] in
        let** value, state = Heap.load (ptr, None) sub_ty state in
        let++ (), state = Heap.store iter_ptr iter_ty iter' state in
        (Enum (1s, [ value ]), state)
      else
        (* return None *)
        Result.ok (Enum (0s, []), state)
    else Heap.error `UBPointerArithmetic state

  let deref (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    (* This works for string deref -- don't know about the rest *)
    let ptr =
      match args with
      | [ Ptr ptr ] -> ptr
      | _ -> failwith "deref: unexpected argument"
    in
    let ty =
      match funsig.inputs with
      | TRef (_, ty, _) :: _ -> ty
      | _ -> failwith "deref: unexpected signature"
    in
    let++ v, state = Heap.load ptr ty state in
    match v with
    | Struct [ v; _ ] -> (v, state)
    | _ -> Fmt.failwith "deref: unexpected value: %a" pp_rust_val v

  let str_len (fun_sig : GAst.fun_sig) ~crate:_ ~args ~state =
    let str_ptr = as_ptr @@ List.hd args in
    let str_ty =
      match fun_sig.inputs with
      | [ TRef (_, str_ty, _) ] -> str_ty
      | _ -> failwith "str_len: unexpected input"
    in
    let++ str_obj, state = Heap.load str_ptr str_ty state in
    match str_obj with
    | Struct [ Ptr (_, Some meta); _ ] -> (Base meta, state)
    | _ -> failwith "str_len: unexpected string type"

  let assert_zero_is_valid (fun_sig : GAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty =
      List.hd
        (List.hd fun_sig.generics.trait_clauses).trait.binder_value
          .decl_generics
          .types
    in
    match Layout.zeroed ~null_ptr:Sptr.null_ptr ty with
    | None -> Heap.error (`Panic "core::intrinsics::assert_zero_valid") state
    | _ -> Result.ok (Tuple [], state)

  let size_of (fun_sig : GAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    let+ size = Layout.size_of_s ty in
    Ok (Base size, state)

  let size_of_val (fun_sig : GAst.fun_sig) ~crate:_ ~args ~state =
    let ty =
      match fun_sig.inputs with
      | [ TRawPtr (ty, _) ] -> ty
      | _ -> failwith "size_of_val: Invalid input type"
    in
    match (ty, args) with
    | TAdt (TBuiltin TSlice, { types = [ sub_ty ]; _ }), [ Ptr (_, Some meta) ]
      ->
        let* len = cast_checked meta ~ty:Typed.t_int in
        let+ size = Layout.size_of_s sub_ty in
        let size = size *@ len in
        Ok (Base size, state)
    | TAdt (TBuiltin TStr, _), [ Ptr (_, Some meta) ] ->
        let+ len = cast_checked meta ~ty:Typed.t_int in
        let size = Layout.size_of_int_ty U8 in
        let size = Typed.int size *@ len in
        Ok (Base size, state)
    | ty, _ ->
        let+ size = Layout.size_of_s ty in
        Ok (Base size, state)

  let min_align_of ~in_input (fun_sig : GAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty =
      if in_input then
        match fun_sig.inputs with
        | [ TRawPtr (ty, _) ] -> ty
        | _ -> failwith "min_align_of: invalid input type"
      else
        (List.hd fun_sig.generics.trait_clauses).trait.binder_value
          .decl_generics
          .types
        |> List.hd
    in
    let layout = Layout.layout_of ty in
    let align = Typed.int layout.align in
    Result.ok (Base align, state)

  let box_new (gen_args : Types.generic_args) ~crate:_ ~args ~state =
    let ty, v =
      match (gen_args, args) with
      | { types = [ ty ]; _ }, [ v ] -> (ty, v)
      | _ -> failwith "box new: invalid arguments"
    in
    let** ptr, state = Heap.alloc_ty ty state in
    let++ (), state = Heap.store ptr ty v state in
    (Ptr ptr, state)

  let ptr_op ?(byte = false) op (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let ptr, meta, v =
      match args with
      | [ Ptr (ptr, meta); Base v ] -> (ptr, meta, v)
      | _ -> failwith "ptr_add: invalid arguments"
    in
    let* v = cast_checked v ~ty:Typed.t_int in
    let ty =
      if byte then Types.TLiteral (TInteger U8)
      else
        match funsig.inputs with
        | TRawPtr (ty, _) :: _ -> ty
        | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let v = if op = Expressions.Add then v else ~-v in
    let ptr' = Sptr.offset ~ty ptr v in
    if%sat Sptr.constraints ptr' then Result.ok (Ptr (ptr', meta), state)
    else Heap.error `Overflow state

  let box_into_raw _ ~crate:_ ~args ~state =
    (* internally a box is exactly a pointer so nothing to do *)
    let box_ptr = List.hd args in
    Result.ok (box_ptr, state)

  let ptr_offset_from (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let ptr1, ptr2 =
      match args with
      | [ Ptr (ptr1, _); Ptr (ptr2, _) ] -> (ptr1, ptr2)
      | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let ty =
      match funsig.inputs with
      | TRawPtr (ty, _) :: _ -> ty
      | _ -> failwith "ptr_offset_from: invalid arguments"
    in
    let* size = Layout.size_of_s ty in
    if%sat Sptr.is_same_loc ptr1 ptr2 &&@ (size >@ 0s) then
      let size = Typed.cast size in
      let off = Sptr.distance ptr1 ptr2 in
      if%sat off %@ size ==@ 0s then Result.ok (Base (off /@ size), state)
      else Heap.error `UBPointerComparison state
    else Heap.error `UBPointerComparison state

  let black_box _ ~crate:_ ~args ~state =
    match args with
    | [ v ] -> Result.ok (v, state)
    | _ -> failwith "black_box: invalid arguments"

  let transmute (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let from_ty = List.hd funsig.inputs in
    let to_ty = funsig.output in
    let v = List.hd args in
    let++ v = Heap.lift_err state @@ Encoder.transmute ~from_ty ~to_ty v in
    (v, state)

  let copy_nonoverlapping (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    let (from_ptr, _), (to_ptr, _), len =
      match args with
      | [ Ptr from_ptr; Ptr to_ptr; Base len ] ->
          (from_ptr, to_ptr, Typed.cast len)
      | _ -> failwith "copy_nonoverlapping: invalid arguments"
    in
    let ty =
      match funsig.inputs with
      | TRawPtr (ty, _) :: _ -> ty
      | _ -> failwith "copy_nonoverlapping: invalid arguments"
    in
    let* ty_size = Layout.size_of_s ty in
    let size = ty_size *@ len in
    let** () =
      if%sat Sptr.is_at_null_loc from_ptr ||@ Sptr.is_at_null_loc to_ptr then
        Heap.error `NullDereference state
      else
        (* check for overlap *)
        let from_ptr_end = Sptr.offset from_ptr size in
        let to_ptr_end = Sptr.offset to_ptr size in
        if%sat
          Sptr.is_same_loc from_ptr to_ptr
          &&@ (Sptr.distance from_ptr to_ptr_end
              <@ 0s
              &&@ (Sptr.distance to_ptr from_ptr_end <@ 0s))
        then Heap.error (`StdErr "copy_nonoverlapping overlapped") state
        else Result.ok ()
    in
    let++ (), state =
      Heap.copy_nonoverlapping ~dst:(to_ptr, None) ~src:(from_ptr, None) ~size
        state
    in
    (Tuple [], state)

  let mul_add _ ~crate:_ ~args ~state =
    match args with
    | [ Base a; Base b; Base c ] ->
        let a, b, c = (Typed.cast a, Typed.cast b, Typed.cast c) in
        Result.ok (Base ((a *@ b) +@ c), state)
    | _ -> failwith "mul_add expects three arguments"

  let abs _ ~crate:_ ~args ~state =
    match args with
    | [ Base v ] ->
        Result.ok (Base (Typed.cast @@ Typed.abs @@ Typed.cast v), state)
    | _ -> failwith "abs expects one argument"

  let write_bytes (fun_sig : GAst.fun_sig) ~crate:_ ~args ~state =
    let ptr, dst, v, count =
      match args with
      | [ Ptr ((ptr, _) as dst); Base v; Base count ] -> (ptr, dst, v, count)
      | _ -> failwith "unexpected write_bytes arguments"
    in
    let* count = cast_checked count ~ty:Typed.t_int in
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    let* size = Layout.size_of_s ty in
    let size = size *@ count in
    (* TODO: if v == 0, then we can replace this mess by initialising a Zeros subtree *)
    if%sat v ==@ 0s then
      let++ (), state = Heap.zeros dst size state in
      (Tuple [], state)
    else
      match Typed.kind size with
      | Int bytes ->
          let list = List.init (Z.to_int bytes) Fun.id in
          let++ (), state =
            Result.fold_list list ~init:((), state) ~f:(fun ((), state) i ->
                let ptr = Sptr.offset ptr @@ Typed.int i in
                Heap.store (ptr, None) (Types.TLiteral (TInteger U8)) (Base v)
                  state)
          in
          (Tuple [], state)
      | _ -> failwith "write_bytes: don't know how to handle symbolic sizes"

  let assert_inhabited (fun_sig : GAst.fun_sig) ~crate:_ ~args:_ ~state =
    let ty =
      (List.hd fun_sig.generics.trait_clauses).trait.binder_value.decl_generics
        .types
      |> List.hd
    in
    if Layout.is_inhabited ty then Result.ok (Tuple [], state)
    else Heap.error (`Panic "core::intrinsics::assert_inhabited") state

  let from_raw_parts ~crate:_ ~args ~state =
    match args with
    | [ Ptr (ptr, _); Base meta ] -> Result.ok (Ptr (ptr, Some meta), state)
    | _ -> failwith "from_raw_parts: invalid arguments"

  let nop ~crate:_ ~args:_ ~state = Result.ok (Tuple [], state)

  let is_val_statically_known ~crate:_ ~args:_ ~state =
    (* see: https://doc.rust-lang.org/std/intrinsics/fn.is_val_statically_known.html *)
    let* b = Rustsymex.nondet Typed.t_bool in
    Result.ok (Base (Typed.int_of_bool b), state)

  let std_assume ~crate:_ ~args ~state =
    match args with
    | [ Base cond ] ->
        let* cond = cast_checked ~ty:Typed.t_int cond in
        if%sat Typed.bool_of_int cond then Result.ok (Tuple [], state)
        else Heap.error (`Panic "core::intrinsics::assume") state
    | _ -> failwith "std_assume: invalid arguments"

  let exact_div (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    match (funsig.inputs, args) with
    | TLiteral lit :: _, [ Base l; Base r ] ->
        let* l, r, ty = cast_checked2 l r in
        let open Typed in
        let** res = Core.eval_lit_binop Expressions.Div lit l r state in
        if is_float ty then Result.ok (Base res, state)
        else
          if%sat (not (r ==@ 0s)) &&@ (l %@ cast r ==@ 0s) then
            Result.ok (Base res, state)
          else Heap.error (`Panic "core::intrinsics::exact_div") state
    | _ -> failwith "exact_div: invalid arguments"

  let ctpop (funsig : GAst.fun_sig) ~crate:_ ~args ~state =
    match args with
    | [ Base v ] -> (
        let ty =
          match funsig.inputs with
          | [ TLiteral (TInteger ty) ] -> ty
          | _ -> failwith "ctpop: invalid arguments"
        in
        let bits = 8 * Layout.size_of_int_ty ty in
        let* v = cast_checked ~ty:Typed.t_int v in
        match Typed.kind v with
        | Int v ->
            let rec aux acc v =
              if Z.equal v Z.zero then Typed.int acc
              else
                let next = Z.shift_right v 1 in
                if Z.testbit v 0 then aux (succ acc) next else aux acc next
            in
            (* convert to unsigned *)
            let v =
              if Layout.is_signed ty then
                let maxv = Z.shift_left Z.one bits in
                if Z.(v < zero) then Z.(((v mod maxv) + maxv) mod maxv) else v
              else v
            in
            Result.ok (Base (aux 0 v), state)
        | _ ->
            (* convert to unsigned *)
            let* v =
              if Layout.is_signed ty then
                let max = Typed.nonzero_z (Z.shift_left Z.one bits) in
                if%sat v <@ 0s then return (((v %@ max) +@ max) %@ max)
                else return v
              else return v
            in
            let two = Typed.nonzero 2 in
            let res =
              List.init bits (fun i -> i)
              |> List.fold_left
                   (fun acc off ->
                     let pow = Typed.nonzero_z (Z.shift_left Z.one off) in
                     acc +@ (v /@ pow %@ two))
                   0s
            in
            Result.ok (Base (res :> Typed.T.cval Typed.t), state))
    | _ -> failwith "ctpop: invalid arguments"

  let compare_bytes ~crate:_ ~args ~state =
    let l, r, len =
      match args with
      | [ Ptr (l, _); Ptr (r, _); Base len ] -> (l, r, len)
      | _ -> failwith "compare_bytes: invalid arguments"
    in
    let* len = cast_checked ~ty:Typed.t_int len in
    let byte = Types.TLiteral (TInteger U8) in
    let rec aux l r len state =
      if%sat len ==@ 0s then Result.ok (Base 0s, state)
      else
        let** bl, state = Heap.load (l, None) byte state in
        let bl = as_base_of ~ty:Typed.t_int bl in
        let** br, state = Heap.load (r, None) byte state in
        let br = as_base_of ~ty:Typed.t_int br in
        if%sat bl ==@ br then
          let l = Sptr.offset l 1s in
          let r = Sptr.offset r 1s in
          aux l r (len -@ 1s) state
        else
          if%sat bl <@ br then Result.ok (Base (-1s), state)
          else Result.ok (Base 1s, state)
    in
    aux l r len state
end
