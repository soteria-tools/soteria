open Charon
open Rustsymex
open Rustsymex.Syntax
open Typed.Syntax
open Typed.Infix
open Rust_val

module M (State : State_intf.S) = struct
  module Sptr = State.Sptr
  module Core = Core.M (State)
  module Encoder = Encoder.Make (Sptr)

  type nonrec rust_val = Sptr.t rust_val

  let pp_rust_val = pp_rust_val Sptr.pp

  let zeroed (fun_sig : UllbcAst.fun_sig) ~args:_ state =
    match Layout.zeroed ~null_ptr:Sptr.null_ptr fun_sig.output with
    | Some v -> Result.ok (v, state)
    | None -> State.error (`StdErr "Non-zeroable type") state

  let array_repeat (gen_args : Types.generic_args) ~args state =
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

  let array_index (idx_op : Types.builtin_index_op)
      (gen_args : Types.generic_args) ~args state =
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
    let ty = List.hd gen_args.types in
    let** ptr' = Sptr.offset ~ty ptr idx |> State.lift_err state in
    if not idx_op.is_range then
      if%sat 0s <=@ idx &&@ (idx <@ size) then
        Result.ok (Ptr (ptr', None), state)
      else State.error `OutOfBounds state
    else
      let range_end = as_base_of ~ty:Typed.t_int (List.nth args 2) in
      if%sat 0s <=@ idx &&@ (idx <=@ range_end) &&@ (range_end <=@ size) then
        let size = range_end -@ idx in
        Result.ok (Ptr (ptr', Some size), state)
      else State.error `OutOfBounds state

  (* Some array accesses are ran on functions, so we handle those here and redirect them.
     Eventually, it would be good to maybe make a Charon pass that gets rid of these before. *)
  let array_index_fn (fun_sig : UllbcAst.fun_sig) ~args state =
    let ptr, range, mode, gargs, range_id =
      match (args, fun_sig.inputs) with
      (* Unfortunate, but right now i don't have a better way to handle this... *)
      | ( [ ptr; Struct range ],
          [
            TRef
              ( _,
                TAdt { id = TBuiltin ((TArray | TSlice) as mode); generics },
                _ );
            TAdt { id = TAdtId range_id; _ };
          ] ) ->
          (ptr, range, mode, generics, range_id)
      | _ -> failwith "Unexpected input type"
    in
    let range_item =
      match (Crate.get_adt range_id).item_meta.lang_item with
      | Some item -> item
      | None -> failwith "Unexpected range item"
    in
    let size =
      match (ptr, gargs.const_generics) with
      (* Array with static size *)
      | _, [ size ] -> Typed.int @@ Charon_util.int_of_const_generic size
      | Ptr (_, Some size), [] -> Typed.cast size
      | _ -> failwith "array_index (fn): couldn't calculate size"
    in
    let idx_from, idx_to =
      match (range_item, range) with
      | "RangeFull", [] -> (Base 0s, Base size)
      | "RangeFrom", [ from ] -> (from, Base size)
      | "RangeTo", [ to_ ] -> (Base 0s, to_)
      | "Range", [ from; to_ ] -> (from, to_)
      | "RangeInclusive", [ from; Base to_ ] ->
          (from, Base (Typed.cast to_ +@ 1s))
      | "RangeToInclusive", [ Base to_ ] ->
          (Base 0s, Base (Typed.cast to_ +@ 1s))
      | _ -> Fmt.failwith "array_index (fn): unexpected range %s" range_item
    in
    let idx_op : Types.builtin_index_op =
      { is_array = mode = TArray; mutability = RShared; is_range = true }
    in
    array_index idx_op gargs ~args:[ ptr; idx_from; idx_to ] state

  let array_slice ~mut:_ (gen_args : Types.generic_args) ~args state =
    let size =
      match gen_args.const_generics with
      | [ size ] -> Charon_util.int_of_const_generic size
      | _ -> failwith "array_slice: unexpected generic constants"
    in
    match args with
    | [ Ptr (ptr, None) ] -> Result.ok (Ptr (ptr, Some (Typed.int size)), state)
    | _ -> failwith "array_index: unexpected arguments"

  let box_new (gen_args : Types.generic_args) ~args state =
    let ty, v =
      match (gen_args, args) with
      | { types = [ ty ]; _ }, [ v ] -> (ty, v)
      | _ -> failwith "box new: invalid arguments"
    in
    let** ptr, state = State.alloc_ty ty state in
    let++ (), state = State.store ptr ty v state in
    (Ptr ptr, state)

  let from_raw_parts ~args state =
    match args with
    | [ Ptr (ptr, _); Base meta ] -> Result.ok (Ptr (ptr, Some meta), state)
    | [ Base v; Base meta ] ->
        let* v = cast_checked ~ty:Typed.t_int v in
        let++ ptr = Sptr.offset Sptr.null_ptr v |> State.lift_err state in
        (Ptr (ptr, Some meta), state)
    | _ ->
        Fmt.failwith "from_raw_parts: invalid arguments %a"
          Fmt.(list ~sep:comma pp_rust_val)
          args

  let nop ~args:_ state = Result.ok (Tuple [], state)

  let float_is (fp : Svalue.FloatClass.t) ~args state =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is: invalid argument"
    in
    let* v =
      of_opt_not_impl ~msg:"float_is expects float" @@ Typed.cast_float v
    in
    let res =
      match fp with
      | NaN -> Typed.is_nan v
      | Normal -> Typed.is_normal v
      | Infinite -> Typed.is_infinite v
      | Zero -> Typed.is_zero v
      | Subnormal -> Typed.is_subnormal v
    in
    Result.ok (Base (Typed.int_of_bool res), state)

  let float_is_finite ~args state =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is_finite: invalid argument"
    in
    let* v =
      of_opt_not_impl ~msg:"float_is_finite expects float" @@ Typed.cast_float v
    in
    let res = Typed.((not (is_nan v)) &&@ not (is_infinite v)) in
    Result.ok (Base (Typed.int_of_bool res), state)

  let float_is_sign pos ~args state =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is_sign: invalid argument"
    in
    let* v =
      of_opt_not_impl ~msg:"float_is_sign expects float" @@ Typed.cast_float v
    in
    let res =
      if pos then Typed.(leq_f (float_like v 0.) v)
      else Typed.(leq_f v (float_like v (-0.)))
    in
    let res = res ||@ Typed.is_nan v in
    Result.ok (Base (Typed.int_of_bool res), state)

  let _mk_box ptr =
    let non_null = Struct [ ptr ] in
    let phantom_data = Struct [] in
    let unique = Struct [ non_null; phantom_data ] in
    let allocator = Struct [] in
    Struct [ unique; allocator ]

  let fixme_try_cleanup ~args:_ state =
    (* FIXME: for some reason Charon doesn't translate std::panicking::try::cleanup? Instead
              we return a Box to a null pointer, hoping the client code doesn't access it. *)
    let box = _mk_box (Ptr (Sptr.null_ptr, Some 0s)) in
    Result.ok (box, state)

  let fixme_box_new (fun_sig : UllbcAst.fun_sig) ~args state =
    let ty = List.hd fun_sig.inputs in
    let value = List.hd args in
    let** ptr, state = State.alloc_ty ty state in
    let++ (), state = State.store ptr ty value state in
    let box = _mk_box (Ptr ptr) in
    (box, state)

  let fixme_null_ptr ~args:_ state = Result.ok (Ptr (Sptr.null_ptr, None), state)

  let alloc_impl ~args state =
    let module Alloc = Alloc.M (State) in
    let* size, align, zeroed =
      match args with
      | [
       _alloc; Struct [ Base size; Struct [ Enum (align, []) ] ]; Base zeroed;
      ] ->
          let+ zeroed = cast_checked ~ty:Typed.t_int zeroed in
          (size, align, Typed.bool_of_int zeroed)
      | _ ->
          Fmt.kstr not_impl "alloc_impl: invalid arguments: %a"
            Fmt.(list ~sep:(any ", ") pp_rust_val)
            args
    in
    if%sat size ==@ 0s then
      Result.ok (Ptr (Sptr.null_ptr_of (Typed.cast align), Some 0s), state)
    else
      let* zeroed = if%sat zeroed then return true else return false in
      (* allocate *)
      let++ ptr, state =
        Alloc.alloc ~zeroed ~args:[ Base size; Base align ] state
      in
      let ptr =
        match ptr with Ptr (p, _) -> p | _ -> failwith "Expected Ptr"
      in
      (* construct the Result<NonNull<[u8]>> *)
      (Enum (0s, [ Struct [ Ptr (ptr, Some size) ] ]), state)
end
