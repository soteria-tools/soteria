open Charon
module BV = Typed.BitVec
open Typed.Syntax
open Typed.Infix
open Rust_val

module M (State : State_intf.S) = struct
  module Core = Core.M (State)
  module Alloc = Alloc.M (State)
  module Encoder = Encoder.Make (State.Sptr)
  open State_monad.Make (State)
  open Syntax

  let zeroed (fun_sig : UllbcAst.fun_sig) _ =
    match Layout.zeroed ~null_ptr:(Sptr.null_ptr ()) fun_sig.output with
    | Some v -> ok v
    | None -> error (`StdErr "Non-zeroable type")

  let array_repeat (gen_args : Types.generic_args) args =
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
    ok (Array (List.init size (fun _ -> rust_val)))

  let array_index (idx_op : Types.builtin_index_op)
      (gen_args : Types.generic_args) args =
    let ptr, size =
      match (idx_op.is_array, List.hd args, gen_args.const_generics) with
      (* Array with static size *)
      | true, Ptr (ptr, None), [ size ] -> (ptr, BV.usize_of_const_generic size)
      | false, Ptr (ptr, Some size), [] -> (ptr, Typed.cast_i Usize size)
      | _ ->
          Fmt.failwith "array_index: unexpected arguments: %a / %a"
            Fmt.(list pp_rust_val)
            args
            Fmt.(list Types.pp_const_generic)
            gen_args.const_generics
    in
    (* TODO: take into account idx.mutability *)
    let idx = as_base_i Usize (List.nth args 1) in
    let ty = List.hd gen_args.types in
    let^^ ptr' = Sptr.offset ~ty ptr idx in
    if not idx_op.is_range then
      let+ () =
        State.assert_ (Usize.(0s) <=$@ idx &&@ (idx <$@ size)) `OutOfBounds
      in
      Ptr (ptr', None)
    else
      let range_end = as_base_i Usize (List.nth args 2) in
      let+ () =
        State.assert_
          (Usize.(0s)
          <=$@ idx
          &&@ (idx <=$@ range_end)
          &&@ (range_end <=$@ size))
          `OutOfBounds
      in
      let size = range_end -!@ idx in
      Ptr (ptr', Some size)

  (* Some array accesses are ran on functions, so we handle those here and redirect them.
     Eventually, it would be good to maybe make a Charon pass that gets rid of these before. *)
  let array_index_fn (fun_sig : UllbcAst.fun_sig) args =
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
      | _, [ size ] -> BV.usize_of_const_generic size
      | Ptr (_, Some size), [] -> Typed.cast size
      | _ -> failwith "array_index (fn): couldn't calculate size"
    in
    let idx_from, idx_to =
      match (range_item, range) with
      | "RangeFull", [] -> (Base Usize.(0s), Base size)
      | "RangeFrom", [ from ] -> (from, Base size)
      | "RangeTo", [ to_ ] -> (Base Usize.(0s), to_)
      | "Range", [ from; to_ ] -> (from, to_)
      | "RangeInclusive", [ from; Base to_ ] ->
          (from, Base (Typed.cast to_ +!@ Usize.(1s)))
      | "RangeToInclusive", [ Base to_ ] ->
          (Base Usize.(0s), Base (Typed.cast to_ +!@ Usize.(1s)))
      | _ -> Fmt.failwith "array_index (fn): unexpected range %s" range_item
    in
    let idx_op : Types.builtin_index_op =
      { is_array = mode = TArray; mutability = RShared; is_range = true }
    in
    array_index idx_op gargs [ ptr; idx_from; idx_to ]

  let array_slice ~mut:_ (gen_args : Types.generic_args) args =
    match (gen_args.const_generics, args) with
    | [ size ], [ Ptr (ptr, None) ] ->
        let size = BV.usize_of_const_generic size in
        ok (Ptr (ptr, Some size))
    | _ -> failwith "array_index: unexpected arguments"

  let box_new (gen_args : Types.generic_args) args =
    let ty, v =
      match (gen_args, args) with
      | { types = [ ty ]; _ }, [ v ] -> (ty, v)
      | _ -> failwith "box new: invalid arguments"
    in
    let* ptr = State.alloc_ty ty in
    let+ () = State.store ptr ty v in
    Ptr ptr

  let from_raw_parts args =
    match args with
    | [ Ptr (ptr, _); Base meta ] -> ok (Ptr (ptr, Some meta))
    | [ Base v; Base meta ] ->
        let v = Typed.cast_i Usize v in
        let^^+ ptr = Sptr.offset (Sptr.null_ptr ()) v in
        Ptr (ptr, Some meta)
    | _ ->
        Fmt.failwith "from_raw_parts: invalid arguments %a"
          Fmt.(list ~sep:comma pp_rust_val)
          args

  let nop _ = ok (Tuple [])

  let float_is (fp : Svalue.FloatClass.t) args =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is: invalid argument"
    in
    let v = Typed.cast_float v in
    let res =
      match fp with
      | NaN -> Typed.Float.is_nan v
      | Normal -> Typed.Float.is_normal v
      | Infinite -> Typed.Float.is_infinite v
      | Zero -> Typed.Float.is_zero v
      | Subnormal -> Typed.Float.is_subnormal v
    in
    ok (Base (BV.of_bool res))

  let float_is_finite args =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is_finite: invalid argument"
    in
    let v = Typed.cast_float v in
    let res = Typed.((not (Float.is_nan v)) &&@ not (Float.is_infinite v)) in
    ok (Base (BV.of_bool res))

  let float_is_sign pos args =
    let v =
      match args with
      | [ Base f ] -> f
      | _ -> failwith "float_is_sign: invalid argument"
    in
    let v = Typed.cast_float v in
    let res =
      if pos then Typed.Float.(leq (like v 0.) v)
      else Typed.Float.(leq v (like v (-0.)))
    in
    let res = res ||@ Typed.Float.is_nan v in
    ok (Base (BV.of_bool res))

  let _mk_box ptr =
    let non_null = Struct [ ptr ] in
    let phantom_data = Struct [] in
    let unique = Struct [ non_null; phantom_data ] in
    let allocator = Struct [] in
    Struct [ unique; allocator ]

  let fixme_try_cleanup _ =
    (* FIXME: for some reason Charon doesn't translate std::panicking::try::cleanup? Instead
              we return a Box to a null pointer, hoping the client code doesn't access it. *)
    let meta = Usize.(0s) in
    let box = _mk_box (Ptr (Sptr.null_ptr (), Some meta)) in
    ok box

  let fixme_box_new (fun_sig : UllbcAst.fun_sig) args =
    let ty = List.hd fun_sig.inputs in
    let value = List.hd args in
    let* ptr = State.alloc_ty ty in
    let+ () = State.store ptr ty value in
    _mk_box (Ptr ptr)

  let fixme_null_ptr _ = ok (Ptr (Sptr.null_ptr (), None))

  let alloc_impl args =
    let zero = Usize.(0s) in
    let size, align, zeroed =
      match args with
      | [
       _alloc; Struct [ Base size; Struct [ Enum (align, []) ] ]; Base zeroed;
      ] ->
          let zeroed = Typed.cast_i U8 zeroed in
          (size, align, BV.to_bool zeroed)
      | _ ->
          Fmt.failwith "alloc_impl: invalid arguments: %a"
            Fmt.(list ~sep:(any ", ") pp_rust_val)
            args
    in
    if%sat size ==@ zero then
      ok (Ptr (Sptr.null_ptr_of (Typed.cast align), Some zero))
    else
      let* zeroed = if%sat zeroed then ok true else ok false in
      (* allocate *)
      let+ ptr = Alloc.alloc ~zeroed [ Base size; Base align ] in
      let ptr =
        match ptr with Ptr (p, _) -> p | _ -> failwith "Expected Ptr"
      in
      (* construct the Result<NonNull<[u8]>> *)
      (* FIXME: the size of this zero is probably wrong *)
      Enum (zero, [ Struct [ Ptr (ptr, Some size) ] ])
end
