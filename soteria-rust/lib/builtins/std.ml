open Charon
module BV = Typed.BitVec
open Typed.Syntax
open Typed.Infix
open Rust_val

module M (Rust_state_m : Rust_state_m.S) = struct
  module Core = Core.M (Rust_state_m)
  module Alloc = Alloc.M (Rust_state_m)
  open Rust_state_m
  open Syntax

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
    ok (Tuple (List.init size (fun _ -> rust_val)))

  let array_index (idx_op : Types.builtin_index_op)
      (gen_args : Types.generic_args) args =
    let ptr, size =
      match (idx_op.is_array, List.hd args, gen_args.const_generics) with
      (* Array with static size *)
      | true, Ptr (ptr, Thin), [ size ] -> (ptr, BV.usize_of_const_generic size)
      | false, Ptr (ptr, Len size), [] -> (ptr, Typed.cast_i Usize size)
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
    let* ptr' = Sptr.offset ~signed:false ~ty ptr idx in
    if not idx_op.is_range then
      let+ () =
        State.assert_ (Usize.(0s) <=$@ idx &&@ (idx <$@ size)) `OutOfBounds
      in
      Ptr (ptr', Thin)
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
      Ptr (ptr', Len size)

  let array_slice ~mut:_ (gen_args : Types.generic_args) args =
    match (gen_args.const_generics, args) with
    | [ size ], [ Ptr (ptr, Thin) ] ->
        let size = BV.usize_of_const_generic size in
        ok (Ptr (ptr, Len size))
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
    let ptr, meta =
      match args with
      | [ ptr; meta ] -> (ptr, meta)
      | _ -> failwith "from_raw_parts: invalid arguments"
    in
    let ptr =
      match ptr with
      | Ptr (ptr, Thin) -> ptr
      | Int v -> Sptr.null_ptr_of @@ Typed.cast_i Usize v
      | _ ->
          failwith "from_raw_parts: first argument must be a pointer or usize"
    in
    let meta =
      match meta with
      | Tuple [] -> Thin
      | Int v -> Len (Typed.cast_i Usize v)
      | Ptr (ptr, Thin) -> VTable ptr
      | _ ->
          failwith
            "from_raw_parts: second argument must be unit, a pointer or usize"
    in
    ok (Ptr (ptr, meta))

  let nop _ = ok (Tuple [])

  let float_is (fp : Svalue.FloatClass.t) args =
    let v =
      match args with
      | [ Float f ] -> f
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
    ok (Int (BV.of_bool res))

  let float_is_finite args =
    let v =
      match args with
      | [ Float f ] -> f
      | _ -> failwith "float_is_finite: invalid argument"
    in
    let v = Typed.cast_float v in
    let res = Typed.((not (Float.is_nan v)) &&@ not (Float.is_infinite v)) in
    ok (Int (BV.of_bool res))

  let float_is_sign pos args =
    let v =
      match args with
      | [ Float f ] -> f
      | _ -> failwith "float_is_sign: invalid argument"
    in
    let v = Typed.cast_float v in
    let res =
      if pos then Typed.Float.(leq (like v 0.) v)
      else Typed.Float.(leq v (like v (-0.)))
    in
    let res = res ||@ Typed.Float.is_nan v in
    ok (Int (BV.of_bool res))

  let _mk_box ptr =
    let non_null = Tuple [ ptr ] in
    let phantom_data = Tuple [] in
    let unique = Tuple [ non_null; phantom_data ] in
    let allocator = Tuple [] in
    Tuple [ unique; allocator ]

  let alloc_impl args =
    let zero = Usize.(0s) in
    let size, align, zeroed =
      match args with
      | [ _alloc; Tuple [ Int size; Tuple [ Enum (align, []) ] ]; Int zeroed ]
        ->
          let size = Typed.cast_i Usize size in
          let align = Typed.cast_i Usize align in
          let zeroed = Typed.cast_i U8 zeroed in
          (size, align, BV.to_bool zeroed)
      | _ ->
          Fmt.failwith "alloc_impl: invalid arguments: %a"
            Fmt.(list ~sep:(any ", ") pp_rust_val)
            args
    in
    (* make Result<NonNull<[u8]>, AllocError> *)
    let mk_res ptr len = Enum (zero, [ Tuple [ Ptr (ptr, Len len) ] ]) in
    if%sat size ==@ zero then
      let dangling = Sptr.null_ptr_of align in
      ok (mk_res dangling zero)
    else
      let* zeroed = if%sat zeroed then ok true else ok false in
      (* allocate *)
      let+ ptr = Alloc.alloc ~zeroed [ Int size; Int align ] in
      let ptr =
        match ptr with Ptr (p, _) -> p | _ -> failwith "Expected Ptr"
      in
      (* FIXME: the size of this zero is probably wrong *)
      mk_res ptr size

  let fixme_panic_cleanup _ =
    (* TODO: whas is __rust_panic_cleanup meant to do and return? *)
    ok (Ptr (Sptr.null_ptr (), VTable (Sptr.null_ptr ())))

  let fixme_catch_unwind_cleanup _ =
    (* We return a null dyn box, like above *)
    ok @@ _mk_box (Ptr (Sptr.null_ptr (), VTable (Sptr.null_ptr ())))
end
