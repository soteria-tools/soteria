open Typed.Syntax
open Typed.Infix
open Rust_val

module M (Rust_state_m : State.State_M) = struct
  module Core = Core.M (Rust_state_m)
  module Alloc = Alloc.M (Rust_state_m)
  open Rust_state_m
  open Syntax

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
    ok (Int (Typed.BV.of_bool res))

  let float_is_finite args =
    let v =
      match args with
      | [ Float f ] -> f
      | _ -> failwith "float_is_finite: invalid argument"
    in
    let v = Typed.cast_float v in
    let res = Typed.((not (Float.is_nan v)) &&@ not (Float.is_infinite v)) in
    ok (Int (Typed.BV.of_bool res))

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
    ok (Int (Typed.BV.of_bool res))

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
          (size, align, Typed.BV.to_bool zeroed)
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
