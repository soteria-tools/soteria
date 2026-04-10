(** Builtins, for functions we stub manually, either because the originals
    suffer from poor performance, or are not properly computable in our engine.
*)

open Typed.Syntax
open Typed.Infix
open Rust_val
open Common

module M (StateM : State.StateM.S) = struct
  module Core = Core.M (StateM)
  module Alloc = Alloc.M (StateM)
  open StateM
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
    (* FIXME: the size of this zero is probably wrong *)
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
      mk_res ptr size

  let fixme_panic_cleanup _ =
    (* TODO: whas is __rust_panic_cleanup meant to do and return? *)
    ok (Ptr (Sptr.null_ptr (), VTable (Sptr.null_ptr ())))

  let fixme_catch_unwind_cleanup args =
    (* We need to make a [&dyn Any] to emulate a trait object, how nightmareish.
       https://doc.rust-lang.org/src/std/panicking.rs.html#557-565 *)
    let ptr =
      match args with
      | [ Ptr (p, _) ] -> p
      | _ -> failwith "fixme_catch_unwind_cleanup: invalid arguments"
    in
    let* usize_size = Layout.size_of (TLiteral (TUInt Usize)) in
    let* vtable, _ =
      State.alloc_untyped ~kind:(VTable Charon.TypesUtils.mk_unit_ty)
        ~zeroed:true
        ~size:Usize.(usize_size *!!@ 3s)
        ~align:(Typed.cast usize_size) ()
    in
    (* We say the drop function is a drop in place to anything, which is
       implemented as a no-op. *)
    let* drop_fn = State.declare_fn (Synthetic GenericDropInPlace) in
    let* () = State.store (vtable, Thin) Charon_util.unit_ptr (Ptr drop_fn) in
    (* We also need the alignment to be 1 *)
    let* align_ptr =
      Sptr.offset ~ty:(TLiteral (TUInt Usize)) ~signed:false vtable Usize.(2s)
    in
    let+ () =
      State.store (align_ptr, Thin) Charon_util.unit_ptr (Int Usize.(1s))
    in
    _mk_box (Ptr (ptr, VTable vtable))

  let to_buffer_if_capture_used _ = ok (Int (Typed.BV.of_bool Typed.v_true))
end
