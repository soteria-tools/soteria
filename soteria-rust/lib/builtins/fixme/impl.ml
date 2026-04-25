open Common
open Rust_val
open Typed.Infix
open Typed.Syntax

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

  let drop_in_place ~t:_ ~to_drop:_ = ok ()

  let mk_box ptr =
    let non_null = Tuple [ ptr ] in
    let phantom_data = Tuple [] in
    let unique = Tuple [ non_null; phantom_data ] in
    let allocator = Tuple [] in
    Tuple [ unique; allocator ]

  (* Emulate catch_unwind cleanup: build a &dyn Any trait object so the caller
     can inspect the panic payload. See
     https://doc.rust-lang.org/src/std/panicking.rs.html#557-565 *)
  let cleanup ~fun_exec:_ ~types:_ ~consts:_ ~args =
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
    let* drop_fn = State.declare_fn (Synthetic GenericDropInPlace) in
    let* () = State.store (vtable, Thin) Charon_util.unit_ptr (Ptr drop_fn) in
    let* align_ptr =
      Sptr.offset ~ty:(TLiteral (TUInt Usize)) ~signed:false vtable Usize.(2s)
    in
    let+ () =
      State.store (align_ptr, Thin) Charon_util.unit_ptr (Int Usize.(1s))
    in
    mk_box (Ptr (ptr, VTable vtable))
end
