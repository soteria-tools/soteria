open Syntaxes.FunctionWrap
open Common
open Svalue
open Typed.Infix
open Typed.Syntax

module M (StateM : State.StateM.S) : Intf.M(StateM).S = struct
  open StateM
  open Syntax

  let drop_in_place ~t:_ ~to_drop:_ = ok ()

  (* Emulate catch_unwind cleanup: build a &dyn Any trait object so the caller
     can inspect the panic payload. See
     https://doc.rust-lang.org/src/std/panicking.rs.html#557-565 *)
  let cleanup ~payload =
    let ptr = Typed.Ptr.ptr_of payload in
    let* usize_size = Layout.size_of (TLiteral (TUInt Usize)) in
    let@ () = with_alloc_kind ~kind:(VTable Charon.TypesUtils.mk_unit_ty) in
    let* vtable =
      State.alloc_untyped ~zeroed:true
        ~size:Usize.(usize_size *!!@ 3s)
        ~align:(Typed.cast_nonzero usize_size)
        ()
    in
    let* drop_fn = State.declare_fn (Synthetic GenericDropInPlace) in
    let* () = State.store vtable Charon_util.unit_ptr drop_fn in
    let vtable = Typed.Ptr.ptr_of vtable in
    let* align_ptr =
      Sptr.offset ~ty:(TLiteral (TUInt Usize)) ~check_signed:true
        Usize.(2s)
        vtable
    in
    let align_ptr = Typed.Ptr.mk_ptr_f align_ptr None in
    let+ () = State.store align_ptr Charon_util.unit_ptr Usize.(1s) in
    Value_codec.mk_box
      (Typed.Ptr.mk_ptr_f ptr (Some vtable))
      Typed.Adt.unit Typed.Adt.unit
end
