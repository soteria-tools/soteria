(** This file was generated with [scripts/intrinsics.py] -- do not edit it
    manually, instead modify the script and re-run it. *)

[@@@warning "-unused-open"]

open Charon
open Common
open Rust_val
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

module M (StateM : State.StateM.S) = struct
  open StateM
  open Syntax

  type rust_val = Sptr.t Rust_val.t
  type 'a ret = ('a, unit) StateM.t
  type fun_exec = Fun_kind.t -> rust_val list -> (rust_val, unit) StateM.t
  type full_ptr = StateM.Sptr.t Rust_val.full_ptr

  let[@inline] as_ptr (v : rust_val) =
    match v with
    | Ptr ptr -> ptr
    | Int v ->
        let v = Typed.cast_i Usize v in
        let ptr = Sptr.null_ptr_of v in
        (ptr, Thin)
    | _ -> failwith "expected pointer"

  let as_base ty (v : rust_val) = Rust_val.as_base ty v
  let as_base_i ty (v : rust_val) = Rust_val.as_base_i ty v
  let as_base_f ty (v : rust_val) = Rust_val.as_base_f ty v

  module type Intf = sig
    (** {@markdown[
          Executes the destructor (if any) of the pointed-to value.

           This is almost the same as calling [`ptr::read`] and discarding
           the result, but has the following advantages:

           * It is *required* to use `drop_in_place` to drop unsized types like
             trait objects, because they can't be read out onto the stack and
             dropped normally.

           * It is friendlier to the optimizer to do this over [`ptr::read`] when
             dropping manually allocated memory (e.g., in the implementations of
             `Box`/`Rc`/`Vec`), as the compiler doesn't need to prove that it's
             sound to elide the copy.

           * It can be used to drop [pinned] data when `T` is not `repr(packed)`
             (pinned data must not be moved before it is dropped).

           Unaligned values cannot be dropped in place, they must be copied to an aligned
           location first using [`ptr::read_unaligned`]. For packed structs, this move is
           done automatically by the compiler. This means the fields of packed structs
           are not dropped in-place.

           [`ptr::read`]: self::read
           [`ptr::read_unaligned`]: self::read_unaligned
           [pinned]: crate::pin

           # Safety

           Behavior is undefined if any of the following conditions are violated:

           * `to_drop` must be [valid] for both reads and writes.

           * `to_drop` must be properly aligned, even if `T` has size 0.

           * `to_drop` must be nonnull, even if `T` has size 0.

           * The value `to_drop` points to must be valid for dropping, which may mean
             it must uphold additional invariants. These invariants depend on the type
             of the value being dropped. For instance, when dropping a Box, the box's
             pointer to the heap must be valid.

           * While `drop_in_place` is executing, the only way to access parts of
             `to_drop` is through the `&mut self` references supplied to the
             `Drop::drop` methods that `drop_in_place` invokes.

           Additionally, if `T` is not [`Copy`], using the pointed-to value after
           calling `drop_in_place` can cause undefined behavior. Note that `*to_drop =
           foo` counts as a use because it will cause the value to be dropped
           again. [`write()`] can be used to overwrite data without causing it to be
           dropped.

           [valid]: self#safety

           # Examples

           Manually remove the last item from a vector:

           ```
           use std::ptr;
           use std::rc::Rc;

           let last = Rc::new(1);
           let weak = Rc::downgrade(&last);

           let mut v = vec![Rc::new(0), last];

           unsafe {
               // Get a raw pointer to the last element in `v`.
               let ptr = &mut v[1] as *mut _;
               // Shorten `v` to prevent the last item from being dropped. We do that first,
               // to prevent issues if the `drop_in_place` below panics.
               v.set_len(1);
               // Without a call `drop_in_place`, the last item would never be dropped,
               // and the memory it manages would be leaked.
               ptr::drop_in_place(ptr);
           }

           assert_eq!(v, &[0.into()]);

           // Ensure that the last item was dropped.
           assert!(weak.upgrade().is_none());
           ```
        ]} *)
    val drop_in_place : t:Types.ty -> to_drop:full_ptr -> unit ret

    val cleanup :
      fun_exec:fun_exec ->
      types:Types.ty list ->
      consts:Types.constant_expr list ->
      args:rust_val list ->
      rust_val ret
  end

  type fn = CorePtrDropInPlace | StdPanickingCatchUnwindCleanup

  let fn_map : fn NameMatcherMap.t =
    [
      ("core::ptr::drop_in_place", CorePtrDropInPlace);
      ("std::panicking::catch_unwind::cleanup", StdPanickingCatchUnwindCleanup);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  (* BEGIN USER IMPLEMENTATION *)
  module Impl : Intf = struct
    open Typed.Infix
    open Typed.Syntax

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
  (* END USER IMPLEMENTATION *)

  let fn_of_stub stub _fun_exec (generics : Charon.Types.generic_args) args =
    match (stub, generics.types, generics.const_generics, args) with
    | CorePtrDropInPlace, [ t ], [], [ to_drop ] ->
        let to_drop = as_ptr to_drop in
        let+ () = Impl.drop_in_place ~t ~to_drop in
        Tuple []
    | StdPanickingCatchUnwindCleanup, _, _, _ ->
        Impl.cleanup ~fun_exec:_fun_exec ~types:generics.types
          ~consts:generics.const_generics ~args
    | _, tys, cs, args ->
        Fmt.kstr not_impl
          "Custom stub found but called with the wrong arguments; got:@.Types: \
           %a@.Consts: %a@.Args: %a"
          Fmt.(list ~sep:comma Charon_util.pp_ty)
          tys
          Fmt.(list ~sep:comma Crate.pp_constant_expr)
          cs
          Fmt.(list ~sep:comma pp_rust_val)
          args

  let eval_fun (f : UllbcAst.fun_decl) (fun_exec : fun_exec)
      (generics : Charon.Types.generic_args) =
    let ctx = Crate.as_namematcher_ctx () in
    let stub =
      NameMatcherMap.find_opt ctx match_config f.item_meta.name fn_map
    in
    Option.map (fun stub -> fn_of_stub stub fun_exec generics) stub
end
