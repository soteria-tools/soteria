open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap

module M (State : State_intf.S) = struct
  module Alloc = Alloc.M (State)
  module Intrinsics = Intrinsics.M (State)
  module Miri = Miri.M (State)
  module Rusteria = Rusteria.M (State)
  module Std = Std.M (State)

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  (* Functions that we shouldn't stub, but need to (e.g. because of Charon) *)
  type fixme_fn = BoxNew | Index | Nop | Panic | TryCleanup

  (* Functions we could not stub, but we do for performance *)
  and optim_fn =
    | FloatIs of Svalue.FloatClass.t
    | FloatIsFinite
    | FloatIsSign of { positive : bool }
    | Zeroed
    | AllocImpl

  (* Rusteria builtin functions *)
  and rusteria_fn = Assert | Assume | Nondet | Panic

  (* Miri builtin functions *)
  and miri_fn = AllocId | Nop

  (* Functions related to the allocator, see https://doc.rust-lang.org/src/alloc/alloc.rs.html#11-36 *)
  and alloc_fn =
    | Alloc of { zeroed : bool }
    | Dealloc
    | Realloc
    | NoAllocShimIsUnstable

  and fn =
    | Rusteria of rusteria_fn
    | Miri of miri_fn
    | Alloc of alloc_fn
    | Fixme of fixme_fn
    | Optim of optim_fn
  [@@deriving show { with_path = false }]

  let std_fun_map =
    [
      (* Rusteria builtins *)
      ("rusteria::assert", Rusteria Assert);
      ("rusteria::assume", Rusteria Assume);
      ("rusteria::nondet", Rusteria Nondet);
      ("rusteria::panic", Rusteria Panic);
      (* Kani builtins -- we re-define these for nicer call traces *)
      ("kani::assert", Rusteria Assert);
      ("kani::assume", Rusteria Assume);
      ("kani::panic", Rusteria Panic);
      (* Miri builtins *)
      ("miristd::miri_get_alloc_id", Miri AllocId);
      ("miristd::miri_pointer_name", Miri Nop);
      ("miristd::miri_print_borrow_state", Miri Nop);
      (* Obol is quite bad at parsing names so this is how they're called there... *)
      ("utils::miri_extern::miri_get_alloc_id", Miri AllocId);
      ("utils::miri_extern::miri_pointer_name", Miri Nop);
      ("utils::miri_extern::miri_print_borrow_state", Miri Nop);
      (* Core *)
      (* This fails because of a silly thing with NonZero in monomorphisation, which we won't
         fix for now as it requires monomorphising trait impls.  *)
      ("alloc::boxed::{@T}::new", Fixme BoxNew);
      ("std::alloc::Global::alloc_impl", Optim AllocImpl);
      (* FIXME: the below indexes fail because the code doesn't get monomorphised properly, and
         returns a thin pointer rather than a fat one. *)
      ("core::array::{core::ops::index::Index}::index", Fixme Index);
      ("core::array::{core::ops::index::IndexMut}::index_mut", Fixme Index);
      ("core::slice::index::{core::ops::index::Index}::index", Fixme Index);
      ( "core::slice::index::{core::ops::index::IndexMut}::index_mut",
        Fixme Index );
      ("core::cell::panic_already_mutably_borrowed", Fixme Panic);
      ("alloc::alloc::handle_alloc_error::ct_error", Fixme Panic);
      (* hax fails to construct a null pointer from a constant *)
      ("core::mem::zeroed", Optim Zeroed);
      (* all float operations could be removed, but we lack bit precision when getting the
         const floats from Rust, meaning these don't really work. Either way, performance wise
         it is much preferable to override these and use SMTLib builtins. *)
      ("core::f16::_::is_finite", Optim FloatIsFinite);
      ("core::f16::_::is_infinite", Optim (FloatIs Infinite));
      ("core::f16::_::is_nan", Optim (FloatIs NaN));
      ("core::f16::_::is_normal", Optim (FloatIs Normal));
      ( "core::f16::_::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ("core::f16::_::is_sign_positive", Optim (FloatIsSign { positive = true }));
      ("core::f16::_::is_subnormal", Optim (FloatIs Subnormal));
      ("core::f32::_::is_finite", Optim FloatIsFinite);
      ("core::f32::_::is_infinite", Optim (FloatIs Infinite));
      ("core::f32::_::is_nan", Optim (FloatIs NaN));
      ("core::f32::_::is_normal", Optim (FloatIs Normal));
      ( "core::f32::_::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ("core::f32::_::is_sign_positive", Optim (FloatIsSign { positive = true }));
      ("core::f32::_::is_subnormal", Optim (FloatIs Subnormal));
      ("core::f64::_::is_finite", Optim FloatIsFinite);
      ("core::f64::_::is_infinite", Optim (FloatIs Infinite));
      ("core::f64::_::is_nan", Optim (FloatIs NaN));
      ("core::f64::_::is_normal", Optim (FloatIs Normal));
      ( "core::f64::_::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ("core::f64::_::is_sign_positive", Optim (FloatIsSign { positive = true }));
      ("core::f64::_::is_subnormal", Optim (FloatIs Subnormal));
      ("core::f128::_::is_finite", Optim FloatIsFinite);
      ("core::f128::_::is_infinite", Optim (FloatIs Infinite));
      ("core::f128::_::is_nan", Optim (FloatIs NaN));
      ("core::f128::_::is_normal", Optim (FloatIs Normal));
      ( "core::f128::_::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ( "core::f128::_::is_sign_positive",
        Optim (FloatIsSign { positive = true }) );
      ("core::f128::_::is_subnormal", Optim (FloatIs Subnormal));
      (* These don't compile, because we don't link a sysroot like Miri with -Zemit-mir *)
      ("alloc::raw_vec::handle_error", Fixme Panic);
      ("core::panicking::panic", Fixme Panic);
      ("core::panicking::panic_fmt", Fixme Panic);
      ("core::slice::index::slice_end_index_len_fail", Fixme Panic);
      ("core::slice::index::slice_end_index_overflow_fail", Fixme Panic);
      ("core::slice::index::slice_index_order_fail", Fixme Panic);
      ("std::alloc::handle_alloc_error", Fixme Panic);
      ("std::option::unwrap_failed", Fixme Panic);
      ("std::result::unwrap_failed", Fixme Panic);
      ("std::vec::Vec::_::remove::assert_failed", Fixme Panic);
      (* These don't compile, for some reason? *)
      ("std::panicking::try::cleanup", Fixme TryCleanup);
      ("std::panicking::catch_unwind::cleanup", Fixme TryCleanup);
      (* Allocator *)
      ("__rust_alloc", Alloc (Alloc { zeroed = false }));
      ("__rust_alloc_zeroed", Alloc (Alloc { zeroed = true }));
      ("__rust_dealloc", Alloc Dealloc);
      ("__rust_no_alloc_shim_is_unstable_v2", Alloc NoAllocShimIsUnstable);
      ("__rust_realloc", Alloc Realloc);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let std_fun_eval (f : UllbcAst.fun_decl) fun_exec =
    let open Std in
    let open Alloc in
    let open Rusteria in
    let open Miri in
    let is_intrinsic = Charon_util.decl_has_attr f "rustc_intrinsic" in
    (* Rust allows defining functions and marking them as intrinsics within a module,
       and the compiler will treat them as the intrinsic of the same name; e.g.
       mod Foo {
        #[rustc_intrinsic]
        unsafe fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
       }
       This means their path doesn't match the one we expect for the patterns; so instead of
       matching on a path, we only consider intrinsics from their name. *)
    if is_intrinsic then
      let name, mono =
        match List.rev f.item_meta.name with
        | PeIdent (name, _) :: _ -> (name, TypesUtils.empty_generic_args)
        | PeMonomorphized mono :: PeIdent (name, _) :: _ -> (name, mono)
        | _ -> failwith "Unexpected intrinsic shape"
      in
      let fn = Intrinsics.eval_fun name fun_exec mono in
      Some fn
    else
      let name =
        match List.last f.item_meta.name with
        | PeIdent (name, _) as ident
          when String.starts_with ~prefix:"__rust" name ->
            [ ident ]
        | _ -> f.item_meta.name
      in
      let ctx = Crate.as_namematcher_ctx () in
      NameMatcherMap.find_opt ctx match_config name std_fun_map
      |> Option.map @@ function
         | Rusteria Assert -> assert_
         | Rusteria Assume -> assume
         | Rusteria Nondet -> nondet f.signature
         | Rusteria Panic -> panic ?msg:None
         | Miri AllocId -> alloc_id
         | Miri Nop -> nop
         | Fixme BoxNew -> fixme_box_new f.signature
         | Fixme Index -> array_index_fn f.signature
         | Fixme Panic -> panic ~msg:(Fmt.to_to_string Crate.pp_name name)
         | Fixme Nop -> nop
         | Fixme TryCleanup -> fixme_try_cleanup
         | Optim AllocImpl -> alloc_impl
         | Optim (FloatIs fc) -> float_is fc
         | Optim FloatIsFinite -> float_is_finite
         | Optim (FloatIsSign { positive }) -> float_is_sign positive
         | Optim Zeroed -> zeroed f.signature
         | Alloc (Alloc { zeroed }) -> alloc ~zeroed
         | Alloc Dealloc -> dealloc
         | Alloc NoAllocShimIsUnstable -> no_alloc_shim_is_unstable
         | Alloc Realloc -> realloc

  let builtin_fun_eval (f : Types.builtin_fun_id) generics =
    let open Std in
    match f with
    | ArrayRepeat -> array_repeat generics
    | ArrayToSliceMut -> array_slice ~mut:true generics
    | ArrayToSliceShared -> array_slice ~mut:false generics
    | Index idx -> array_index idx generics
    | BoxNew -> box_new generics
    | PtrFromParts _ -> from_raw_parts
end
