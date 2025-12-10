open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

(* Functions we could not stub, but we do for performance *)
type fixme_fn = PanicCleanup | CatchUnwindCleanup

type optim_fn =
  | FloatIs of Svalue.FloatClass.t
  | FloatIsFinite
  | FloatIsSign of { positive : bool }
  | Zeroed
  | AllocImpl
  | Panic

(* Rusteria builtin functions *)
type rusteria_fn = Assert | Assume | Nondet | Panic

(* Miri builtin functions *)
type miri_fn = AllocId | PromiseAlignement | Nop

(* Functions related to the allocator, see https://doc.rust-lang.org/src/alloc/alloc.rs.html#11-36 *)
type alloc_fn =
  | Alloc of { zeroed : bool }
  | Dealloc
  | Realloc
  | NoAllocShimIsUnstable

type fn =
  | Alloc of alloc_fn
  | Fixme of fixme_fn
  | Miri of miri_fn
  | Optim of optim_fn
  | Rusteria of rusteria_fn

let std_fun_pair_list =
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
    ("std::intrinsics::miri_promise_symbolic_alignment", Miri PromiseAlignement);
    (* Obol is quite bad at parsing names so this is how they're called there... *)
    ("utils::miri_extern::miri_get_alloc_id", Miri AllocId);
    ("utils::miri_extern::miri_pointer_name", Miri Nop);
    ("utils::miri_extern::miri_print_borrow_state", Miri Nop);
    (* Allocator *)
    ("__rust_alloc", Alloc (Alloc { zeroed = false }));
    ("__rust_alloc_zeroed", Alloc (Alloc { zeroed = true }));
    ("__rust_dealloc", Alloc Dealloc);
    ("__rust_no_alloc_shim_is_unstable_v2", Alloc NoAllocShimIsUnstable);
    ("__rust_realloc", Alloc Realloc);
    (* Panic Builtins *)
    ("__rust_panic_cleanup", Fixme PanicCleanup);
    (* Core *)
    ("std::alloc::Global::alloc_impl", Optim AllocImpl);
    (* FIXME(OCaml): all float operations could be removed, but we lack bit precision when
       getting the const floats from Rust, meaning these don't really work. Either way,
       performance wise it is much preferable to override these and use SMTLib builtins. *)
    ("core::f16::_::is_finite", Optim FloatIsFinite);
    ("core::f16::_::is_infinite", Optim (FloatIs Infinite));
    ("core::f16::_::is_nan", Optim (FloatIs NaN));
    ("core::f16::_::is_normal", Optim (FloatIs Normal));
    ("core::f16::_::is_sign_negative", Optim (FloatIsSign { positive = false }));
    ("core::f16::_::is_sign_positive", Optim (FloatIsSign { positive = true }));
    ("core::f16::_::is_subnormal", Optim (FloatIs Subnormal));
    ("core::f32::_::is_finite", Optim FloatIsFinite);
    ("core::f32::_::is_infinite", Optim (FloatIs Infinite));
    ("core::f32::_::is_nan", Optim (FloatIs NaN));
    ("core::f32::_::is_normal", Optim (FloatIs Normal));
    ("core::f32::_::is_sign_negative", Optim (FloatIsSign { positive = false }));
    ("core::f32::_::is_sign_positive", Optim (FloatIsSign { positive = true }));
    ("core::f32::_::is_subnormal", Optim (FloatIs Subnormal));
    ("core::f64::_::is_finite", Optim FloatIsFinite);
    ("core::f64::_::is_infinite", Optim (FloatIs Infinite));
    ("core::f64::_::is_nan", Optim (FloatIs NaN));
    ("core::f64::_::is_normal", Optim (FloatIs Normal));
    ("core::f64::_::is_sign_negative", Optim (FloatIsSign { positive = false }));
    ("core::f64::_::is_sign_positive", Optim (FloatIsSign { positive = true }));
    ("core::f64::_::is_subnormal", Optim (FloatIs Subnormal));
    ("core::f128::_::is_finite", Optim FloatIsFinite);
    ("core::f128::_::is_infinite", Optim (FloatIs Infinite));
    ("core::f128::_::is_nan", Optim (FloatIs NaN));
    ("core::f128::_::is_normal", Optim (FloatIs Normal));
    ("core::f128::_::is_sign_negative", Optim (FloatIsSign { positive = false }));
    ("core::f128::_::is_sign_positive", Optim (FloatIsSign { positive = true }));
    ("core::f128::_::is_subnormal", Optim (FloatIs Subnormal));
    (* Compiling these would import a lot of formatting code, so we
       override them *)
    ("alloc::raw_vec::handle_error", Optim Panic);
    ("core::panicking::panic", Optim Panic);
    ("core::panicking::panic_fmt", Optim Panic);
    ("core::panicking::panic_nounwind_fmt", Optim Panic);
    ("core::slice::index::slice_start_index_len_fail", Optim Panic);
    ("core::slice::index::slice_end_index_len_fail", Optim Panic);
    ("core::slice::index::slice_end_index_overflow_fail", Optim Panic);
    ("core::slice::index::slice_index_order_fail", Optim Panic);
    ("std::alloc::handle_alloc_error", Optim Panic);
    ("std::char::encode_utf8_raw::do_panic", Optim Panic);
    ("std::option::unwrap_failed", Optim Panic);
    ("std::result::unwrap_failed", Optim Panic);
    ("std::rt::panic_fmt", Optim Panic);
    ("std::vec::Vec::_::remove::assert_failed", Optim Panic);
    (* This uses async stuff we would like to ignore, for now we patch it *)
    ("std::panicking::catch_unwind::cleanup", Fixme CatchUnwindCleanup);
  ]

let opaque_names = List.map fst std_fun_pair_list

let std_fun_map =
  std_fun_pair_list
  |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
  |> NameMatcherMap.of_list

module M (State_monad : State_monad.S) = struct
  module Alloc = Alloc.M (State_monad)
  module Intrinsics = Intrinsics.M (State_monad)
  module Miri = Miri.M (State_monad)
  module Rusteria = Rusteria.M (State_monad)
  module Std = Std.M (State_monad)

  let std_fun_eval (f : UllbcAst.fun_decl) fun_exec =
    let open Std in
    let open Alloc in
    (* Rust allows defining functions and marking them as intrinsics within a module,
       and the compiler will treat them as the intrinsic of the same name; e.g.
       mod Foo {
        #[rustc_intrinsic]
        unsafe fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
       }
       This means their path doesn't match the one we expect for the patterns; so instead of
       matching on a path, we only consider intrinsics from their name. *)
    if Charon_util.decl_has_attr f "rustc_intrinsic" then
      let name, mono =
        match List.rev f.item_meta.name with
        | PeIdent (name, _) :: _ -> (name, TypesUtils.empty_generic_args)
        | PeInstantiated mono :: PeIdent (name, _) :: _ ->
            (name, mono.binder_value)
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
         | Rusteria Assert -> Rusteria.assert_
         | Rusteria Assume -> Rusteria.assume
         | Rusteria Nondet -> Rusteria.nondet f.signature
         | Rusteria Panic -> Rusteria.panic ?msg:None
         | Miri AllocId -> Miri.alloc_id
         | Miri PromiseAlignement -> Miri.promise_alignement
         | Miri Nop -> nop
         | Optim AllocImpl -> alloc_impl
         | Optim Panic ->
             Rusteria.panic ~msg:(Fmt.to_to_string Crate.pp_name name)
         | Optim (FloatIs fc) -> float_is fc
         | Optim FloatIsFinite -> float_is_finite
         | Optim (FloatIsSign { positive }) -> float_is_sign positive
         | Optim Zeroed -> zeroed f.signature
         | Alloc (Alloc { zeroed }) -> alloc ~zeroed
         | Alloc Dealloc -> dealloc
         | Alloc NoAllocShimIsUnstable -> no_alloc_shim_is_unstable
         | Alloc Realloc -> realloc
         | Fixme PanicCleanup -> fixme_panic_cleanup
         | Fixme CatchUnwindCleanup -> fixme_catch_unwind_cleanup

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
