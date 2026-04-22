open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
module SMap = Map.Make (String)

let match_config =
  NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

(* Functions we stub to avoid problems in the interpreter *)
type fixme_fn = CatchUnwindCleanup | PromiseAlignment | DropInPlace

(* Functions we could not stub, but we do for performance *)
type optim_fn =
  | FloatIs of Svalue.FloatClass.t
  | FloatIsFinite
  | FloatIsSign of { positive : bool }
  | AllocImpl
  | Nop
  | Panic
  | PrintToBufferIfCaptureUsed

(* Soteria builtin functions *)
type soteria_fn = Assert | Assume | NondetBytes | Panic

(* Miri builtin functions *)
type miri_fn = Alloc | AllocId | Dealloc | Nop

(* Functions related to panics *)
type panic_fn = PanicCleanup

(* Functions related to the allocator, see
   https://doc.rust-lang.org/src/alloc/alloc.rs.html#11-36 *)
type alloc_fn =
  | Alloc of { zeroed : bool }
  | Dealloc
  | Realloc
  | NoAllocShimIsUnstable

type system_fn = HashmapRandomKeys | TlvAtexit

type fn =
  | Fixme of fixme_fn
  | Optim of optim_fn
  | Soteria of soteria_fn
  | System of system_fn

type extern_fn = Alloc of alloc_fn | Miri of miri_fn | Panic of panic_fn

let extern_functions =
  [
    (* Allocator *)
    ("__rust_alloc", Alloc (Alloc { zeroed = false }));
    ("__rust_alloc_zeroed", Alloc (Alloc { zeroed = true }));
    ("__rust_dealloc", Alloc Dealloc);
    ("__rust_no_alloc_shim_is_unstable_v2", Alloc NoAllocShimIsUnstable);
    ("__rust_realloc", Alloc Realloc);
    (* Miri builtins *)
    ("miri_get_alloc_id", Miri AllocId);
    ("miri_pointer_name", Miri Nop);
    ("miri_print_borrow_state", Miri Nop);
    ("miri_run_provenance_gc", Miri Nop);
    ("miri_write_to_stdout", Miri Nop);
    ("miri_alloc", Miri Alloc);
    ("miri_dealloc", Miri Dealloc);
    (* Panics *)
    ("__rust_panic_cleanup", Panic PanicCleanup);
  ]
  |> SMap.of_list

let std_fun_pair_list =
  [
    (* Soteria builtins *)
    ("soteria::assert", Soteria Assert);
    ("soteria::assume", Soteria Assume);
    ("soteria::nondet_bytes", Soteria NondetBytes);
    ("soteria::panic", Soteria Panic);
    (* Kani builtins -- we re-define these for nicer call traces *)
    ("kani::assert", Soteria Assert);
    ("kani::assume", Soteria Assume);
    ("kani::panic", Soteria Panic);
    (* Miri builtins *)
    (* HACK: this should be handled with intrinsics. *)
    ("std::intrinsics::miri_promise_symbolic_alignment", Fixme PromiseAlignment);
    (* System stuff *)
    (* TODO: the name of the function is *just* _tlv_atexit, because it's an
       external function, but we don't yet have a way to detect that. This
       is the same issue we have for allocator calls, for which our solution
       is unsatisfactory (checking if the name starts with "__rust"). *)
    ( "std::sys::thread_local::guard::apple::enable::_tlv_atexit",
      System TlvAtexit );
    ("std::sys::random::hashmap_random_keys", System HashmapRandomKeys);
    (* Dropping, in particular for the generic case, does nothing. *)
    ("core::ptr::drop_in_place", Fixme DropInPlace);
    (* Core *)
    ("std::alloc::Global::alloc_impl", Optim AllocImpl);
    (* FIXME(OCaml): all float operations could be removed, but we lack bit
       precision when getting the const floats from Rust, meaning these don't
       really work. Either way, performance wise it is much preferable to
       override these and use SMTLib builtins. *)
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
    (* Compiling these would import a lot of formatting code, so we override
       them *)
    ("std::io::_eprint", Optim Nop);
    ("std::io::_print", Optim Nop);
    ("std::io::stdio::print_to", Optim Nop);
    ( "std::io::stdio::print_to_buffer_if_capture_used",
      Optim PrintToBufferIfCaptureUsed );
    ("alloc::raw_vec::handle_error", Optim Panic);
    ("core::panicking::panic", Optim Panic);
    ("core::panicking::panic_fmt", Optim Panic);
    ("core::panicking::panic_nounwind_fmt", Optim Panic);
    ("core::panicking::assert_failed_inner", Optim Panic);
    ("core::slice::index::slice_start_index_len_fail", Optim Panic);
    ("core::slice::index::slice_end_index_len_fail", Optim Panic);
    ("core::slice::index::slice_end_index_overflow_fail", Optim Panic);
    ("core::slice::index::slice_index_order_fail", Optim Panic);
    ("std::alloc::handle_alloc_error", Optim Panic);
    ("std::char::encode_utf8_raw::do_panic", Optim Panic);
    ("std::option::unwrap_failed", Optim Panic);
    ("std::result::unwrap_failed", Optim Panic);
    ("std::rt::panic_fmt", Optim Panic);
    ("std::rt::begin_panic", Optim Panic);
    ("std::vec::Vec::_::remove::assert_failed", Optim Panic);
    (* This uses async stuff we would like to ignore, for now we patch it *)
    ("std::panicking::catch_unwind::cleanup", Fixme CatchUnwindCleanup);
  ]

let opaque_names = List.map fst std_fun_pair_list

let std_fun_map =
  std_fun_pair_list
  |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
  |> NameMatcherMap.of_list

module M (StateM : State.StateM.S) = struct
  module Alloc = Alloc.M (StateM)
  module Intrinsics = Intrinsics.M (StateM)
  module Miri = Miri.M (StateM)
  module Soteria_lib = Soteria_lib.M (StateM)
  module Std = Std.M (StateM)
  module System = System.M (StateM)

  let fn_to_stub fn_sig fn_name fun_exec = function
    | Soteria Assert -> Soteria_lib.assert_
    | Soteria Assume -> Soteria_lib.assume
    | Soteria NondetBytes -> Soteria_lib.nondet_bytes fn_sig
    | Soteria Panic -> Soteria_lib.panic ?msg:None
    | Optim AllocImpl -> Std.alloc_impl
    | Optim Panic ->
        Soteria_lib.panic ~msg:(Fmt.to_to_string Crate.pp_name fn_name)
    | Optim (FloatIs fc) -> Std.float_is fc
    | Optim FloatIsFinite -> Std.float_is_finite
    | Optim (FloatIsSign { positive }) -> Std.float_is_sign positive
    | Optim Nop -> Std.nop
    | Optim PrintToBufferIfCaptureUsed -> Std.to_buffer_if_capture_used
    | Fixme CatchUnwindCleanup -> Std.fixme_catch_unwind_cleanup
    | Fixme DropInPlace -> Std.nop
    | Fixme PromiseAlignment -> Miri.promise_alignement
    | System HashmapRandomKeys -> System.hashmap_random_keys
    | System TlvAtexit -> System.tlv_atexit fun_exec

  let extern_fn_to_stub = function
    | Alloc (Alloc { zeroed }) -> Alloc.alloc ~zeroed
    | Alloc Dealloc -> Alloc.dealloc
    | Alloc NoAllocShimIsUnstable -> Alloc.no_alloc_shim_is_unstable
    | Alloc Realloc -> Alloc.realloc
    | Miri Alloc -> Miri.alloc
    | Miri AllocId -> Miri.alloc_id
    | Miri Dealloc -> Miri.dealloc
    | Miri Nop -> Std.nop
    | Panic PanicCleanup -> Std.panic_cleanup

  let eval_stub (f : UllbcAst.fun_decl) fun_exec =
    let name = f.item_meta.name in
    let ctx = Crate.as_namematcher_ctx () in
    let stub = NameMatcherMap.find_opt ctx match_config name std_fun_map in
    match stub with
    | Some stub ->
        let stub args = fn_to_stub f.signature name fun_exec stub args in
        Some stub
    | None -> None

  let eval_intrinsic (f : UllbcAst.fun_decl) name generics fun_exec =
    (* In the case of monomorphised code, the generics will be empty but present
       in the name; we need to get them there. *)
    let generics =
      match List.last_opt f.item_meta.name with
      | Some (PeInstantiated mono) -> mono.binder_value
      | _ -> generics
    in
    Intrinsics.eval_fun name fun_exec generics

  let eval_extern name =
    match SMap.find_opt name extern_functions with
    | Some extern_fn -> extern_fn_to_stub extern_fn
    | None ->
        fun _args ->
          Fmt.kstr StateM.not_impl "Extern function %s is not handled" name
end
