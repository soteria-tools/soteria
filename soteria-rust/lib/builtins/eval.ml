open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
open Rustsymex

module M (State : State_intf.S) = struct
  module Std = Std.M (State)
  module Alloc = Alloc.M (State)
  module Rusteria = Rusteria.M (State)
  module Miri = Miri.M (State)

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  (* Functions that we shouldn't stub, but need to (e.g. because of Charon) *)
  type fixme_fn = BoxNew | Index | Nop | Panic | TryCleanup | NullPtr

  (* Functions we could not stub, but we do for performance *)
  type optim_fn =
    | FloatIs of Svalue.FloatClass.t
    | FloatIsFinite
    | FloatIsSign of { positive : bool }
    | Zeroed

  (* Rusteria builtin functions *)
  type rusteria_fn = Assert | Assume | Nondet | Panic

  (* Miri builtin functions *)
  type miri_fn = AllocId | Nop

  (* Functions related to the allocator, see https://doc.rust-lang.org/src/alloc/alloc.rs.html#11-36 *)
  type alloc_fn =
    | Alloc of { zeroed : bool }
    | Dealloc
    | Realloc
    | NoAllocShimIsUnstable

  (* Standard library functions *)
  type std_fn =
    (* Std *)
    | Abs
    | AssertZeroValid
    | AssertInhabited
    | Assume
    | ByteSwap
    | BlackBox
    | Breakpoint
    | CatchUnwind
    | Checked of Expressions.binop
    | CompareBytes
    | Copy of { nonoverlapping : bool }
    | CopySign
    | Ctpop
    | DiscriminantValue
    | ExactDiv
    | FloatFast of Expressions.binop
    | FloatMinMax of { min : bool }
    | FloatRounding of Svalue.FloatRoundingMode.t
    | FloatToInt
    | Index
    | IsValStaticallyKnown
    | Likely
    | AlignOf of { of_val : bool }
    | MulAdd
    | Nop
    | PanicSimple
    | PtrByteOp of Expressions.binop
    | PtrGuaranteedCmp
    | PtrOp of { op : Expressions.binop; check : bool }
    | PtrOffsetFrom of { unsigned : bool }
    | RawEq
    | Saturating of Expressions.binop
    | SizeOf
    | SizeOfVal
    | Transmute
    | TypeId
    | TypeName
    | TypedSwapNonOverlapping
    | Unchecked of Expressions.binop
    | VariantCount
    | Wrapping of Expressions.binop
    | WriteBytes

  type fn =
    | Rusteria of rusteria_fn
    | Miri of miri_fn
    | Alloc of alloc_fn
    | Std of std_fn
    | Fixme of fixme_fn
    | Optim of optim_fn

  let std_fun_map =
    [
      (* Rusteria builtins *)
      ("rusteria::assert", Rusteria Assert);
      ("rusteria::assume", Rusteria Assume);
      ("rusteria::nondet", Rusteria Nondet);
      ("rusteria::panic", Rusteria Panic);
      (* Kani builtins -- we re-define these for nicer call traces *)
      ("kani::assert", Rusteria Assert);
      ("kani::panic", Rusteria Panic);
      (* Miri builtins *)
      ("miristd::miri_get_alloc_id", Miri AllocId);
      ("miristd::miri_pointer_name", Miri Nop);
      ("miristd::miri_print_borrow_state", Miri Nop);
      (* Core *)
      (* This fails because of a silly thing with NonZero in monomorphisation, which we won't
         fix for now as it requires monomorphising trait impls.  *)
      ("alloc::boxed::{@T}::new", Fixme BoxNew);
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
      ("core::ptr::null_mut", Fixme NullPtr);
      ("core::mem::zeroed", Optim Zeroed);
      (* all float operations could be removed, but we lack bit precision when getting the
         const floats from Rust, meaning these don't really work. Either way, performance wise
         it is much preferable to override these and use SMTLib builtins. *)
      ("core::f16::{f16}::is_finite", Optim FloatIsFinite);
      ("core::f16::{f16}::is_infinite", Optim (FloatIs Infinite));
      ("core::f16::{f16}::is_nan", Optim (FloatIs NaN));
      ("core::f16::{f16}::is_normal", Optim (FloatIs Normal));
      ( "core::f16::{f16}::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ( "core::f16::{f16}::is_sign_positive",
        Optim (FloatIsSign { positive = true }) );
      ("core::f16::{f16}::is_subnormal", Optim (FloatIs Subnormal));
      ("core::f32::{f32}::is_finite", Optim FloatIsFinite);
      ("core::f32::{f32}::is_infinite", Optim (FloatIs Infinite));
      ("core::f32::{f32}::is_nan", Optim (FloatIs NaN));
      ("core::f32::{f32}::is_normal", Optim (FloatIs Normal));
      ( "core::f32::{f32}::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ( "core::f32::{f32}::is_sign_positive",
        Optim (FloatIsSign { positive = true }) );
      ("core::f32::{f32}::is_subnormal", Optim (FloatIs Subnormal));
      ("core::f64::{f64}::is_finite", Optim FloatIsFinite);
      ("core::f64::{f64}::is_infinite", Optim (FloatIs Infinite));
      ("core::f64::{f64}::is_nan", Optim (FloatIs NaN));
      ("core::f64::{f64}::is_normal", Optim (FloatIs Normal));
      ( "core::f64::{f64}::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ( "core::f64::{f64}::is_sign_positive",
        Optim (FloatIsSign { positive = true }) );
      ("core::f64::{f64}::is_subnormal", Optim (FloatIs Subnormal));
      ("core::f128::{f128}::is_finite", Optim FloatIsFinite);
      ("core::f128::{f128}::is_infinite", Optim (FloatIs Infinite));
      ("core::f128::{f128}::is_nan", Optim (FloatIs NaN));
      ("core::f128::{f128}::is_normal", Optim (FloatIs Normal));
      ( "core::f128::{f128}::is_sign_negative",
        Optim (FloatIsSign { positive = false }) );
      ( "core::f128::{f128}::is_sign_positive",
        Optim (FloatIsSign { positive = true }) );
      ("core::f128::{f128}::is_subnormal", Optim (FloatIs Subnormal));
      (* These don't compile, for some reason? *)
      ("std::panicking::try::cleanup", Fixme TryCleanup);
      ("std::panicking::catch_unwind::cleanup", Fixme TryCleanup);
      (* Allocator *)
      ("__rust_alloc", Alloc (Alloc { zeroed = false }));
      ("__rust_alloc_zeroed", Alloc (Alloc { zeroed = true }));
      ("__rust_dealloc", Alloc Dealloc);
      ("__rust_no_alloc_shim_is_unstable_v2", Alloc NoAllocShimIsUnstable);
      ("__rust_realloc", Alloc Realloc);
      (* Intrinsics *)
      ("core::intrinsics::abort", Std PanicSimple);
      ("core::intrinsics::add_with_overflow", Std (Checked (Add OUB)));
      ( "core::intrinsics::arith_offset",
        Std (PtrOp { op = Add OUB; check = false }) );
      ("core::intrinsics::assert_inhabited", Std AssertInhabited);
      (* TODO: is the following correct? *)
      ("core::intrinsics::assert_mem_uninitialized_valid", Std Nop);
      ("core::intrinsics::assert_zero_valid", Std AssertZeroValid);
      ("core::intrinsics::assume", Std Assume);
      ("core::intrinsics::black_box", Std BlackBox);
      ("core::intrinsics::breakpoint", Std Breakpoint);
      ("core::intrinsics::bswap", Std ByteSwap);
      ("core::intrinsics::catch_unwind", Std CatchUnwind);
      ("core::intrinsics::ceilf16", Std (FloatRounding Ceil));
      ("core::intrinsics::ceilf32", Std (FloatRounding Ceil));
      ("core::intrinsics::ceilf64", Std (FloatRounding Ceil));
      ("core::intrinsics::ceilf128", Std (FloatRounding Ceil));
      ("core::intrinsics::cold_path", Std Nop);
      ("core::intrinsics::compare_bytes", Std CompareBytes);
      ("core::intrinsics::copy", Std (Copy { nonoverlapping = false }));
      ( "core::intrinsics::copy_nonoverlapping",
        Std (Copy { nonoverlapping = true }) );
      ("core::intrinsics::copysignf16", Std CopySign);
      ("core::intrinsics::copysignf32", Std CopySign);
      ("core::intrinsics::copysignf64", Std CopySign);
      ("core::intrinsics::copysignf128", Std CopySign);
      ("core::intrinsics::ctpop", Std Ctpop);
      ("core::intrinsics::discriminant_value", Std DiscriminantValue);
      ("core::intrinsics::exact_div", Std ExactDiv);
      ("core::intrinsics::fabsf16", Std Abs);
      ("core::intrinsics::fabsf32", Std Abs);
      ("core::intrinsics::fabsf64", Std Abs);
      ("core::intrinsics::fabsf128", Std Abs);
      ("core::intrinsics::fadd_fast", Std (FloatFast (Add OUB)));
      ("core::intrinsics::fdiv_fast", Std (FloatFast (Div OUB)));
      ("core::intrinsics::float_to_int_unchecked", Std FloatToInt);
      ("core::intrinsics::floorf16", Std (FloatRounding Floor));
      ("core::intrinsics::floorf32", Std (FloatRounding Floor));
      ("core::intrinsics::floorf64", Std (FloatRounding Floor));
      ("core::intrinsics::floorf128", Std (FloatRounding Floor));
      ("core::intrinsics::fmaf16", Std MulAdd);
      ("core::intrinsics::fmaf32", Std MulAdd);
      ("core::intrinsics::fmaf64", Std MulAdd);
      ("core::intrinsics::fmaf128", Std MulAdd);
      ("core::intrinsics::fmul_fast", Std (FloatFast (Mul OUB)));
      ("core::intrinsics::frem_fast", Std (FloatFast (Rem OUB)));
      ("core::intrinsics::fsub_fast", Std (FloatFast (Sub OUB)));
      ("core::intrinsics::is_val_statically_known", Std IsValStaticallyKnown);
      ("core::intrinsics::likely", Std Likely);
      ("core::intrinsics::maxnumf16", Std (FloatMinMax { min = false }));
      ("core::intrinsics::maxnumf32", Std (FloatMinMax { min = false }));
      ("core::intrinsics::maxnumf64", Std (FloatMinMax { min = false }));
      ("core::intrinsics::maxnumf128", Std (FloatMinMax { min = false }));
      ("core::intrinsics::minnumf16", Std (FloatMinMax { min = true }));
      ("core::intrinsics::minnumf32", Std (FloatMinMax { min = true }));
      ("core::intrinsics::minnumf64", Std (FloatMinMax { min = true }));
      ("core::intrinsics::minnumf128", Std (FloatMinMax { min = true }));
      ("core::intrinsics::align_of", Std (AlignOf { of_val = false }));
      ("core::intrinsics::align_of_val", Std (AlignOf { of_val = true }));
      ("core::intrinsics::mul_with_overflow", Std (Checked (Mul OUB)));
      ("core::intrinsics::offset", Std (PtrOp { op = Add OUB; check = true }));
      ("core::intrinsics::ptr_guaranteed_cmp", Std PtrGuaranteedCmp);
      ( "core::intrinsics::ptr_offset_from",
        Std (PtrOffsetFrom { unsigned = false }) );
      ( "core::intrinsics::ptr_offset_from_unsigned",
        Std (PtrOffsetFrom { unsigned = true }) );
      ("core::intrinsics::raw_eq", Std RawEq);
      ( "core::intrinsics::round_ties_even_f16",
        Std (FloatRounding NearestTiesToEven) );
      ( "core::intrinsics::round_ties_even_f32",
        Std (FloatRounding NearestTiesToEven) );
      ( "core::intrinsics::round_ties_even_f64",
        Std (FloatRounding NearestTiesToEven) );
      ( "core::intrinsics::round_ties_even_f128",
        Std (FloatRounding NearestTiesToEven) );
      ("core::intrinsics::roundf16", Std (FloatRounding NearestTiesToAway));
      ("core::intrinsics::roundf32", Std (FloatRounding NearestTiesToAway));
      ("core::intrinsics::roundf64", Std (FloatRounding NearestTiesToAway));
      ("core::intrinsics::roundf128", Std (FloatRounding NearestTiesToAway));
      ("core::intrinsics::saturating_add", Std (Saturating (Add OUB)));
      ("core::intrinsics::saturating_sub", Std (Saturating (Sub OUB)));
      ("core::intrinsics::size_of", Std SizeOf);
      ("core::intrinsics::size_of_val", Std SizeOfVal);
      ("core::intrinsics::sub_with_overflow", Std (Checked (Sub OUB)));
      ("core::intrinsics::transmute", Std Transmute);
      ("core::intrinsics::truncf16", Std (FloatRounding Truncate));
      ("core::intrinsics::truncf32", Std (FloatRounding Truncate));
      ("core::intrinsics::truncf64", Std (FloatRounding Truncate));
      ("core::intrinsics::truncf128", Std (FloatRounding Truncate));
      ("core::intrinsics::type_id", Std TypeId);
      ("core::intrinsics::type_name", Std TypeName);
      ( "core::intrinsics::typed_swap_nonoverlapping",
        Std TypedSwapNonOverlapping );
      ("core::intrinsics::unchecked_add", Std (Unchecked (Add OUB)));
      ("core::intrinsics::unchecked_div", Std (Unchecked (Div OUB)));
      ("core::intrinsics::unchecked_mul", Std (Unchecked (Mul OUB)));
      ("core::intrinsics::unchecked_rem", Std (Unchecked (Rem OUB)));
      ("core::intrinsics::unchecked_shl", Std (Unchecked (Shl OUB)));
      ("core::intrinsics::unchecked_shr", Std (Unchecked (Shr OUB)));
      ("core::intrinsics::unchecked_sub", Std (Unchecked (Sub OUB)));
      ("core::intrinsics::unlikely", Std Likely);
      ("core::intrinsics::variant_count", Std VariantCount);
      ("core::intrinsics::wrapping_add", Std (Wrapping (Add OWrap)));
      ("core::intrinsics::wrapping_div", Std (Wrapping (Div OWrap)));
      ("core::intrinsics::wrapping_mul", Std (Wrapping (Mul OWrap)));
      ("core::intrinsics::wrapping_rem", Std (Wrapping (Rem OWrap)));
      ("core::intrinsics::wrapping_sub", Std (Wrapping (Sub OWrap)));
      ("core::intrinsics::write_bytes", Std WriteBytes);
      ("core::intrinsics::write_bytes::write_bytes", Std WriteBytes);
      ("core::intrinsics::write_bytes::precondition_check", Std Nop);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let std_fun_eval (f : UllbcAst.fun_decl) fun_exec =
    let open Std in
    let open Alloc in
    let open Rusteria in
    let open Miri in
    let opt_bind f opt = match opt with None -> f () | x -> x in
    let mono () =
      match List.last f.item_meta.name with
      | PeMonomorphized mono -> mono
      | _ ->
          Fmt.failwith "Expected %a to have been monomorphised" Crate.pp_name
            f.item_meta.name
    in
    let is_intrinsic = Charon_util.decl_has_attr f "rustc_intrinsic" in
    (* Rust allows defining functions and marking them as intrinsics within a module,
       and the compiler will treat them as the intrinsic of the same name; e.g.
       mod Foo {
        #[rustc_intrinsic]
        unsafe fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
       }
       This means their path doesn't match the one we expect for the patterns; we normalise
       this here. Another solution could be to match intrinsics on their name only and not their
       path (possibly in a separate function?), but that's maybe overkill for the very rare
       cases were people actually re-define the intrinsics. *)
    let name =
      match f.item_meta.name with
      | _ when not is_intrinsic -> (
          match List.last f.item_meta.name with
          | PeIdent (name, _) as ident
            when String.starts_with ~prefix:"__rust" name ->
              [ ident ]
          | _ -> f.item_meta.name)
      | PeIdent ("core", _) :: _ -> f.item_meta.name
      | _ -> (
          match List.rev f.item_meta.name with
          | (PeIdent _ as name) :: _ ->
              [
                PeIdent ("core", Types.Disambiguator.zero);
                PeIdent ("intrinsics", Types.Disambiguator.zero);
                name;
              ]
          | (PeMonomorphized _ as mono) :: (PeIdent _ as name) :: _ ->
              [
                PeIdent ("core", Types.Disambiguator.zero);
                PeIdent ("intrinsics", Types.Disambiguator.zero);
                name;
                mono;
              ]
          | _ -> failwith "Unexpected intrinsic shape")
    in
    let ctx = Crate.as_namematcher_ctx () in
    NameMatcherMap.find_opt ctx match_config name std_fun_map
    |> ( Option.map @@ function
         | Rusteria Assert -> assert_
         | Rusteria Assume -> assume
         | Rusteria Nondet -> nondet f.signature
         | Rusteria Panic -> panic
         | Miri AllocId -> alloc_id
         | Miri Nop -> nop
         | Fixme BoxNew -> fixme_box_new f.signature
         | Fixme Index -> array_index_fn f.signature
         | Fixme Panic -> panic
         | Fixme Nop -> nop
         | Fixme NullPtr -> fixme_null_ptr
         | Fixme TryCleanup -> fixme_try_cleanup
         | Optim (FloatIs fc) -> float_is fc
         | Optim FloatIsFinite -> float_is_finite
         | Optim (FloatIsSign { positive }) -> float_is_sign positive
         | Optim Zeroed -> zeroed f.signature
         | Alloc (Alloc { zeroed }) -> alloc ~zeroed
         | Alloc Dealloc -> dealloc
         | Alloc NoAllocShimIsUnstable -> no_alloc_shim_is_unstable
         | Alloc Realloc -> realloc
         | Std Abs -> abs
         | Std (AlignOf _) -> min_align_of (mono ())
         | Std AssertZeroValid -> assert_zero_is_valid (mono ())
         | Std AssertInhabited -> assert_inhabited (mono ())
         | Std Assume -> std_assume
         | Std BlackBox -> black_box
         | Std Breakpoint -> breakpoint
         | Std ByteSwap -> byte_swap f.signature
         | Std CatchUnwind -> catch_unwind fun_exec
         | Std (Checked op) -> checked_op op f.signature
         | Std CompareBytes -> compare_bytes
         | Std (Copy { nonoverlapping }) -> copy_fn nonoverlapping f.signature
         | Std CopySign -> copy_sign
         | Std Ctpop -> ctpop f.signature
         | Std DiscriminantValue -> discriminant_value f.signature
         | Std ExactDiv -> exact_div f.signature
         | Std (FloatFast bop) -> float_fast bop
         | Std (FloatMinMax { min }) -> float_minmax min
         | Std (FloatRounding rm) -> float_rounding rm
         | Std FloatToInt -> float_to_int f.signature
         | Std Index -> array_index_fn f.signature
         | Std IsValStaticallyKnown -> is_val_statically_known
         | Std Likely -> likely
         | Std MulAdd -> mul_add
         | Std Nop -> nop
         | Std PanicSimple -> std_panic
         | Std (PtrByteOp op) -> ptr_op ~byte:true op f.signature
         | Std PtrGuaranteedCmp -> ptr_guaranteed_cmp
         | Std (PtrOp { op; check }) -> ptr_op ~check op f.signature
         | Std (PtrOffsetFrom { unsigned }) ->
             ptr_offset_from unsigned f.signature
         | Std RawEq -> raw_eq f.signature
         | Std (Saturating op) -> saturating op f.signature
         | Std SizeOf -> size_of (mono ())
         | Std SizeOfVal -> size_of_val f.signature
         | Std Transmute -> transmute f.signature
         | Std TypeId -> type_id (mono ())
         | Std TypeName -> type_name (mono ())
         | Std TypedSwapNonOverlapping -> typed_swap_nonoverlapping f.signature
         | Std (Unchecked op) -> unchecked_op op f.signature
         | Std VariantCount -> variant_count (mono ())
         | Std (Wrapping op) -> wrapping_op op f.signature
         | Std WriteBytes -> write_bytes (mono ()) )
    |> opt_bind @@ fun () ->
       if is_intrinsic then
         Option.some @@ fun ~args:_ _ ->
         Fmt.kstr not_impl "Unsupported intrinsic: %a" Crate.pp_name name
       else None

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
