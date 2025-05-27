open Charon
module NameMatcherMap = Charon.NameMatcher.NameMatcherMap
open Rustsymex

module M (Heap : Heap_intf.S) = struct
  module Std = Std.M (Heap)
  module Rusteria = Rusteria.M (Heap)
  module Miri = Miri.M (Heap)

  let match_config =
    NameMatcher.{ map_vars_to_vars = false; match_with_trait_decl_refs = false }

  type type_loc = GenArg | Input

  type std_fun =
    (* Rusteria builtins *)
    | RusteriaAssert
    | RusteriaAssume
    | RusteriaNondet
    | RusteriaPanic
    (* Miri builtins *)
    | MiriAllocId
    (* Std *)
    | Abs
    | AssertZeroValid
    | AssertInhabited
    | Assume
    | ByteSwap
    | BlackBox
    | BoxIntoRaw
    | Checked of Expressions.binop
    | CompareBytes
    | CopyNonOverlapping
    | CopySign
    | Ctpop
    | DiscriminantValue
    | ExactDiv
    | FloatFast of Expressions.binop
    | FloatIs of fpclass
    | FloatIsFinite
    | FloatIsSign of { positive : bool }
    | FloatMinMax of { min : bool }
    | Index
    | IsValStaticallyKnown
    | Likely
    | MinAlignOf of type_loc
    | MulAdd
    | Nop
    | PanicSimple
    | PtrByteOp of Expressions.binop
    | PtrGuaranteedCmp
    | PtrOp of Expressions.binop
    | PtrOffsetFrom of { unsigned : bool }
    | RawEq
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
    | Zeroed

  let std_fun_map =
    [
      (* Rusteria builtins *)
      ("rusteria::assert", RusteriaAssert);
      ("rusteria::assume", RusteriaAssume);
      ("rusteria::nondet", RusteriaNondet);
      ("rusteria::panic", RusteriaPanic);
      (* Kani builtins -- we re-define these for nicer call traces *)
      ("kani::assert", RusteriaAssert);
      ("kani::panic", RusteriaPanic);
      (* Miri builtins *)
      ("miristd::miri_get_alloc_id", MiriAllocId);
      ("miristd::miri_pointer_name", Nop);
      ("miristd::miri_print_borrow_state", Nop);
      (* Core *)
      (* FIXME: get rid of these, as Charon improves *)
      ("alloc::boxed::{alloc::boxed::Box}::into_raw", BoxIntoRaw);
      ("alloc::boxed::{@T}::from_raw", BoxIntoRaw);
      (* FIXME: the below indexes fail because the code doesn't get monomorphised properly, and
         returns a thin pointer rather than a fat one. *)
      ("core::array::{core::ops::index::Index}::index", Index);
      ("core::array::{core::ops::index::IndexMut}::index_mut", Index);
      ("core::slice::index::{core::ops::index::Index}::index", Index);
      ("core::slice::index::{core::ops::index::IndexMut}::index_mut", Index);
      ("core::cell::panic_already_mutably_borrowed", PanicSimple);
      ("core::hint::black_box", BlackBox);
      ("core::mem::zeroed", Zeroed);
      (* all float operations could be removed, but we lack bit precision when getting the
         const floats from Rust, meaning these don't really work. Either way, performance wise
         it is much preferable to override these and use SMTLib builtins. *)
      ("core::f16::{f16}::is_finite", FloatIsFinite);
      ("core::f16::{f16}::is_infinite", FloatIs FP_infinite);
      ("core::f16::{f16}::is_nan", FloatIs FP_nan);
      ("core::f16::{f16}::is_normal", FloatIs FP_normal);
      ("core::f16::{f16}::is_sign_negative", FloatIsSign { positive = false });
      ("core::f16::{f16}::is_sign_positive", FloatIsSign { positive = true });
      ("core::f16::{f16}::is_subnormal", FloatIs FP_subnormal);
      ("core::f32::{f32}::is_finite", FloatIsFinite);
      ("core::f32::{f32}::is_infinite", FloatIs FP_infinite);
      ("core::f32::{f32}::is_nan", FloatIs FP_nan);
      ("core::f32::{f32}::is_normal", FloatIs FP_normal);
      ("core::f32::{f32}::is_sign_negative", FloatIsSign { positive = false });
      ("core::f32::{f32}::is_sign_positive", FloatIsSign { positive = true });
      ("core::f32::{f32}::is_subnormal", FloatIs FP_subnormal);
      ("core::f64::{f64}::is_finite", FloatIsFinite);
      ("core::f64::{f64}::is_infinite", FloatIs FP_infinite);
      ("core::f64::{f64}::is_nan", FloatIs FP_nan);
      ("core::f64::{f64}::is_normal", FloatIs FP_normal);
      ("core::f64::{f64}::is_sign_negative", FloatIsSign { positive = false });
      ("core::f64::{f64}::is_sign_positive", FloatIsSign { positive = true });
      ("core::f64::{f64}::is_subnormal", FloatIs FP_subnormal);
      ("core::f128::{f128}::is_finite", FloatIsFinite);
      ("core::f128::{f128}::is_infinite", FloatIs FP_infinite);
      ("core::f128::{f128}::is_nan", FloatIs FP_nan);
      ("core::f128::{f128}::is_normal", FloatIs FP_normal);
      ("core::f128::{f128}::is_sign_negative", FloatIsSign { positive = false });
      ("core::f128::{f128}::is_sign_positive", FloatIsSign { positive = true });
      ("core::f128::{f128}::is_subnormal", FloatIs FP_subnormal);
      (* FIXME: all core::ptr operations could be removed, however because we must enable
         ub_checks at runtime due to unchecked_op, this means ub checks also happen in
         the impl of core::ptr::..., and these checks are *SLOW* -- they do binary operations
         on the integer value of the pointer to ensure it is well aligned etc. *)
      ("core::ptr::const_ptr::{@T}::add", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::byte_add", PtrByteOp Add);
      ("core::ptr::const_ptr::{@T}::byte_offset", PtrByteOp Add);
      ("core::ptr::const_ptr::{@T}::byte_sub", PtrByteOp Sub);
      ("core::ptr::const_ptr::{@T}::offset", PtrOp Add);
      ("core::ptr::const_ptr::{@T}::sub", PtrOp Sub);
      ("core::ptr::mut_ptr::{@T}::add", PtrOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_add", PtrByteOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_offset", PtrByteOp Add);
      ("core::ptr::mut_ptr::{@T}::byte_sub", PtrByteOp Sub);
      ("core::ptr::mut_ptr::{@T}::offset", PtrOp Add);
      ("core::ptr::mut_ptr::{@T}::sub", PtrOp Sub);
      (* Intrinsics *)
      ("core::intrinsics::abort", PanicSimple);
      ("core::intrinsics::add_with_overflow", Checked Add);
      ("core::intrinsics::arith_offset", PtrOp Add);
      ("core::intrinsics::assert_inhabited", AssertInhabited);
      (* TODO: is the following correct? *)
      ("core::intrinsics::assert_mem_uninitialized_valid", Nop);
      ("core::intrinsics::assert_zero_valid", AssertZeroValid);
      ("core::intrinsics::assume", Assume);
      ("core::intrinsics::black_box", BlackBox);
      ("core::intrinsics::bswap", ByteSwap);
      ("core::intrinsics::cold_path", Nop);
      ("core::intrinsics::compare_bytes", CompareBytes);
      ("core::intrinsics::copy_nonoverlapping", CopyNonOverlapping);
      ("core::intrinsics::copysignf16", CopySign);
      ("core::intrinsics::copysignf32", CopySign);
      ("core::intrinsics::copysignf64", CopySign);
      ("core::intrinsics::copysignf128", CopySign);
      ("core::intrinsics::ctpop", Ctpop);
      ("core::intrinsics::discriminant_value", DiscriminantValue);
      ("core::intrinsics::exact_div", ExactDiv);
      ("core::intrinsics::fabsf16", Abs);
      ("core::intrinsics::fabsf32", Abs);
      ("core::intrinsics::fabsf64", Abs);
      ("core::intrinsics::fabsf128", Abs);
      ("core::intrinsics::fadd_fast", FloatFast Add);
      ("core::intrinsics::fdiv_fast", FloatFast Div);
      ("core::intrinsics::fmaf16", MulAdd);
      ("core::intrinsics::fmaf32", MulAdd);
      ("core::intrinsics::fmaf64", MulAdd);
      ("core::intrinsics::fmaf128", MulAdd);
      ("core::intrinsics::maxnumf16", FloatMinMax { min = false });
      ("core::intrinsics::maxnumf32", FloatMinMax { min = false });
      ("core::intrinsics::maxnumf64", FloatMinMax { min = false });
      ("core::intrinsics::maxnumf128", FloatMinMax { min = false });
      ("core::intrinsics::minnumf16", FloatMinMax { min = true });
      ("core::intrinsics::minnumf32", FloatMinMax { min = true });
      ("core::intrinsics::minnumf64", FloatMinMax { min = true });
      ("core::intrinsics::minnumf128", FloatMinMax { min = true });
      ("core::intrinsics::fmul_fast", FloatFast Mul);
      ("core::intrinsics::fsub_fast", FloatFast Sub);
      ("core::intrinsics::is_val_statically_known", IsValStaticallyKnown);
      ("core::intrinsics::likely", Likely);
      ("core::intrinsics::min_align_of", MinAlignOf GenArg);
      ("core::intrinsics::min_align_of_val", MinAlignOf Input);
      ("core::intrinsics::offset", PtrOp Add);
      ("core::intrinsics::pref_align_of", MinAlignOf GenArg);
      ("core::intrinsics::ptr_guaranteed_cmp", PtrGuaranteedCmp);
      ("core::intrinsics::ptr_offset_from", PtrOffsetFrom { unsigned = false });
      ( "core::intrinsics::ptr_offset_from_unsigned",
        PtrOffsetFrom { unsigned = true } );
      ("core::intrinsics::raw_eq", RawEq);
      ("core::intrinsics::size_of", SizeOf);
      ("core::intrinsics::size_of_val", SizeOfVal);
      ("core::intrinsics::transmute", Transmute);
      ("core::intrinsics::type_id", TypeId);
      ("core::intrinsics::type_name", TypeName);
      ("core::intrinsics::typed_swap_nonoverlapping", TypedSwapNonOverlapping);
      ("core::intrinsics::unchecked_add", Unchecked Add);
      ("core::intrinsics::unchecked_div", Unchecked Div);
      ("core::intrinsics::unchecked_mul", Unchecked Mul);
      ("core::intrinsics::unchecked_rem", Unchecked Rem);
      ("core::intrinsics::unchecked_shl", Unchecked Shl);
      ("core::intrinsics::unchecked_shr", Unchecked Shr);
      ("core::intrinsics::unchecked_sub", Unchecked Sub);
      ("core::intrinsics::unlikely", Likely);
      ("core::intrinsics::variant_count", VariantCount);
      ("core::intrinsics::wrapping_add", Wrapping Add);
      ("core::intrinsics::wrapping_div", Wrapping Div);
      ("core::intrinsics::wrapping_mul", Wrapping Mul);
      ("core::intrinsics::wrapping_rem", Wrapping Rem);
      ("core::intrinsics::wrapping_sub", Wrapping Sub);
      ("core::intrinsics::write_bytes", WriteBytes);
      ("core::intrinsics::write_bytes::write_bytes", WriteBytes);
      ("core::intrinsics::write_bytes::precondition_check", Nop);
    ]
    |> List.map (fun (p, v) -> (NameMatcher.parse_pattern p, v))
    |> NameMatcherMap.of_list

  let std_fun_eval (f : UllbcAst.fun_decl) =
    let open Std in
    let open Rusteria in
    let open Miri in
    let opt_bind f opt = match opt with None -> f () | x -> x in
    let ctx = Crate.as_namematcher_ctx () in
    let real_name =
      if Charon_util.decl_has_attr f "rustc_intrinsic" then
        Types.
          [
            PeIdent ("core", Disambiguator.zero);
            PeIdent ("intrinsics", Disambiguator.zero);
            List.last f.item_meta.name;
          ]
      else f.item_meta.name
    in
    NameMatcherMap.find_opt ctx match_config real_name std_fun_map
    |> ( Option.map @@ function
         | RusteriaAssert -> assert_
         | RusteriaAssume -> assume
         | RusteriaNondet -> nondet f.signature
         | RusteriaPanic -> panic
         | MiriAllocId -> alloc_id
         | Abs -> abs
         | AssertZeroValid -> assert_zero_is_valid f.signature
         | AssertInhabited -> assert_inhabited f.signature
         | Assume -> std_assume
         | ByteSwap -> byte_swap f.signature
         | BlackBox -> black_box
         | BoxIntoRaw -> box_into_raw
         | Checked op -> checked_op op f.signature
         | CompareBytes -> compare_bytes
         | CopyNonOverlapping -> copy_nonoverlapping_fn f.signature
         | CopySign -> copy_sign
         | Ctpop -> ctpop f.signature
         | DiscriminantValue -> discriminant_value f.signature
         | ExactDiv -> exact_div f.signature
         | FloatFast bop -> float_fast bop
         | FloatIs fc -> float_is fc
         | FloatIsFinite -> float_is_finite
         | FloatIsSign { positive } -> float_is_sign positive
         | FloatMinMax { min } -> float_minmax min
         | Index -> array_index_fn f.signature
         | IsValStaticallyKnown -> is_val_statically_known
         | Likely -> likely
         | MinAlignOf t -> min_align_of ~in_input:(t = Input) f.signature
         | MulAdd -> mul_add
         | Nop -> nop
         | PanicSimple -> std_panic
         | PtrByteOp op -> ptr_op ~byte:true op f.signature
         | PtrGuaranteedCmp -> ptr_guaranteed_cmp
         | PtrOp op -> ptr_op op f.signature
         | PtrOffsetFrom { unsigned } -> ptr_offset_from unsigned f.signature
         | RawEq -> raw_eq f.signature
         | SizeOf -> size_of f.signature
         | SizeOfVal -> size_of_val f.signature
         | Transmute -> transmute f.signature
         | TypeId -> type_id f.signature
         | TypeName -> type_name f.signature
         | TypedSwapNonOverlapping -> typed_swap_nonoverlapping f.signature
         | Unchecked op -> unchecked_op op f.signature
         | VariantCount -> variant_count f.signature
         | Wrapping op -> wrapping_op op f.signature
         | WriteBytes -> write_bytes f.signature
         | Zeroed -> zeroed f.signature )
    |> opt_bind @@ fun () ->
       let is_intrinsic =
         match real_name with
         | PeIdent (("core" | "std"), _) :: PeIdent ("intrinsics", _) :: _ ->
             true
         | _ -> false
       in
       if is_intrinsic then
         Option.some @@ fun ~args:_ ~state:_ ->
         Fmt.kstr not_impl "Unsupported intrinsic: %a" Crate.pp_name real_name
       else None

  let builtin_fun_eval (f : Expressions.builtin_fun_id) generics =
    let open Std in
    match f with
    | ArrayRepeat -> array_repeat generics
    | ArrayToSliceMut -> array_slice ~mut:true generics
    | ArrayToSliceShared -> array_slice ~mut:false generics
    | Index idx -> array_index idx generics
    | BoxNew -> box_new generics
    | PtrFromParts _ -> from_raw_parts
end
