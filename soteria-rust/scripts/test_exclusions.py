from typing import Optional

from common import *

# Tests that we want to skip when running tests quickly to look for regressions, in development
# path -> (outcome, optional reason)
SKIPPED_TESTS: dict[str, tuple[Outcome, Optional[str]]] = {
    # Kani
    "ArithOperators/rem_float_fixme.rs": Outcome.PASS(),
    "FloatingPoint/main.rs": Outcome.PASS(),
    "Intrinsics/Count/ctpop.rs": Outcome.UNKNOWN(
        "Doesn't work in Charon, 2^N branches in Obol",
    ),
    "Intrinsics/FastMath/div_f64.rs": Outcome.UNKNOWN("Very slow"),
    "Intrinsics/Math/Rounding/Round/roundf64.rs": Outcome.PASS(),
    "Intrinsics/Math/Rounding/RoundTiesEven/round_ties_even_f64.rs": Outcome.PASS(),
    # Miri
    "pass/issues/issue-17877.rs": Outcome.UNKNOWN(
        "Makes an array of size 16384, too slow",
    ),
    "pass/arrays.rs": Outcome.UNKNOWN(
        "Makes an array [(), usize::MAX], which we try evaluating",
    ),
}

KNOWN_ISSUES = {
    # Kani
    "ArithOperators/unsafe_add_fail.rs": "The main function takes a parameter? Kani crashes too",
    "ArithOperators/unsafe_mul_fail.rs": "The main function takes a parameter? Kani crashes too",
    "ArithOperators/unsafe_sub_fail.rs": "The main function takes a parameter? Kani crashes too",
    "Cleanup/unwind_fixme.rs": "The main function takes a paramter? Kani crashes too",
    "CodegenConstValue/main.rs": "We don't translate slice constants",
    "DynTrait/vtable_size_align_drop.rs": "The test does a ptr-int-ptr cast, which we don't support",
    "FunctionCall/type_check.rs": "We don't support TypeId",
    "FunctionCall/Variadic/fixme_main.rs": "We don't handle functions with spread arguments (not in Charon)",
    "FunctionCall/Variadic/main.rs": "We don't handle functions with spread arguments (not in Charon)",
    "FunctionCall_Ret-Param/main.rs": "Creates 2^N branches",
    "Intrinsics/ConstEval/pref_align_of.rs": "Requires support for custom target architectures",
    "Intrinsics/ConstEval/type_id.rs": "We don't support TypeId",
    "Intrinsics/Copy/copy_nonoverlapping_swap.rs": "Uses mem::uninitialized, which is instant UB lmao",
    "Intrinsics/CopySign/copysignf32.rs": "SMT-lib limitations around NaN mean we can't model this",
    "Intrinsics/CopySign/copysignf64.rs": "SMT-lib limitations around NaN mean we can't model this",
    "LayoutRandomization/should_fail.rs": "We don't handle layout randomization yet",
    "Panic/prove_safety_only.rs": "We don't have a mode to prove safety only",
    "Slice/extra_checks_fail.rs": "There is no error here, and Kani fails this test too",
    "Uninit/access-padding-enum-diverging-variants.rs": "Kani can't handle variants with different paddings",
    "Unwind-Attribute/fixme_lib.rs": "We don't have a flag to not check for unwinding",
    "ValidValues/write_bytes.rs": "Kani checks for validity on write, whereas Miri does on read; we copy Miri.",
    "Whitespace/main.rs": "Weird double move; maybe moving from a niched value doesn't actually move it?",
    # Miri
    "fail/dangling_pointers/dangling_pointer_project_underscore_let.rs": "let _ = ... assignments get optimized out",
    "fail/dangling_pointers/dangling_pointer_project_underscore_let_type_annotation.rs": "let _ = ... assignments get optimized out",
    "fail/dangling_pointers/dangling_pointer_project_underscore_match.rs": "let _ = ... assignments get optimized out",
    "fail/dangling_pointers/deref_dangling_box.rs": "We don't check for dangling pointers for boxes",
    "fail/dyn-call-trait-mismatch.rs": "We don't check the validity of dyn calls",
    "fail/dyn-upcast-nop-wrong-trait.rs": "We don't check the validity of dyn casts",
    "fail/dyn-upcast-trait-mismatch.rs": "We don't check the validity of dyn casts",
    "fail/intrinsics/typed-swap-invalid-scalar.rs": "Uses weird CFGs, technically we pass it",
    "fail/issue-miri-1112.rs": "We don't check the validity of VTables",
    "fail/function_calls/arg_inplace_mutate.rs": "We don't check that arguments aren't mutated in place",
    "fail/function_calls/return_pointer_aliasing_read.rs": "We don't check arguments don't alias with the return place",
    "fail/function_calls/return_pointer_aliasing_write.rs": "We don't check arguments don't alias with the return place",
    "fail/overlapping_assignment.rs": "MIR-only check for assignment overlap (we don't do this atm)",
    "fail/validity/cast_fn_ptr_invalid_caller_ret.rs": "We don't use a fn ptr's type for checking validity",
    "fail/validity/nonzero.rs": "The valid_range_start attribute isn't parsed by Charon?",
    "fail/validity/wrong-dyn-trait-generic.rs": "We don't check the validity of dyn casts",
    "fail/validity/wrong-dyn-trait.rs": "We don't check the validity of dyn casts",
    "pass/align.rs": "We don't allow ptr-int-ptr conversions, Miri does (under a flag)",
    "pass/integer-ops.rs": "Miri allows negative bit shifts, we don't (like Kani)",
    "pass/disable-alignment-check.rs": "We don't provide a way to disable alignment checks",
    "pass/dyn-traits.rs": "VTable addresses should be randomised",
    "pass/extern_types.rs": "We don't handle extern types",
    "pass/function_calls/abi_compat.rs": "We are too restrictive on fn pointer casts",
    "pass/issues/issue-120337-irrefutable-let-ice.rs": "Weird ! type in a union?",
    "pass/issues/issue-3200-packed-field-offset.rs": "We don't handle repr(packed) in dangling checks",
    "pass/issues/issue-3200-packed2-field-offset.rs": "We don't handle repr(packed) in dangling checks",
    "pass/issues/issue-miri-1075.rs": "We don't check the status code on process::exit(N) -- 0 is ok!",
    "pass/overflow_checks_off.rs": "We don't provide a way to disable overflow checks",
    "pass/provenance.rs": "It is unclear how to properly do ptr-int-ptr conversions",
    "pass/ptr_int_from_exposed.rs": "It is unclear how to properly do ptr-int-ptr conversions",
    "pass/ptr_offset.rs": "It is unclear how to properly do ptr-int-ptr conversions",
    "pass/u128.rs": "We don't do int-float-int conversions properly",
    "panic/mir-validation.rs": "We don't validate the MIR for projections",
}


KANI_EXCLUSIONS = [
    # unsupported: concurrency
    "/AsyncAwait/",
    "/Atomic/",
    "/Coroutines/",
    "Drop/drop_after_moving_across_channel.rs",
    "Drop/drop_slice.rs",
    "SizeAndAlignOfDst/main.rs",
    "SizeAndAlignOfDst/main_assert.rs",
    "/ThreadLocalRef/",
    "Uninit/atomic.rs",
    # unsupported: FFI
    "/ForeignItems/",
    "FunctionCall/Variadic/main.rs",
    "Static/unsafe_extern_static_uninitialized.rs",
    # unsupported: libc
    "Cast/cast_abstract_args_to_concrete.rs",
    "/LibC/",
    "/Strings/copy_empty_string_by_intrinsic.rs",
    "Slice/pathbuf.rs",
    # unsupported: SIMD
    "/SIMD/",
    # unsupported: volatile
    "/Volatile/",
    "/VolatileIntrinsics/",
    # unsupported: #[kani::stub]
    "/Stubbing/",
    # unsupported: #[kani::proof_for_contract], #[kani::ensures], #[kani::requires]
    "/FunctionContracts/",
    # unsupported: kani::exists, kani::forall
    "/Quantifiers/",
    # unsupported: kani::mem
    "/MemPredicates/",
    "SizeAndAlignOfDst/unsized_tail.rs",
    "ValidValues/unaligned.rs",
    # unsupported: kani::float::float_to_int_in_range
    "FloatToIntInRange/test.rs",
    # ignore: we don't support --test yet
    "Options/check_tests.rs",
    # ignore: invalid tests
    "ArithOperators/unsafe_add_fail.rs",
    "ArithOperators/unsafe_mul_fail.rs",
    "ArithOperators/unsafe_sub_fail.rs",
    # ignore: tests that Kani also fails/can't compile
    "Intrinsics/Assert/uninit_valid_panic.rs",
    "Intrinsics/Forget/forget_fail.rs",
    "NondetVectors/main.rs",
    "Panic/compile_panic.rs",
    "Panic/prove_safety_only.rs",
    "Serde/main.rs",
    "Slice/drop_in_place.rs",
    "Slice/extra_checks_fail.rs",
    # utils
    "fixme",
    "/Helpers/",
    "/UnsizedCoercion/defs.rs",
]

MIRI_EXCLUSIONS = [
    # unsupported: concurrency
    "async",
    "/concurrency/",
    "/data_race/",
    "/many-seeds/",
    "/tls/",
    "0weak_memory_consistency",
    "fail/tls_macro_leak.rs",
    "fail/tls_static_leak.rs",
    "fail/panic/tls_macro_const_drop_panic.rs",
    "fail/panic/tls_macro_drop_panic.rs",
    "pass/issues/issue-139553.rs",
    "fail/tree_borrows/reservedim_spurious_write.rs",
    "fail/tree_borrows/spurious_read.rs",
    "fail/unaligned_pointers/atomic_unaligned.rs",
    "fail/coroutine-pinned-moved.rs",
    "fail/both_borrows/retag_data_race_write.rs",
    "pass/coroutine.rs",
    "pass/future-self-referential.rs",
    "pass/issues/issue-miri-2068.rs",
    "pass/move-data-across-await-point.rs",
    "pass/rc.rs",
    "pass/function_calls/abi_compat.rs",
    "pass/track-caller-attribute.rs",
    "pass/atomic-compare-exchange-weak-never-fail.rs",
    "pass/atomic-readonly-load.rs",
    "pass/atomic.rs",
    "pass/box-custom-alloc-aliasing.rs",
    "pass/dyn-arbitrary-self.rs",
    "pass/dyn-upcast.rs",
    "pass/getpid.rs",
    "pass/issues/issue-33387.rs",
    "pass/leak-in-static.rs",
    "pass/mpsc.rs",
    "pass/panic/catch_panic.rs",
    "pass/panic/concurrent-panic.rs",
    "pass/panic/std-panic-locations.rs",
    "pass/panic/thread_panic.rs",
    "pass/sendable-class.rs",
    "pass/send-is-not-static-par-for.rs",
    "pass/static_memory_modification.rs",
    "pass/tree_borrows/read_retag_no_race.rs",
    "pass/tree_borrows/spurious_read.rs",
    "weak_memory",
    "pass/iter_macro.rs",
    # unsupported: FFI
    "/native-lib/",
    "exported_symbol",
    "extern_static",
    "fail/function_calls/check_arg_count_abort.rs",
    "fail/unsupported_foreign_function.rs",
    "fail/extern-type-field-offset.rs",
    "fail/function_calls/check_arg_abi.rs",
    "fail/function_calls/check_arg_count_too_many_args.rs",
    "fail/function_calls/exported_symbol_shim_clashing.rs",
    "fail/function_calls/check_arg_count_too_few_args.rs",
    "pass/extern_types.rs",
    # unsupported: GenMC
    "genmc/pass/test_cxx_build.rs",
    # unsuppported: libc
    "fail/alloc/global_system_mixup.rs",
    "pass/global_allocator.rs",
    "pass/heap_allocator.rs",
    # unsupported: Miri builtins
    "pass/backtrace/backtrace-api-v1.rs",
    "fail/alloc/no_global_allocator.rs",
    "fail/panic/unwind_panic_abort.rs",
    "fail/unaligned_pointers/promise_alignment.rs",
    "fail/unaligned_pointers/promise_alignment_zero.rs",
    # unsupported: OS builtins
    "/shims/",
    # unsupported: SIMD
    "simd",
    "fail/function_pointers/abi_mismatch_vector.rs",
    # unsupported: stacked borrows
    "/stacked-borrows/",
    "/stacked_borrows/",
    # unsupported: tail calls
    "tail_call",
    # unsupported: other targets (x86, wasm)
    "fail/function_calls/target_feature.rs",
    "fail/function_calls/target_feature_wasm.rs",
    "fail/intrinsics/intrinsic_target_feature.rs",
    "pass/function_calls/target_feature.rs",
    "pass/wtf8.rs",
    # ignored: we don't handle @revisions annotations
    "fail/intrinsics/typed-swap-invalid-scalar.rs",
    # ignored: tests Miri fails
    "fail/no_main.rs",
    "pass/provenance.rs",
    "pass/transmute_ptr.rs",
    # ignored: tests with dependencies
    "/deps/",
    "/fail-dep/",
    "/pass-dep/",
    # utils
    "/ui.rs",
    "/utils/",
]
