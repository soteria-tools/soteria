from typing import Callable, Iterable, TypedDict, assert_never

from cliopts import ArgError, CliOpts, SuiteName
from common import *

DynFlagFn = Optional[Callable[[Path], list[str]]]


class TestConfig(TypedDict):
    name: str
    root: Path
    args: list[str]
    dyn_flags: DynFlagFn
    tests: list[Path]


KANI_PATH = (PWD / ".." / ".." / ".." / "kani" / "tests" / "kani").resolve()
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

MIRI_PATH = (PWD / ".." / ".." / ".." / "miri" / "tests").resolve()
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


def filter_tests(opts: CliOpts, tests: Iterable[Path]) -> list[Path]:
    exclusions = opts["exclusions"]
    filters = opts["filters"]
    tests = sorted(
        t
        for t in tests
        if (not any(e in str(t) for e in exclusions))
        and (filters == [] or all(f in str(t) for f in filters))
    )
    return tests


def with_cache(fn: DynFlagFn) -> DynFlagFn:
    if fn is None:
        return None
    cache: dict[Path, list[str]] = {}

    def cached_fn(file: Path) -> list[str]:
        if file in cache:
            return cache[file]
        flags = fn(file)
        cache[file] = flags
        return flags

    return cached_fn


def get_config_line(file: Path, prefix: str) -> Optional[str]:
    with open(file, "r") as f:
        for line in f:
            if line.startswith(prefix):
                return line.rstrip("\n").split(prefix)[1].strip()
    return None


def kani(opts: CliOpts) -> TestConfig:

    @with_cache
    def kani_dyn_flags(file: Path) -> list[str]:
        config = get_config_line(file, "// kani-flags:")
        if config:
            return config.split()
        return []

    root = Path(KANI_PATH)
    tests = filter_tests(
        opts,
        (
            path
            for path in root.rglob("*.rs")
            if not any(exclusion in str(path) for exclusion in KANI_EXCLUSIONS)
        ),
    )

    if opts["tool"] == "Kani":
        args = []
    elif opts["tool"] == "Miri":
        args = ["--test", "-Zmiri-ignore-leaks"]
    elif opts["tool"] == "Rusteria":
        args = ["--ignore-leaks", "--kani"]
    else:
        assert_never(opts["tool"])

    return {
        "name": "Kani",
        "root": root,
        "args": args,
        "dyn_flags": kani_dyn_flags if opts["tool"] == "Kani" else None,
        "tests": tests,
    }


def miri(opts: CliOpts) -> TestConfig:
    @with_cache
    def rusteria_dyn_flags(file: Path) -> list[str]:
        flags = []
        config = get_config_line(file, "//@compile-flags:")
        # if file contains "-Zmiri-ignore-leaks", add "--ignore-leaks"
        if config:
            if "-Zmiri-ignore-leaks" in config:
                flags.append("--ignore-leaks")
            if "-Zmiri-strict-provenance" in config:
                flags.append("--provenance=strict")
            if "-Zmiri-permissive-provenance" in config:
                flags.append("--provenance=permissive")
            if "-Zmiri-disable-stacked-borrows" in config:
                flags.append("--ignore-aliasing")
        return flags

    @with_cache
    def miri_dyn_flags(file: Path) -> list[str]:
        config = get_config_line(file, "//@compile-flags:")
        if config:
            return config.split()
        return []

    root = Path(MIRI_PATH)
    tests = filter_tests(
        opts,
        (
            path
            for path in root.rglob("*.rs")
            if not any(exclusion in str(path) for exclusion in MIRI_EXCLUSIONS)
        ),
    )

    if opts["tool"] == "Kani":
        args = ["-Z=uninit-checks", "-Z=valid-value-checks"]
        dyn_flags = None
    elif opts["tool"] == "Miri":
        args = []
        dyn_flags = miri_dyn_flags
    elif opts["tool"] == "Rusteria":
        args = ["--miri"]
        dyn_flags = rusteria_dyn_flags
    else:
        assert_never(opts["tool"])

    return {
        "name": "Miri",
        "root": root,
        "args": args,
        "dyn_flags": dyn_flags,
        "tests": tests,
    }


def custom(opts: CliOpts) -> TestConfig:
    kani_config = kani(opts)
    miri_config = miri(opts)

    def dyn_flags(file: Path) -> list[str]:
        if "/kani/" in str(file):
            dyn = kani_config["dyn_flags"]
            flags = kani_config["args"]
        elif "/miri/" in str(file):
            dyn = miri_config["dyn_flags"]
            flags = miri_config["args"]
        else:
            dyn = None
            flags = []
        return (dyn(file) if dyn else []) + flags

    if opts["test_folder"] is not None:
        root = opts["test_folder"]
        files = opts["test_folder"].rglob("*.rs")
    elif opts["test_file"] is not None:
        with open(opts["test_file"], "r") as f:
            files = [Path(p.strip()) for p in f.readlines()]
        root = Path(os.path.commonpath(files))
    else:
        raise ArgError("No test folder specified, use --folder <path>")
    tests = filter_tests(opts, files)
    return {
        "name": "Custom",
        "root": root,
        "args": [],
        "dyn_flags": dyn_flags,
        "tests": tests,
    }


TEST_SUITES: dict[SuiteName, Callable[[CliOpts], TestConfig]] = {  # type: ignore
    "kani": kani,
    "miri": miri,
    "custom": custom,
}
