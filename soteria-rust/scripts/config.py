from typing import Callable, Iterable, TypedDict, assert_never
from common import *
from cliopts import ArgError, CliOpts, SuiteName

DynFlagFn = Optional[Callable[[Path], list[str]]]


class TestConfig(TypedDict):
    name: str
    root: Path
    args: list[str]
    dyn_flags: DynFlagFn
    tests: list[Path]


KANI_PATH = (PWD / ".." / ".." / ".." / "kani" / "tests" / "kani").resolve()
KANI_EXCLUSIONS = [
    "/Stubbing/",
    "/SIMD/",
    "/Atomic/",
    "/ForeignItems/",
    "/Coroutines/",
    "/VolatileIntrinsics/",
    "/Volatile/",
    "/DynTrait/",
    "/AsyncAwait/",
    "/Quantifiers/",
    "/FunctionContracts/",
]

MIRI_PATH = (PWD / ".." / ".." / ".." / "miri" / "tests").resolve()
MIRI_EXCLUSIONS = [
    "/ui.rs",
    "/deps/",
    "/fail-dep/",
    "/pass-dep/",
    "/native-lib/",
    "/many-seeds/",
    "/utils/",
    "/stacked-borrows/",
    "/stacked_borrows/",
    "/concurrency/",
    "/data_race/",
    "simd",
    "async",
    "/shims/",
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
        args = ["-Zmiri-ignore-leaks"]
    elif opts["tool"] == "Miri":
        args = ["--test"]
    elif opts["tool"] == "Obol" or opts["tool"] == "Charon":
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
        # if file contains "-Zmiri-ignore-leaks", add "--ignore-leaks"
        if "-Zmiri-ignore-leaks" in file.read_text():
            flags.append("--ignore-leaks")
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
    elif opts["tool"] == "Obol" or opts["tool"] == "Charon":
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
    test_folder = opts["test_folder"]
    if test_folder is None:
        raise ArgError("No test folder specified, use --folder <path>")
    tests = filter_tests(opts, (path for path in test_folder.rglob("*.rs")))
    return {
        "name": "Custom",
        "root": test_folder,
        "args": [],
        "dyn_flags": lambda _: [],
        "tests": tests,
    }


TEST_SUITES: dict[SuiteName, Callable[[CliOpts], TestConfig]] = {
    "kani": kani,
    "miri": miri,
    "custom": custom,
}
