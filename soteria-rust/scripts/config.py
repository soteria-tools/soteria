from typing import Callable, Iterable
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
]

MIRI_PATH = (PWD / ".." / ".." / ".." / "miri" / "tests").resolve()
MIRI_EXCLUSIONS = [
    "/ui.rs",
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


def kani(opts: CliOpts) -> TestConfig:
    root = Path(KANI_PATH)
    tests = filter_tests(
        opts,
        (
            path
            for path in root.rglob("*.rs")
            if not any(exclusion in str(path) for exclusion in KANI_EXCLUSIONS)
        ),
    )
    args = [] if opts["tool"] == "Kani" else ["--ignore-leaks", "--kani"]
    return {
        "name": "Kani",
        "root": root,
        "args": args,
        "dyn_flags": None,
        "tests": tests,
    }


def miri(opts: CliOpts) -> TestConfig:
    dyn_flag_cache: dict[Path, list[str]] = {}

    def dyn_flags(file: Path) -> list[str]:
        if file in dyn_flag_cache:
            return dyn_flag_cache[file]
        flags = []
        # if file contains "-Zmiri-ignore-leaks", add "--ignore-leaks"
        if "-Zmiri-ignore-leaks" in file.read_text():
            flags.append("--ignore-leaks")
        dyn_flag_cache[file] = flags
        return flags

    root = Path(MIRI_PATH)
    tests = filter_tests(
        opts,
        (
            path
            for path in root.rglob("*.rs")
            if not any(exclusion in str(path) for exclusion in MIRI_EXCLUSIONS)
        ),
    )
    args = [] if opts["tool"] == "Kani" else ["--miri"]
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
