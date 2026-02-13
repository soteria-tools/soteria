from typing import Callable, Iterable, TypedDict, assert_never

from cliopts import ArgError, CliOpts, SuiteName
from common import *
from test_exclusions import KANI_EXCLUSIONS, MIRI_EXCLUSIONS

DynFlagFn = Optional[Callable[[Path], list[str]]]


class TestConfig(TypedDict):
    name: str
    root: Path
    args: list[str]
    dyn_flags: DynFlagFn
    tests: list[Path]


KANI_PATH = (PWD / ".." / ".." / ".." / "kani" / "tests" / "kani").resolve()
MIRI_PATH = (PWD / ".." / ".." / ".." / "miri" / "tests").resolve()


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
            if "-Zmiri-recursive-validation" in config:
                flags.append("--recursive-validity")
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
