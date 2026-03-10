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

T = TypeVar("T")


def filter_tests(opts: CliOpts, tests: Iterable[T]) -> list[T]:
    exclusions = opts["exclusions"]
    filters = opts["filters"]
    tests = sorted(
        (
            t
            for t in tests
            if (not any(e in str(t) for e in exclusions))
            and (filters == [] or all(f in str(t) for f in filters))
        ),
        key=lambda t: str(t).lower(),
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

        config = config.split() if config else []
        if opts["tool"] == "Kani":
            return config

        compile_flags = get_config_line(file, "// compile-flags:")
        compile_flags = compile_flags.split() if compile_flags else []

        flags = []
        kani_flags = config + compile_flags
        while kani_flags:
            kani_flag = kani_flags.pop(0).strip()
            if kani_flag in [
                "--no-memory-safety-checks",
                "--no-restrict-vtable",
                "--no-undefined-function-checks",
                "--no-unwinding-checks",
                "--no-default-checks",
                "--extra-pointer-checks",
                "--no-overflow-checks",
            ]:
                continue
            if kani_flag.startswith("-Zmir-opt-level") or kani_flag.startswith(
                "--randomize-layout"
            ):
                continue
            if kani_flag == "--default-unwind":
                kani_flags.pop(0)
                continue
            if kani_flag == "--edition":
                if opts["tool"] == "Miri":
                    flags += ["--edition", kani_flags.pop(0)]
                else:
                    flags += ["--rustc", '"--edition=' + kani_flags.pop(0) + '"']
                continue
            if kani_flag == "-Z":
                unstable_flag = kani_flags.pop(0).strip()
                if unstable_flag in [
                    "restrict-vtable",
                    "unstable-options",
                    "uninit-checks",
                ]:
                    continue
                raise ArgError(
                    f"Unhandled unstable flag for Kani test {file}: -Z {unstable_flag}\n"
                )

            raise ArgError(
                f"Unhandled dynamic flags for Kani test {file}: {kani_flag}\n"
            )

        return flags

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
        args = ["--test", "-Zmiri-ignore-leaks", "-Zmiri-disable-stacked-borrows"]
    elif opts["tool"] == "Soteria":
        args = ["--ignore-leaks", "--kani", "--ignore-aliasing"]
    else:
        assert_never(opts["tool"])

    return {
        "name": "Kani",
        "root": root,
        "args": args,
        "dyn_flags": kani_dyn_flags,
        "tests": tests,
    }


def miri(opts: CliOpts) -> TestConfig:
    @with_cache
    def soteria_dyn_flags(file: Path) -> list[str]:
        flags = []
        config = get_config_line(file, "//@compile-flags:")
        config = config.split() if config else []
        while config:
            miri_flag = config.pop(0).strip()

            if miri_flag == "-C":
                compile_flag = config.pop(0).strip()
                miri_flag = f"-C{compile_flag}"

            if miri_flag in [
                # We don't support disabling validation, as Miri's support for it is
                # entirely dependent on it's architecture and hard to maintain for our tool
                "-Zmiri-disable-validation",
                # We don't support disabling alignment checks
                "-Zmiri-disable-alignment-check",
                # We already do align checks symbolically!
                "-Zmiri-symbolic-alignment-check",
                # We always use tree borrows!
                "-Zmiri-tree-borrows",
                # We do not support precise interior mutability, so disabling it would not make a difference
                "-Zmiri-tree-borrows-no-precise-interior-mut",
                # We don't allow disabling isolation
                "-Zmiri-disable-isolation",
                # We don't allow disabling overflow checks
                "-Coverflow-checks=off",
                # Miri-specific optimisation flags
                "-Zmiri-provenance-gc=0",
                "-Zmiri-disable-leak-backtraces",
                "-Zmiri-track-alloc-accesses",
            ]:
                continue

            # Compilation flags we don't support
            if any(
                miri_flag.startswith(prefix)
                for prefix in [
                    "-Zmir-opt-level",
                    "-Zinline-mir",
                    "-Zinline-mir-hint-threshold",
                    "-Zmiri-track-alloc-id",
                    "-Zmiri-mute-stdout-stderr",
                    "-Zoom=panic",
                    "-Cpanic",
                    "-Cdebug-assertions",
                ]
            ):
                continue

            if "-Zmiri-ignore-leaks" == miri_flag:
                flags.append("--ignore-leaks")
                continue
            if "-Zmiri-strict-provenance" == miri_flag:
                flags.append("--provenance=strict")
                continue
            if "-Zmiri-permissive-provenance" == miri_flag:
                flags.append("--provenance=permissive")
                continue
            if "-Zmiri-disable-stacked-borrows" == miri_flag:
                flags.append("--ignore-aliasing")
                continue
            if "-Zmiri-recursive-validation" == miri_flag:
                flags.append("--recursive-validity=deny")
                continue

            raise ArgError(
                f"Unhandled dynamic flags for Miri test {file}: {miri_flag}\n"
            )

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
    elif opts["tool"] == "Soteria":
        args = ["--miri"]
        dyn_flags = soteria_dyn_flags
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
