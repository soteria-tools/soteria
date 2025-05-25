#!/usr/bin/env python3


from ast import expr
from io import TextIOWrapper
from pathlib import Path
import time
import datetime
from types import FunctionType
from typing import Callable

from common import *
from parselog import *


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


def exec_test(
    file: Path,
    *,
    root: Path,
    log: TextIOWrapper,
    args: list[str] = [],
    dyn_flags: Optional[Callable[[Path], list[str]]] = None,
):
    relative = file.relative_to(root)
    expect_failure = determine_failure_expect(str(file))

    if dyn_flags:
        args = [*args, *dyn_flags(file)]

    pprint(f"Running {relative} ... ", inc=True, end="", flush=True)
    log.write(f"[TEST] Running {file} - {datetime.datetime.now()}:\n")

    before = time.time()
    cmd = f"soteria-rust exec-main {' '.join(args)} {file}"
    data = subprocess.run(
        cmd,
        capture_output=True,
        shell=True,
        text=True,
    )
    elapsed = time.time() - before

    full_log = f"{data.stderr}\n{data.stdout}\n\n"
    log.write(full_log)

    outcome = categorise_rusteria(full_log, expect_failure=expect_failure)
    (msg, clr, reason) = outcome = outcome[0] if isinstance(outcome, list) else outcome
    if str(relative) in KNOWN_ISSUES:
        issue = KNOWN_ISSUES[str(relative)]
        print(
            f"{clr}{msg}{RESET} in {elapsed:.3f}s {YELLOW}✦{RESET} {BOLD}{issue}{RESET}"
        )
    else:
        print(f"{clr}{msg}{RESET} in {elapsed:.3f}s")
    return outcome


def exec_tests(tests: list[Path], *, log: Path, **kwargs):
    pprint(f"Building {BOLD}Rusteria{RESET} ... ", inc=True, end="")
    before = time.time()
    build_rusteria()
    elapsed = time.time() - before
    print(f"Took {elapsed:.3f}s to build")

    flags = parse_flags()

    args = kwargs.get("args", []) + flags["cmd_flags"]
    kwargs["args"] = args

    tests = [
        t
        for t in tests
        if (not any(e in str(t) for e in flags["exclusions"]))
        and (flags["filters"] == [] or all(f in str(t) for f in flags["filters"]))
    ]
    tests.sort()
    log.touch()
    log.write_text(f"Running {len(tests)} tests - {datetime.datetime.now()}:\n\n")
    pprint(f"{BOLD}Running {len(tests)} tests{RESET}", inc=True)

    oks = 0
    errs = 0
    before = time.time()
    with log.open("a") as logfile:
        for path in tests:
            msg, _, _ = exec_test(path, log=logfile, **kwargs)
            if msg == "Success":
                oks += 1
            elif msg == "Failure":
                errs += 1
    elapsed = time.time() - before
    pprint(
        f"{BOLD}Finished in {elapsed:.2f}s{RESET}: {GREEN}{oks}{RESET}/{RED}{errs}{RESET}/{len(tests)}"
    )


def kani():
    root = Path(KANI_PATH)
    log = PWD / "kani.log"
    tests = [
        path
        for path in root.rglob("*.rs")
        if not any(exclusion in str(path) for exclusion in KANI_EXCLUSIONS)
    ]
    exec_tests(tests, root=root, log=log, args=["--ignore-leaks", "--kani"])


def miri():
    def dyn_flags(file: Path) -> list[str]:
        flags = []
        # if file contains "-Zmiri-ignore-leaks", add "--ignore-leaks"
        if "-Zmiri-ignore-leaks" in file.read_text():
            flags.append("--ignore-leaks")
        return flags

    root = Path(MIRI_PATH)
    log = PWD / "miri.log"
    tests = [
        path
        for path in root.rglob("*.rs")
        if not any(exclusion in str(path) for exclusion in MIRI_EXCLUSIONS)
    ]
    exec_tests(tests, root=root, log=log, args=["--miri"], dyn_flags=dyn_flags)


if __name__ == "__main__":
    try:
        if len(sys.argv) <= 1:
            raise RuntimeError
        if sys.argv[1] == "kani":
            kani()
        elif sys.argv[1] == "miri":
            miri()
        else:
            raise RuntimeError
    except RuntimeError:
        print(
            f"{RED}Invalid command -- specify {YELLOW}kani{RED} or "
            f"{YELLOW}mir{RED} as a first argument."
        )
    except KeyboardInterrupt:
        exit(1)
