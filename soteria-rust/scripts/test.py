#!/usr/bin/env python3


from io import TextIOWrapper
from pathlib import Path
import time
import datetime
from typing import Callable
import math

from common import *
from parselog import *

DynFlagFn = Optional[Callable[[Path], list[str]]]


class TestConfig(TypedDict):
    root: Path
    args: list[str]
    dyn_flags: DynFlagFn


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


def kani() -> tuple[list[Path], TestConfig]:
    root = Path(KANI_PATH)
    tests = [
        path
        for path in root.rglob("*.rs")
        if not any(exclusion in str(path) for exclusion in KANI_EXCLUSIONS)
    ]
    return tests, {
        "root": root,
        "args": ["--ignore-leaks", "--kani"],
        "dyn_flags": None,
    }


def miri() -> tuple[list[Path], TestConfig]:
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
    tests = [
        path
        for path in root.rglob("*.rs")
        if not any(exclusion in str(path) for exclusion in MIRI_EXCLUSIONS)
    ]
    return tests, {
        "root": root,
        "args": ["--miri"],
        "dyn_flags": dyn_flags,
    }


TEST_SUITES = {
    "kani": kani,
    "miri": miri,
}


# Execute a test, return the categorisation and the elapsed time
def exec_test(
    file: Path,
    *,
    log: Optional[TextIOWrapper] = None,
    args: list[str] = [],
    dyn_flags: Optional[Callable[[Path], list[str]]] = None,
) -> tuple[LogCategorisation, float]:
    expect_failure = determine_failure_expect(str(file))
    if dyn_flags:
        args = [*args, *dyn_flags(file)]

    if log:
        log.write(f"[TEST] Running {file} - {datetime.datetime.now()}:\n")

    before = time.time()
    cmd = ["soteria-rust", "rustc", str(file)] + args + ["--compact", "--no-color"]
    data = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
    )
    elapsed = time.time() - before

    full_log = f"{data.stderr}\n{data.stdout}\n\n"
    if log:
        log.write(full_log)

    outcome = categorise_rusteria(full_log, expect_failure=expect_failure)
    outcome = outcome[0] if isinstance(outcome, list) else outcome

    return outcome, elapsed


def build():
    pprint(f"Building {BOLD}Rusteria{RESET} ... ", flush=True)
    before = time.time()
    build_rusteria()
    elapsed = time.time() - before
    pprint(f"Took {elapsed:.3f}s to build", inc=True)


def filter_tests(tests: list[Path], flags: Flags):
    tests = [
        t
        for t in tests
        if (not any(e in str(t) for e in flags["exclusions"]))
        and (flags["filters"] == [] or all(f in str(t) for f in flags["filters"]))
    ]
    tests.sort()
    return tests


def exec_tests(tests: list[Path], test_conf: TestConfig, log: Path):
    build()
    flags = parse_flags(sys.argv[2:])

    args = test_conf["args"] + flags["cmd_flags"]

    tests = filter_tests(tests, flags)
    log.touch()
    log.write_text(f"Running {len(tests)} tests - {datetime.datetime.now()}:\n\n")
    pprint(f"{BOLD}Running {len(tests)} tests{RESET}", inc=True)

    oks = 0
    errs = 0
    charon = 0
    before = time.time()
    with log.open("a") as logfile:
        for path in tests:
            relative = path.relative_to(test_conf["root"])
            pprint(f"Running {relative} ... ", inc=True, end="", flush=True)

            if str(relative) in SKIPPED_TESTS:
                (msg, clr, reason) = SKIPPED_TESTS[str(relative)]
                print(
                    f"{clr}{msg}{RESET} {YELLOW}✦{RESET} {GRAY}{BOLD}Skipped{RESET}: {BOLD}{reason}{RESET}"
                )
            else:
                (msg, clr, reason), elapsed = exec_test(
                    path,
                    log=logfile,
                    args=args,
                    dyn_flags=test_conf["dyn_flags"],
                )
                txt = f"{clr}{msg}{RESET} in {elapsed:.3f}s"
                if elapsed > 1:
                    txt += " 🐌"
                if str(relative) in KNOWN_ISSUES:
                    issue = KNOWN_ISSUES[str(relative)]
                    txt += f" {YELLOW}✦{RESET} {BOLD}{issue}{RESET}"
                print(txt)

            if msg == "Success":
                oks += 1
            elif msg == "Failure":
                errs += 1
            elif "Charon" in msg:
                charon += 1
    elapsed = time.time() - before
    pprint(
        f"{BOLD}Finished in {elapsed:.3f}s{RESET}: {GREEN}{oks}{RESET}/{RED}{errs}{RESET}/{len(tests)} ({PURPLE}{charon}{RESET})"
    )


def evaluate_perf(tests: list[Path], test_conf: TestConfig):
    build()
    flags = parse_flags(sys.argv[3:])

    iters = flags["iterations"] or 5
    args = test_conf["args"] + flags["cmd_flags"]
    csv_suffix = f"{iters}-{int(time.time())}" if not flags["tag"] else flags["tag"]
    csv_file = PWD / f"eval-{csv_suffix}.csv"

    tests = filter_tests(tests, flags)

    pprint(f"{BOLD}Running {len(tests)} tests, {iters} times{RESET}", inc=True)

    test_times: dict[Path, list[float]] = {}
    before = time.time()
    for path in tests:
        relative = path.relative_to(test_conf["root"])
        txt = f"Running {relative} ..."
        if str(relative) in SKIPPED_TESTS:
            pprint(f"{txt} {GRAY}{BOLD}skipped", inc=True)
            continue

        times = test_times[relative] = []

        msg, clr = "", ""
        for i in range(iters):
            pprint(f"{txt} {i+1}/{iters}", end="\r")
            (msg, clr, _), t = exec_test(
                path,
                args=args,
                dyn_flags=test_conf["dyn_flags"],
            )
            if msg not in ["Success", "Failure"]:
                break
            times.append(t)
        if msg not in ["Success", "Failure"]:
            pprint(f"{txt} {clr}{msg}{RESET} {GRAY}(skipped){RESET}", inc=True)
            continue
        avg = sum(times) / iters
        pprint(f"{txt} {clr}{msg}{RESET} took {BOLD}{avg:.3f}s{RESET}", inc=True)

    elapsed = time.time() - before
    test_times_items = [*test_times.items()]
    test_times_items.sort(key=lambda v: sum(v[1]))
    total_average = (
        sum((t for v in test_times_items for t in v[1])) / len(test_times_items) / iters
    )

    pprint(
        f"{BOLD}Finished in {elapsed:.3f}s total, {total_average:.3f}s/iter{RESET}",
        inc=True,
    )
    pprint("", inc=True)
    rows: list[list[tuple[str, Optional[str]]]] = [
        [("File", BOLD), ("Avg", BOLD), ("Δ AllAvgs", BOLD), ("Std Dev", BOLD)]
    ]
    for test, times in test_times_items:
        if len(times) == 0:
            continue
        average = sum(times) / len(times)
        delta = average / total_average
        deviation = (
            math.sqrt((sum(pow(x - average, 2) for x in times)) / (iters - 1))
            if iters > 1
            else 0
        )

        color = RESET
        if delta > 1.5:
            color = ORANGE
            if delta > 2:
                color = RED
                if delta > 4:
                    color = f"{RED}{BOLD}"
        rows.append(
            [
                (str(test), None),
                (f"{average:.3f}s", color),
                (f"{delta:.2f}%", None),
                (f"{deviation:.3f}s", None),
            ]
        )
    csv_file.touch()
    with csv_file.open("w") as csv_io:
        csv_io.writelines(",".join(c[0] for c in row) + "\n" for row in rows)

    pptable(rows)


def diff_evaluation(path1: Path, path2: Path):
    def parse(path: Path) -> dict[str, float]:
        rows = {}
        with open(path, "r") as csv:
            next(csv)
            for row in csv:
                cells = row.split(",")
                rows[cells[0]] = float(cells[1][:-1])
        return rows

    csv1 = parse(path1)
    csv2 = parse(path2)
    files = set(csv1.keys())
    files = files.union(csv2.keys())
    csv1 = {f: t for f, t in csv1.items()}
    csv2 = {f: t for f, t in csv2.items()}
    total_time1 = sum(csv1.values())
    total_time2 = sum(csv2.values())
    delta_total = (total_time2 - total_time1) / total_time1 * 100
    diff_clr = RED if total_time1 < total_time2 else GREEN
    pprint(f"Diff of {path1} → {path2}", inc=True)
    pprint(
        f"Went from {total_time1:.3f}s to {total_time2:.3f}s ({diff_clr}{delta_total:.1f}%{RESET})"
    )

    rows: list[list[tuple[str, Optional[str]]]] = []
    for f in files:
        before = csv1[f]
        after = csv2[f]
        delta = (after - before) / before * 100
        color = RESET
        if delta > 5:
            color = YELLOW
            if delta > 10:
                color = ORANGE
                if delta > 30:
                    color = RED
        elif delta < -5:
            color = GREEN
            if delta < -10:
                color = CYAN
                if delta < -30:
                    color = BLUE
        rows.append(
            [
                (f, None),
                (f"{before:.3f}s", None),
                ("→", None),
                (f"{after:.3f}s", None),
                (f"{delta:.1f}%", color),
            ]
        )
    rows.sort(key=lambda r: float(r[4][0][:-1]))
    rows.insert(
        0, [("File", BOLD), ("Before", BOLD), ("", None), ("After", BOLD), ("Δ%", BOLD)]
    )
    pptable(rows)


class ArgError(Exception):
    def __init__(self, msg: str):
        super().__init__(msg)
        self.msg = msg

    def __str__(self):
        return self.msg


if __name__ == "__main__":
    try:
        if len(sys.argv) <= 1:
            raise ArgError("missing command")
        if sys.argv[1] in ["kani", "miri"]:
            tests, config = TEST_SUITES[sys.argv[1]]()
            log = PWD / f"{sys.argv[1]}.log"
            exec_tests(tests, config, log)
        elif sys.argv[1] == "eval":
            if len(sys.argv) <= 2:
                raise ArgError("missing test suite name: kani or miri")
            if sys.argv[2] not in ["kani", "miri"]:
                raise ArgError("invalid test suite name, expected kani or miri")
            tests, config = TEST_SUITES[sys.argv[2]]()
            evaluate_perf(tests, config)
        elif sys.argv[1] == "eval-diff":
            if len(sys.argv) <= 3:
                raise ArgError("missing paths to two evaluation CSV files")
            diff_evaluation(Path(sys.argv[2]), Path(sys.argv[3]))
        else:
            raise ArgError("Unknown command, expected kani, miri, eval or eval-diff")
    except ArgError as e:
        print(f"{RED}Error: {YELLOW}{e}")
    except KeyboardInterrupt:
        exit(1)
