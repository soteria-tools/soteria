#!/usr/bin/env python3


from io import TextIOWrapper
from pathlib import Path
import time
import datetime
from typing import Callable, Literal
import math

from common import *
from parselog import (
    TestCategoriser,
    LogCategorisation_,
)
from cliopts import ArgError, CliOpts, parse_flags
from config import TEST_SUITES, TestConfig


# Execute a test, return the categorisation and the elapsed time
def exec_test(
    file: Path,
    *,
    cmd: list[str],
    categoriser: TestCategoriser,
    log: Optional[TextIOWrapper] = None,
    dyn_flags: Optional[Callable[[Path], list[str]]] = None,
) -> tuple[LogCategorisation_, float]:
    expect_failure = determine_failure_expect(str(file))
    if dyn_flags:
        cmd = cmd + dyn_flags(file)

    if log:
        log.write(f"[TEST] Running {file} - {datetime.datetime.now()}:\n")

    before = time.time()
    data = subprocess.run(
        cmd + [str(file)],
        capture_output=True,
        text=True,
    )
    elapsed = time.time() - before

    full_log = f"{data.stderr}\n{data.stdout}\n\n"
    if log:
        log.write(full_log)

    outcome = categoriser(full_log, expect_failure=expect_failure)
    outcome = outcome[0] if isinstance(outcome, list) else outcome

    return outcome, elapsed


def build():
    pprint(f"Building {BOLD}Rusteria{RESET} ... ", flush=True)
    before = time.time()
    build_rusteria()
    elapsed = time.time() - before
    pprint(f"Took {elapsed:.3f}s to build", inc=True)


def exec_tests(opts: CliOpts, test_conf: TestConfig):
    build()
    cmd = opts["tool_cmd"] + test_conf["args"]
    tests = test_conf["tests"]

    log = PWD / f"{test_conf['name'].lower()}.log"
    log.touch()
    log.write_text(f"Running {len(tests)} tests - {datetime.datetime.now()}:\n\n")
    pprint(f"{BOLD}Running {len(tests)} tests{RESET}", inc=True)

    interrupts = 0

    end_msgs: list[str] = []

    oks = 0
    errs = 0
    tool_errs = 0
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
                try:
                    (msg, clr, reason), elapsed = exec_test(
                        path,
                        cmd=cmd,
                        log=logfile,
                        categoriser=opts["categorise"],
                        dyn_flags=test_conf["dyn_flags"],
                    )
                except KeyboardInterrupt as e:
                    interrupts += 1
                    print(
                        f" {ORANGE}✷{RESET} {BOLD}User interrupted{RESET} ({interrupts}/3)"
                    )
                    if interrupts >= 3:
                        raise e
                    continue

                msg = msg.replace("Tool", opts["tool"])
                txt = f"{clr}{msg}{RESET} in {elapsed:.3f}s"
                if elapsed > 1:
                    txt += " 🐌"
                if str(relative) in KNOWN_ISSUES:
                    issue = KNOWN_ISSUES[str(relative)]
                    txt += f" {YELLOW}✦{RESET} {BOLD}{issue}{RESET}"
                    if msg == "Success":
                        end_msgs.append(f'Fixed {relative}, for "{BOLD}{issue}{RESET}"')
                print(txt)

            if msg == "Success":
                oks += 1
            elif msg == "Failure":
                errs += 1
            elif opts["tool"] in msg:
                tool_errs += 1
    elapsed = time.time() - before
    pprint(
        f"{BOLD}Finished in {elapsed:.3f}s{RESET}: {GREEN}{oks}{RESET}/{RED}{errs}{RESET}/{len(tests)} ({PURPLE}{tool_errs}{RESET})",
        inc=True,
    )
    if len(end_msgs) > 0:
        pprint(f"{BOLD}Closing remarks:", inc=True)
    for msg in end_msgs:
        pprint(f"{ORANGE}✭{RESET} {msg}", inc=True)


def evaluate_perf(opts: CliOpts, iters: int, test_conf: TestConfig):
    build()

    tests = test_conf["tests"]
    cmd = opts["tool_cmd"] + test_conf["args"]
    csv_suffix = opts.get("tag") or f"{iters}-{int(time.time())}"
    csv_file = PWD / f"eval-{csv_suffix}.csv"

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
                cmd=cmd,
                categoriser=opts["categorise"],
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


def main():
    try:
        opts = parse_flags()
    except ArgError as e:
        print(f"{RED}Error: {YELLOW}{e}")
        exit(1)

    cmd = opts["cmd"]
    if cmd[0] == "exec":
        (suite,) = cmd[1]
        config = TEST_SUITES[suite](opts)
        exec_tests(opts, config)
    elif cmd[0] == "all":
        for name, callback in TEST_SUITES.items():
            if name == "custom":
                continue
            config = callback(opts)
            pprint(f"Running {BOLD}{name}{RESET} tests", inc=True)
            exec_tests(opts, config)
    elif cmd[0] == "eval":
        (suite, iterations) = cmd[1]
        config = TEST_SUITES[suite](opts)
        evaluate_perf(opts, iterations, config)
    elif cmd[0] == "eval-diff":
        (file1, file2) = cmd[1]
        diff_evaluation(file1, file2)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pprint(f"{BOLD}User interrupted script -- exiting 👋", inc=True)
        exit(1)
