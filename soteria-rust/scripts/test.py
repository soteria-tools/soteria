#!/usr/bin/env python3


from io import TextIOWrapper
from pathlib import Path
import time
import datetime
from typing import assert_never
import math

from common import *
from parselog import (
    TestCategoriser,
    LogCategorisation_,
    parse_per_test,
)
from cliopts import (
    ArgError,
    CliOpts,
    SuiteName,
    opts_for_kani,
    opts_for_miri,
    opts_for_rusteria,
    parse_flags,
)
from config import TEST_SUITES, TestConfig, filter_tests


# Execute a test, return the categorisation and the elapsed time
def exec_test(
    file: Optional[Path],
    *,
    cmd: list[str],
    categoriser: TestCategoriser,
    test_conf: Optional[TestConfig] = None,
    tool: ToolName,
    log: Optional[TextIOWrapper] = None,
    timeout: Optional[int] = None,
    cwd: Optional[Path] = None,
) -> tuple[LogCategorisation_, float]:
    expect_failure = determine_failure_expect(str(file))
    if test_conf and file and test_conf["dyn_flags"]:
        cmd = cmd + test_conf["dyn_flags"](file)

    if log:
        log.write(f"[TEST] Running {file} - {datetime.datetime.now()}:\n")

    before = time.time()

    try:
        data = subprocess_run(
            cmd + ([str(file)] if file else []),
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=cwd,
        )
    except subprocess.TimeoutExpired:
        if log:
            log.write("Forced timeout\n")

        # Kani tends to leave a kani-compiler process behind
        if tool == "Kani":
            subprocess.run(["pkill", "-f", "kani-compiler"])
        if tool == "Rusteria":
            subprocess.run(["pkill", "-f", "z3"])

        return ((Outcome.TIME_OUT, None), timeout or 0)

    elapsed = time.time() - before

    full_log = f"{data.stderr}\n{data.stdout}\nCODE: {data.returncode}\n\n"
    if log:
        log.write(full_log)

    outcome = categoriser(full_log, expect_failure=expect_failure)
    outcome = outcome[0] if isinstance(outcome, list) else outcome

    return outcome, elapsed


_built = False


def build():
    global _built
    if _built:
        return
    _built = True
    pprint(f"Building {BOLD}Rusteria{RESET} ... ", flush=True, end="")
    before = time.time()
    build_rusteria()
    elapsed = time.time() - before
    print(f"took {BOLD}{elapsed:.3f}s{RESET}")


def exec_tests(opts: CliOpts, test_conf: TestConfig):
    build()
    cmd = opts["tool_cmd"] + test_conf["args"]
    tests = test_conf["tests"]

    log = PWD / f"{test_conf['name'].lower()}.log"
    log.touch()
    log.write_text(f"Running {len(tests)} tests - {datetime.datetime.now()}:\n\n")
    pprint(f"{BOLD}Running {len(tests)} tests{RESET}")

    interrupts = 0

    end_msgs: list[str] = []

    oks = 0
    errs = 0
    tool_errs = 0
    before = time.time()
    with log.open("a") as logfile:
        for path in tests:
            relative = path.relative_to(test_conf["root"])
            pprint(f"Running {relative} ... ", end="", flush=True)

            if str(relative) in SKIPPED_TESTS and not opts["no_skips"]:
                (outcome, reason) = SKIPPED_TESTS[str(relative)]
                msg = "" if reason is None else f": {BOLD}{reason}{RESET}"
                print(f"{outcome} {YELLOW}✦{RESET} {GRAY}{BOLD}Skipped{RESET}{msg}")
            else:
                try:
                    (outcome, reason), elapsed = exec_test(
                        path,
                        cmd=cmd,
                        log=logfile,
                        tool=opts["tool"],
                        categoriser=opts["categorise"],
                        test_conf=test_conf,
                        timeout=opts["timeout"],
                    )
                except KeyboardInterrupt as e:
                    interrupts += 1
                    print(
                        f" {ORANGE}✷{RESET} {BOLD}User interrupted{RESET} ({interrupts}/3)"
                    )
                    if interrupts >= 3:
                        raise e
                    continue

                txt = f"{outcome} in {elapsed:.3f}s"
                if elapsed > 1:
                    txt += " 🐌"
                if not outcome.is_expected() and reason:
                    txt += f" ({GRAY}{reason}{RESET})"
                if issue := dict_get_suffix(KNOWN_ISSUES, str(relative)):
                    txt += f" {YELLOW}✦{RESET} {BOLD}{issue}{RESET}"
                    if outcome.is_pass():
                        end_msgs.append(f'Fixed {relative}, for "{BOLD}{issue}{RESET}"')
                if issue := dict_get_suffix(SKIPPED_TESTS, str(relative)):
                    _, issue = issue
                    txt += f" {YELLOW}✦{RESET} {BOLD}(not skipped){RESET}"
                    if not outcome.is_timeout():
                        end_msgs.append(
                            f"Fixed timeout {relative} ({elapsed:.3f}s)"
                            + (
                                f", for {BOLD}{issue}{RESET}"
                                if issue is not None
                                else ""
                            )
                        )

                print(txt)

            if outcome.is_pass():
                oks += 1
            elif outcome.is_fail():
                errs += 1
            elif outcome.is_tool():
                tool_errs += 1
    elapsed = time.time() - before
    pprint(
        f"{BOLD}Finished in {elapsed:.3f}s{RESET}: {GREEN}{oks}{RESET}/{RED}{errs}{RESET}/{len(tests)} ({PURPLE}{tool_errs}{RESET})",
    )
    if len(end_msgs) > 0:
        pprint(f"{BOLD}Closing remarks:")
        for msg in end_msgs:
            pprint(f"{ORANGE}✭{RESET} {msg}")


def evaluate_perf(opts: CliOpts, iters: int, test_conf: TestConfig):
    build()

    tests = test_conf["tests"]
    cmd = opts["tool_cmd"] + test_conf["args"]
    csv_suffix = opts.get("tag") or f"{iters}-{int(time.time())}"
    csv_file = PWD / f"eval-{csv_suffix}.csv"

    pprint(f"{BOLD}Running {len(tests)} tests, {iters} times{RESET}")

    test_times: dict[Path, list[float]] = {}
    before = time.time()
    for path in tests:
        relative = path.relative_to(test_conf["root"])
        txt = f"Running {relative} ..."
        if str(relative) in SKIPPED_TESTS and not opts["no_skips"]:
            pprint(f"{txt} {GRAY}{BOLD}skipped")
            continue

        times = test_times[relative] = []

        outcome = Outcome.UNKNOWN
        for i in range(iters):
            pprint(f"{txt} {i+1}/{iters}", end="\r", inc=False)
            (outcome, _), t = exec_test(
                path,
                cmd=cmd,
                categoriser=opts["categorise"],
                test_conf=test_conf,
                tool=opts["tool"],
                timeout=opts["timeout"],
            )
            if not outcome.is_expected():
                break
            times.append(t)
        if not outcome.is_expected():
            pprint(f"{txt} {outcome} {GRAY}(skipped){RESET}")
            continue
        avg = sum(times) / iters
        pprint(f"{txt} {outcome} took {BOLD}{avg:.3f}s{RESET}")

    elapsed = time.time() - before
    test_times_items = [*test_times.items()]
    test_times_items.sort(key=lambda v: sum(v[1]))
    total_average = (
        sum((t for v in test_times_items for t in v[1])) / len(test_times_items) / iters
    )

    pprint(
        f"{BOLD}Finished in {elapsed:.3f}s total, {total_average:.3f}s/iter{RESET}",
    )
    pprint("")
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
    pprint(f"Diff of {path1} → {path2}")
    pprint(
        f"Went from {total_time1:.3f}s to {total_time2:.3f}s ({diff_clr}{delta_total:.1f}%{RESET})",
        inc=False,
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


Benchmark = dict[ToolName, tuple[Outcome, float]]


def benchmark(opts: CliOpts):
    build()

    log = PWD / "benchmark.log"
    log.touch()
    log.write_text(f"Running benchmark - {datetime.datetime.now()}:\n\n")

    log = log.open("a")

    results: dict[tuple[Path, SuiteName], Benchmark] = {}  # type: ignore
    end_msgs: set[str] = set()
    interrupts = 0
    timeout = opts["timeout"] or 5

    def run_benchmark(opts: CliOpts):
        for name, callback in TEST_SUITES.items():
            if name == "custom":
                continue
            test_conf = callback(opts)
            if len(test_conf["tests"]) == 0:
                continue
            cmd = opts["tool_cmd"] + test_conf["args"]
            pprint(
                f"{CYAN}{BOLD}==>{RESET} Running benchmark {BOLD}{test_conf['name']}{RESET} with {BOLD}{opts['tool']}",
            )
            log.write(f"Running benchmark {test_conf['name']} with {opts['tool']}\n\n")

            before = time.time()
            for path in test_conf["tests"]:
                relative = path.relative_to(test_conf["root"])
                pprint(f"Running {relative} ... ", end="", flush=True)
                try:
                    (outcome, _), elapsed = exec_test(
                        path,
                        cmd=cmd,
                        log=log,
                        categoriser=opts["categorise"],
                        test_conf=test_conf,
                        tool=opts["tool"],
                        timeout=timeout,
                    )
                except KeyboardInterrupt:
                    nonlocal interrupts
                    interrupts += 1
                    print(
                        f" {ORANGE}✷{RESET} {BOLD}User interrupted{RESET} ({interrupts}/3)"
                    )
                    if interrupts >= 3:
                        return
                    continue

                outcome = outcome.simplify()
                print(f"{outcome} in {elapsed:.3f}s")

                test_key = (relative, name)
                res = results.get(
                    test_key, {tool: (Outcome.UNKNOWN, -1) for tool in TOOL_NAMES}
                )
                res[opts["tool"]] = outcome, elapsed
                results[test_key] = res

            elapsed = time.time() - before
            pprint(
                f"{BOLD}Finished in {elapsed:.3f}s{RESET}",
            )

    pprint(f"{BOLD}Running benchmark{RESET}")
    try:
        run_benchmark(opts_for_rusteria(opts, force_obol=True, timeout=timeout))
        run_benchmark(opts_for_kani(opts, timeout=timeout))
        run_benchmark(opts_for_miri(opts))
    except:  # noqa E722: we make sure outputting results happens despite exit
        ...

    rows: list[list[tuple[str, Optional[str]]]] = [
        [
            ("Suite", BOLD),
            ("File", BOLD),
            ("Rusteria", BOLD),
            ("(s)", None),
            ("Kani", BOLD),
            ("(s)", None),
            ("Miri", BOLD),
            ("(s)", None),
        ]
    ]
    for (path, suite), res in results.items():
        row: list[tuple[str, Optional[str]]] = [(suite, BOLD), (str(path), BOLD)]
        for tool in TOOL_NAMES:
            outcome, elapsed = res[tool]
            row.append((outcome.txt, outcome.clr))
            row.append((f"{elapsed:.3f}", None) if elapsed >= 0 else ("", None))
        rows.append(row)

    csv_file = PWD / "benchmark.csv"
    csv_file.touch()
    with csv_file.open("w") as csv_io:
        csv_io.writelines(",".join(c[0] for c in row) + "\n" for row in rows)
    pprint()
    pptable(rows)

    if len(end_msgs) > 0:
        pprint(f"{BOLD}Closing remarks:")
        for end_msg in end_msgs:
            pprint(f"{ORANGE}✭{RESET} {end_msg}")


def kani_comparison(opts: CliOpts, path: Path, cached: bool):
    build()

    tests = filter_tests(opts, path.rglob("*.rs"))
    pprint(f"{BOLD}Running {len(tests)} tests{RESET}")

    interrupts = 0

    def run_with(opts: CliOpts, id: str) -> dict[str, tuple[Outcome, float]]:
        key = opts["tool"]
        log_path = PWD / f"kani-comparison-{id}.log"
        if cached:
            results = parse_per_test(log_path)
            results = {k: v[key] for k, v in results.items()}
            return results

        log_path.touch()
        log_path.write_text(
            f"Running {len(tests)} tests - {datetime.datetime.now()}:\n\n"
        )
        log = log_path.open("a")

        pprint(
            f"{CYAN}{BOLD}==>{RESET} Running with {BOLD}{key}{RESET} {GRAY}({id})",
        )
        log.write(f"Running tests with {key}\n\n")

        before = time.time()
        for test in tests:
            relative = test.relative_to(path)
            pprint(f"Running {relative} ... ", end="", flush=True)
            try:
                (outcome, _), elapsed = exec_test(
                    test,
                    cmd=opts["tool_cmd"],
                    log=log,
                    categoriser=opts["categorise"],
                    tool=key,
                    timeout=opts["timeout"],
                )
            except KeyboardInterrupt:
                nonlocal interrupts
                interrupts += 1
                print(
                    f" {ORANGE}✷{RESET} {BOLD}User interrupted{RESET} ({interrupts}/3)"
                )
                if interrupts >= 3:
                    break
                continue

            outcome = outcome.simplify()
            print(f"{outcome} in {elapsed:.3f}s")

        elapsed = time.time() - before
        pprint(
            f"{BOLD}Finished in {elapsed:.3f}s{RESET}",
        )
        log.close()

        results = parse_per_test(log_path)
        results = {k: v[key] for k, v in results.items()}
        return results

    def with_env(key: str, value: str):
        class WithEnv:
            def __enter__(self):
                self.prev = os.environ.get(key)
                os.environ[key] = value

            def __exit__(self, exc_type, exc_value, traceback):
                if self.prev is not None:
                    os.environ[key] = self.prev
                else:
                    del os.environ[key]

        return WithEnv()

    rusteria = opts_for_rusteria(opts, force_obol=True)
    rusteria["tool_cmd"] += ["--kani"]
    res_rusteria = run_with(rusteria, "rusteria")
    kani = opts_for_kani(opts)
    kani["tool_cmd"] = [
        f for f in kani["tool_cmd"] if not f.startswith("--harness-timeout=5s")
    ]
    kani["tool_cmd"] += ["--harness-timeout=10s"]
    res_kani = run_with(kani, "kani")

    with with_env("RUSTFLAGS", "--cfg unwind"):
        res_kani_unwind = run_with(kani, "kani-unwinding")

    table: list[list[tuple[str, Optional[str]]]] = []
    table += [
        [
            ("Suite", BOLD),
            ("Test", BOLD),
            ("Rusteria", BOLD),
            ("(s)", None),
            ("Kani", BOLD),
            ("(s)", None),
            ("Kani (unwinding)", BOLD),
            ("(s)", None),
        ]
    ]
    for test in res_rusteria.keys():
        r_out, r_time = res_rusteria.get(test, (Outcome.UNKNOWN, -2))
        k_out, k_time = res_kani.get(test, (Outcome.UNKNOWN, -2))
        ku_out, ku_time = res_kani_unwind.get(test, (Outcome.UNKNOWN, -2))

        if k_time == -1:
            k_time = 10
        if ku_time == -1:
            ku_time = 10

        suite, test = test.split("::")

        table.append(
            [
                (suite, BOLD),
                (test, None),
                (r_out.txt, r_out.clr),
                (f"{r_time:.3f}", None) if r_time >= 0 else ("-", GRAY),
                (k_out.txt, k_out.clr),
                (f"{k_time:.3f}", None) if k_time >= 0 else ("-", GRAY),
                (ku_out.txt, ku_out.clr),
                (f"{ku_time:.3f}", None) if ku_time >= 0 else ("-", GRAY),
            ]
        )

    csv_file = PWD / "kani-comparison.csv"
    csv_file.touch()
    with csv_file.open("w") as csv_io:
        csv_io.writelines(",".join(c[0] for c in row) + "\n" for row in table)
    pptable(table)


def finetime(opts: CliOpts):
    build()

    finetime = (PWD / ".." / ".." / ".." / "finetime").resolve()

    # to find the tests, we run the crate with a 0 step fuel
    def get_tests() -> list[str]:
        ropts = opts_for_rusteria(opts, force_obol=True, timeout=None)
        tool_cmd = ropts["tool_cmd"].copy()
        tool_cmd[1] = "cargo"
        data = subprocess_run(
            tool_cmd + ["--kani", "--step-fuel=0", str(finetime)],
            capture_output=True,
            text=True,
        )
        tests: list[str] = []
        for line in data.stdout.splitlines():
            # look for
            # warning: <test> (0.01ms): runtime error, Missed 1 branches
            if line.startswith("warning: "):
                tests.append(line.split(" ")[1])
        return tests

    tests = get_tests()
    pprint(f"{BOLD}Running {len(tests)} tests{RESET}")

    interrupts = 0

    def run_with_rusteria() -> dict[str, tuple[Outcome, float]]:
        ropts = opts_for_rusteria(opts, force_obol=True, timeout=None)
        tool_cmd = ropts["tool_cmd"].copy()
        tool_cmd[1] = "cargo"
        tool_cmd += ["--kani", "--no-compile"]
        log_path = PWD / "finetime-comparison-rusteria.log"
        log_path.touch()
        log_path.write_text(
            f"Running {len(tests)} tests - {datetime.datetime.now()}:\n\n"
        )
        log = log_path.open("a")

        pprint(
            f"{CYAN}{BOLD}==>{RESET} Running with {BOLD}Rusteria{RESET}",
        )
        log.write("Running tests with Rusteria\n\n")

        before = time.time()
        for test in tests:
            pprint(f"Running {test} ... ", end="", flush=True)
            try:
                filter = ["--filter", test]
                (outcome, _), elapsed = exec_test(
                    finetime,
                    cmd=tool_cmd + filter,
                    log=log,
                    categoriser=ropts["categorise"],
                    tool="Rusteria",
                    timeout=ropts["timeout"],
                )
            except KeyboardInterrupt:
                nonlocal interrupts
                interrupts += 1
                print(
                    f" {ORANGE}✷{RESET} {BOLD}User interrupted{RESET} ({interrupts}/3)"
                )
                if interrupts >= 3:
                    break
                continue

            outcome = outcome.simplify()
            print(f"{outcome} in {elapsed:.3f}s")

        elapsed = time.time() - before
        pprint(
            f"{BOLD}Finished in {elapsed:.3f}s{RESET}",
        )
        log.close()

        results = parse_per_test(log_path)
        results = {
            (k if "finetime::" not in k else k.replace("finetime::", "")): v["Rusteria"]
            for k, v in results.items()
        }
        return results

    def run_with_kani(*, cached=False) -> dict[str, tuple[Outcome, float]]:
        kopts = opts_for_kani(opts, timeout=None)
        kopts["tool_cmd"] = ["cargo", "kani"] + kopts["tool_cmd"][1:]
        log_path = PWD / "finetime-comparison-kani.log"

        if cached:
            results = parse_per_test(log_path)
            results = {
                (k if not k.startswith("None::") else k.replace("None::", "")): v[
                    "Kani"
                ]
                for k, v in results.items()
            }
            return results

        log_path.touch()
        log_path.write_text(
            f"Running {len(tests)} tests - {datetime.datetime.now()}:\n\n"
        )
        log = log_path.open("a")

        pprint(
            f"{CYAN}{BOLD}==>{RESET} Running with {BOLD}Kani",
        )
        log.write("Running tests with Kani\n\n")

        before = time.time()

        pprint("Running bulk ... ", end="", flush=True)
        try:
            (outcome, _), elapsed = exec_test(
                None,
                cmd=kopts["tool_cmd"],
                log=log,
                categoriser=kopts["categorise"],
                tool="Kani",
                timeout=kopts["timeout"],
                cwd=finetime,
            )
        except KeyboardInterrupt:
            nonlocal interrupts
            print(f" {ORANGE}✷{RESET} {BOLD}User interrupted{RESET} ({interrupts}/3)")
            return {}

        outcome = outcome.simplify()
        print(f"{outcome} in {elapsed:.3f}s")

        elapsed = time.time() - before
        pprint(
            f"{BOLD}Finished in {elapsed:.3f}s{RESET}",
        )
        log.close()

        results = parse_per_test(log_path)
        results = {k: v["Kani"] for k, v in results.items()}
        return results

    res_rusteria = run_with_rusteria()
    res_kani = run_with_kani(cached=True)

    table: list[list[tuple[str, Optional[str]]]] = []
    table += [
        [
            ("Suite", BOLD),
            ("Test", BOLD),
            ("Rusteria", BOLD),
            ("(s)", None),
            ("Kani", BOLD),
            ("(s)", None),
        ]
    ]
    keys = list(set(res_rusteria.keys()).union(set(res_kani.keys())))
    keys.sort()
    for test in keys:
        r_out, r_time = res_rusteria.get(test, (Outcome.UNKNOWN, -2))
        k_out, k_time = res_kani.get(test, (Outcome.UNKNOWN, -2))

        if k_time == -1:
            k_time = 10
        suite, test_name = test.split("::", 1)

        table.append(
            [
                (suite, BOLD),
                (test_name, None),
                (r_out.txt, r_out.clr),
                (f"{r_time:.3f}", None) if r_time >= 0 else ("-", GRAY),
                (k_out.txt, k_out.clr),
                (f"{k_time:.3f}", None) if k_time >= 0 else ("-", GRAY),
            ]
        )

    csv_file = PWD / "finetime-comparison.csv"
    csv_file.touch()
    with csv_file.open("w") as csv_io:
        csv_io.writelines(",".join(c[0] for c in row) + "\n" for row in table)
    pptable(table)


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
            if len(config["tests"]) == 0:
                continue
            pprint(f"Running {BOLD}{name}{RESET} tests")
            exec_tests(opts, config)
    elif cmd[0] == "eval":
        (suite, iterations) = cmd[1]
        config = TEST_SUITES[suite](opts)
        evaluate_perf(opts, iterations, config)
    elif cmd[0] == "eval-diff":
        (file1, file2) = cmd[1]
        diff_evaluation(file1, file2)
    elif cmd[0] == "benchmark":
        benchmark(opts)
    elif cmd[0] == "comp-kani":
        (compare_path, cached) = cmd[1]
        kani_comparison(opts, compare_path, cached)
    elif cmd[0] == "finetime":
        finetime(opts)
    else:
        assert_never(cmd)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pprint(f"{BOLD}User interrupted script -- exiting 👋")
        exit(1)
