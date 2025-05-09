#!/usr/bin/env python3

import sys
import re
from typing import Iterable, Optional

PURPLE = "\033[0;35m"
RED = "\033[0;31m"
ORANGE = "\033[38;5;208m"
YELLOW = "\033[38;5;220m"
GREEN = "\033[0;32m"
CYAN = "\033[0;36m"
BLUE = "\033[0;34m"
GRAY = "\033[0;90m"
BOLD = "\033[1m"
RESET = "\033[0m"

# if piping output, remove colors:
NO_COLOR = not sys.stdout.isatty()
if NO_COLOR:
    PURPLE = RED = ORANGE = YELLOW = GREEN = CYAN = BLUE = GRAY = BOLD = RESET = ""


def rainbow(i):
    if NO_COLOR:
        return ""

    i = i % 7
    return [
        "\033[38;5;197m",
        "\033[38;5;208m",
        "\033[38;5;220m",
        "\033[38;5;70m",
        "\033[38;5;74m",
        "\033[38;5;33m",
        "\033[38;5;127m",
    ][i]


known_issue = {
    "tests/kani/ArithOperators/unsafe_add_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "tests/kani/ArithOperators/unsafe_mul_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "tests/kani/ArithOperators/unsafe_sub_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "kani/Intrinsics/Compiler/variant_count.rs": "Kani doesn't handle variant_count yet -- we do!",
    "tests/kani/LayoutRandomization/should_fail.rs": "We don't handle layout randomization yet",
    "kani/Uninit/access-padding-enum-diverging-variants.rs": "Kani can't handle variants with different paddings",
    "kani/Uninit/access-padding-enum-multiple-variants.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "kani/Uninit/access-padding-enum-single-field.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "kani/Uninit/access-padding-enum-single-variant.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "kani/ValidValues/write_bytes.rs": "Kani checks for validity on write, whereas Miri does on read; we copy Miri.",
    "pass/integer-ops.rs": "Miri allows negative bit shifts, we don't (like Kani)",
    "pass/disable-alignment-check.rs": "We don't provide a way to disable alignment checks",
}


def file_str(file_name: str):
    issue = known_issue.get(file_name, None)
    if issue:
        return f"{GRAY}{file_name} {YELLOW}✦{RESET} {BOLD}{issue}{RESET}"
    return file_name


# { (result, color) -> {(test, specific reason?)} }
LogInfo = dict[tuple[str, str], set[tuple[str, Optional[str]]]]


def analyse(file: str) -> LogInfo:
    file_filters = [arg[3:] for arg in sys.argv if arg.startswith("-f=")]

    stats: LogInfo = {}

    def log(test: str, cause: str, color: str, reason: Optional[str] = None):
        key = (cause, color)
        if key not in stats:
            stats[key] = set()
        stats[key].add((test, reason))

    content = open(file, "r").read()
    tests = content.split("\nRunning ")
    print(f"• Found {len(tests)} tests in {file}")
    tests[0] = tests[0].replace("Running ", "")
    for test in tests:
        file_path = re.search(r"(.+)\n", test)
        if not file_path:
            exit(f"No file found in {test}")
        file_path = file_path.group(1)
        if len(file_filters) and not any(
            [filter in file_path for filter in file_filters]
        ):
            continue

        expect_failure = False
        if "kani" in file_path:
            try:
                with open(file_path, "r") as f:
                    content = f.read()
                    # this only holds for kani!
                    expect_failure = "kani-verify-fail" in content
            except:
                ...
        elif "miri" in file_path:
            expect_failure = "/fail/" in file_path or "/panic/" in file_path

        tests_idx = file_path.split("/").index("tests") + 1
        file_name = "/".join(file_path.split("/")[tests_idx:])

        if "Fatal (Charon)" in test:
            # this isn't Charon's fault, really
            unresolved = re.findall(
                r"failed to resolve: could not find `(.+)` in `(.+)`", test
            )
            if unresolved:
                for fn, crate in unresolved:
                    log(file_name, "Missing dependency", ORANGE, f"{crate}::{fn}")
                continue

            compile_errors = re.findall(r"error(\[E\d+\]: .+)\n", test)
            compile_errors = [
                # these are Hax/Charon errors!
                err
                for err in compile_errors
                if not err.startswith("[E9999]")
            ]
            if compile_errors:
                for error in compile_errors:
                    log(file_name, "Compilation error", ORANGE, error)
                continue

            if "cannot find macro" in test:
                err = re.search(r"(cannot find macro.*)\n", test)
                if err is not None:
                    log(file_path, "Compilation error", ORANGE, f"{err}")
                    continue

            sub_errors = []

            err_labels = re.findall(r'The label is "(.+)"', test)
            sub_errors = sub_errors + err_labels

            panics = re.findall(r"thread \'.+\' panicked at (.+):", test)
            sub_errors = sub_errors + [
                (
                    f"Hax panicked: {err}"
                    if "frontend/exporter" in err
                    else f"Panic: {err}"
                )
                for err in panics
            ]

            if "Unexpected trait reference kind" in test:
                sub_errors.append("Unexpected trait reference kind")
            if "Cannot convert constant back to an expression" in test:
                sub_errors.append("Cannot convert constant back to an expression")
            if "Charon__GAstOfJson.gtranslated_crate_of_json" in test:
                sub_errors.append("Parsing ULLBC from JSON")

            if len(sub_errors) > 0:
                for reason in sub_errors:
                    log(file_name, "Charon", PURPLE, reason)
            else:
                log(file_name, "Charon", PURPLE)
            continue

        if "Fatal: No entry points found" in test:
            log(file_name, "No entry points found", RED)
            continue

        if "resolve_constant (Generated_Expressions.COpaque" in test:
            log(file_name, "Charon", PURPLE, "Constant resolving")
            continue

        if "MISSING FEATURE, VANISHING" in test:
            cause = re.search(r"MISSING FEATURE, VANISHING: (.+)\n", test)
            if not cause:
                exit(f"No cause found for vanish in {test}")
            cause = cause.group(1)
            color = YELLOW
            reason = None

            if "Unsupported intrinsic" in cause and not "--intrinsics" in sys.argv:
                reason = cause.replace("Unsupported intrinsic: ", "")
                cause = "Unsupported intrinsic"
                color = ORANGE

            if "not found in store" in cause:
                cause = "Variable not found in store"
            if "Unsupported cast kind" in cause:
                cause = "Unsupported cast kind"
            if "Unhandled transmute" in cause:
                cause = "Unhandled transmute"
            if "is opaque" in cause:
                reason = cause.replace("Function ", "").replace(" is opaque", "")
                cause = "Opaque function - Charon?"
                color = PURPLE
            if "Splitting " in cause:
                cause = "Splitting value"

            log(file_name, cause, color, reason)
            continue

        if "Done." in test:
            if not expect_failure:
                log(file_name, "Success", GREEN, "Expected success, got success")
            else:
                log(file_name, "Failure", RED, "Expected success, got failure")
            continue

        err_re = re.compile(r"Error in (\d+) branch")
        if err_re.search(test):
            if expect_failure:
                log(file_name, "Success", GREEN, "Expected failure, got failure")
            else:
                log(file_name, "Failure", RED, "Expected failure, got success")
            continue

        if "Fatal: Exn: Failure" in test:
            cause = re.search(r"Fatal: Exn: Failure\(\"(.+)\"\)", test)
            if not cause:
                exit(f"No cause found for fatal exn in {test}")
            log(file_name, "Raised exception", RED, cause.group(1))
            continue

        if "Fatal: Exn" in test:
            cause = re.search(r"Fatal: Exn: (.+)", test)
            if not cause:
                exit(f"No cause found for fatal exn in {test}")
            log(file_name, "Raised exception", RED, cause.group(1))
            continue

        if "Fatal: Execution vanished" in test:
            log(file_name, "Vanished", RED)
            continue

        log(file_name, f"Unknown (Fatal error)", RED)
    return stats


def merge(logs: Iterable[LogInfo]) -> LogInfo:
    ret: LogInfo = {}
    for log in logs:
        for key, tests in log.items():
            if key not in ret:
                ret[key] = set()
            ret[key] = ret[key].union(tests)
    return ret


# List equivalent of LogInfo:
# [( cause, color, test_num, {(test, specific reason?)} )]
LogInfoList = list[tuple[str, str, int, set[tuple[str, Optional[str]]]]]


# parses a LogInfo into a list TestItems, applying the required filtering and sorting
def as_items(log: LogInfo) -> LogInfoList:
    cause_filters = [arg[3:] for arg in sys.argv if arg.startswith("-F=")]
    alpha_sort = "--az" in sys.argv
    rev_sort = "--rev" in sys.argv

    items = [
        (cause, color, len(set(test[0] for test in tests)), tests)
        for (cause, color), tests in log.items()
        if (len(cause_filters) == 0 or any(filter in cause for filter in cause_filters))
    ]

    if alpha_sort:
        items.sort(key=lambda x: x[0])
    else:
        items.sort(key=lambda x: -x[2])

    if rev_sort:
        items.reverse()

    return items


# Reverse of LogInfo: mapping of test to outcome
# { test -> {(outcome, color, specific reason?)} }
TestOutcomeMap = dict[str, set[tuple[str, str, Optional[str]]]]


def as_test_outcome_map(log: LogInfo) -> TestOutcomeMap:
    ret: TestOutcomeMap = {}

    for (cause, color), tests in log.items():
        for test, reason in tests:
            if test not in ret:
                ret[test] = set()
            ret[test].add((cause, color, reason))

    return ret


def main(files: list[str]):
    stats_all: list[LogInfo] = [analyse(file) for file in files]
    stats: LogInfo = merge(stats_all)
    items: LogInfoList = as_items(stats)

    i = 0
    verbosity = sum(1 for flag in sys.argv if flag == "-v")

    print(f"{BOLD}Summary:{RESET}")
    for cause, color, num, tests in items:
        print(f"{rainbow(i)}|{RESET} {color}{num:3d}{RESET} {cause}")
        if verbosity >= 1:
            dot = f"{rainbow(i)}•{RESET}"
            if all(test[1] is None for test in tests):
                # print tests one by one
                tests = [file for file, _ in tests]
                tests.sort()
                tests_str = f"\n  {dot} ".join([file_str(f) for f in tests])
                print(f"  {dot} {tests_str}")
            else:
                # aggregate by reason
                reasons_d: dict[str, list[str]] = {}
                for file, reason in tests:
                    if reason is None:
                        reason = "Unknown reason"
                    reasons_d[reason] = reasons_d.get(reason, []) + [file]
                reasons = reasons_d.items()
                if "--az" in sys.argv:
                    reasons = sorted(reasons, key=lambda x: x[0])
                else:
                    reasons = sorted(reasons, key=lambda x: -len(x[1]))
                if "--rev" in sys.argv:
                    reasons.reverse()
                for reason, files in reasons:
                    print(f"  {dot} {reason} ({len(files)})")
                    if verbosity >= 2:
                        files.sort()
                        files_str = "\n      ".join([file_str(f) for f in files])
                        print(f"      {files_str}")
        i += 1

    print(
        f"{BOLD}Total:{RESET} {len(set(t[0] for tests in stats.values() for t in tests))}"
    )


# { (reason, color, specific reason?) }
TestInfo = set[tuple[str, str, Optional[str]]]
# { test -> (message U (before, after)) }
Diff = dict[str, str | tuple[TestInfo, TestInfo]]


def diff(f1: str, f2: str):
    log1 = analyse(f1)
    tests1 = as_test_outcome_map(log1)

    log2 = analyse(f2)
    tests2 = as_test_outcome_map(log2)

    all_tests = [*(set(tests1.keys()).union(tests2.keys()))]
    all_tests.sort()

    diffs: Diff = {}

    for test in all_tests:
        file1 = tests1.get(test)
        file2 = tests2.get(test)
        if file1 is None:
            diffs[test] = "Not present before"
            continue
        if file2 is None:
            diffs[test] = "Not present after"
            continue
        only_before = file1 - file2
        only_after = file2 - file1
        if len(only_before) != 0 or len(only_after) != 0:
            diffs[test] = (only_before, only_after)

    all_causes = list(set(log1.keys()).union(log2.keys()))
    all_causes.sort(key=lambda x: x[0])
    verbosity = sum(1 for flag in sys.argv if flag == "-v")

    minus = f"{RED}-{RESET}"
    plus = f"{GREEN}+{RESET}"
    i = 0
    print(f"{BOLD}Summary:{RESET}")
    for key in all_causes:
        (cause, color) = key
        len_before = len(log1.get(key, []))
        len_after = len(log2.get(key, []))
        if len_before == len_after:
            msg = f"{GRAY}{len_before}{RESET}"
        else:
            msg = f"{len_before} -> {len_after}"
        print(f"{rainbow(i)}|{RESET} {color}{cause}{RESET}: {msg}")
    if verbosity < 1:
        return
    print()
    print(f"{BOLD}Diffs:{RESET} ({len(diffs)})")
    for test, diff in diffs.items():
        if isinstance(diff, str):
            print(f"{rainbow(i)}|{RESET} {test}{RESET} {diff}")
        else:
            print(f"{rainbow(i)}|{RESET} {test}{RESET}")
            if verbosity < 2:
                continue
            only_before, only_after = diff
            for cause, color, reason in only_before:
                if reason is not None:
                    print(f"  {minus} {color}{cause}{RESET} ({reason})")
                else:
                    print(f"  {minus} {color}{cause}{RESET}")
            for cause, color, reason in only_after:
                if reason is not None:
                    print(f"  {plus} {color}{cause}{RESET} ({reason})")
                else:
                    print(f"  {plus} {color}{cause}{RESET}")
        i = i + 1


if __name__ == "__main__":
    if len(sys.argv) < 2 or "--help" in sys.argv:
        print("Usage: parselog.py <logfile> [...logfiles] [...--flags]")
        sys.exit(1)

    # Normalise arguments: ["-F", "abc"] becomes ["-F=abc"]
    args = sys.argv
    i = 0
    while i < len(args):
        if args[i] == "-F" or args[i] == "-f":
            args[i] = args[i] + "=" + args[i + 1]
            args.pop(i + 1)
        i += 1
    sys.argv = args

    files = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    if files is []:
        print("Usage: parselog.py <logfile> [...logfiles] [...--flags]")
        sys.exit(1)
    if "--diff" in sys.argv:
        if len(files) != 2:
            print("--diff requires two files")
            sys.exit(1)
        diff(files[0], files[1])
    else:
        main(files)
