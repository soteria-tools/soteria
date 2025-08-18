#!/usr/bin/env python3

from common import *

import sys
import re
from typing import Iterable, Optional, Protocol


def file_str(file_name: str):
    issue = KNOWN_ISSUES.get(file_name, None)
    if issue:
        return f"{GRAY}{file_name} {YELLOW}✦{RESET} {BOLD}{issue}{RESET}"
    return file_name


# { (result, color) -> {(test, specific reason?)} }
LogInfo = dict[tuple[str, str], set[tuple[str, Optional[str]]]]
LogCategorisation_ = tuple[str, str, Optional[str]]
LogCategorisation = LogCategorisation_ | list[LogCategorisation_]


class TestCategoriser(Protocol):
    def __call__(self, test: str, *, expect_failure: bool) -> LogCategorisation: ...


def categorise_rusteria(test: str, *, expect_failure: bool) -> LogCategorisation:
    if "Fatal (Frontend)" in test:
        # this isn't frontend's fault, really
        unresolved = re.findall(
            r"failed to resolve: could not find `(.+)` in `(.+)`", test
        )
        if unresolved:
            return [
                ("Missing dependency", ORANGE, f"{crate}::{fn}")
                for fn, crate in unresolved
            ]

        compile_errors = re.findall(r"error(\[E\d+\]: .+)\n", test)
        compile_errors = [
            # these are Hax/Charon errors!
            err
            for err in compile_errors
            if not err.startswith("[E9999]")
        ]
        if compile_errors:
            return [("Compilation error", ORANGE, error) for error in compile_errors]

        if "cannot find macro" in test:
            err = re.search(r"(cannot find macro.*)\n", test)
            if err is not None:
                return ("Compilation error", ORANGE, f"{err}")

        sub_errors = []

        err_labels = re.findall(r'The label is "(.+)"', test)
        sub_errors = sub_errors + err_labels

        panics = re.findall(r"thread \'.+\' panicked at (.+):", test)
        sub_errors = sub_errors + [
            (f"Hax panicked: {err}" if "frontend/exporter" in err else f"Panic: {err}")
            for err in panics
        ]

        if "Unexpected trait reference kind" in test:
            sub_errors.append("Unexpected trait reference kind")
        if "Cannot convert constant back to an expression" in test:
            sub_errors.append("Cannot convert constant back to an expression")
        if "Charon__GAstOfJson.gtranslated_crate_of_json" in test:
            sub_errors.append("Parsing ULLBC from JSON")

        if len(sub_errors) > 0:
            return [("Tool", PURPLE, reason) for reason in sub_errors]
        return ("Tool", PURPLE, None)

    if "Fatal: No entry points found" in test:
        return ("No entry points found", RED, None)

    if "Execution timed out" in test or "Forced timeout" in test:
        return ("Time out", ORANGE, None)

    if "resolve_constant (Generated_Expressions.COpaque" in test:
        return ("Tool", PURPLE, "Constant resolving")

    if "MISSING FEATURE, VANISHING" in test:
        cause = re.search(r"MISSING FEATURE, VANISHING: (.+)\n", test)
        if not cause:
            exit(f"No cause found for vanish in {test}")
        cause = cause.group(1)
        color = YELLOW
        reason = None

        if "Unsupported intrinsic" in cause:
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
            cause = "Opaque function - Tool"
            color = PURPLE
        if "Opaque constant" in cause:
            reason = cause.replace("Constant constant: ", "")
            cause = "Opaque constant - Tool"
            color = PURPLE
        if "Splitting " in cause:
            cause = "Splitting value"

        return (cause, color, reason)

    if "Execution vanished" in test:
        return ("Vanished", RED, None)

    if "Miss encountered in WPST" in test:
        return ("Miss encountered", RED, None)

    # check errors first; one error overrides any success
    fatal_regex = r"^error: .*: runtime error in"
    if re.search(fatal_regex, test, re.MULTILINE):
        cause = re.search(r"runtime error in .*Exn: Failure\(\"(.+)\"\)", test)
        cause = cause or re.search(r"runtime error in .*Exn: Failure\(\"(.+)\"\)", test)
        if not cause:
            return ("Raised exception", RED, None)
        return ("Raised exception", RED, cause.group(1))

    err_regex = r"^error: (.+): found issues in"
    if re.search(err_regex, test, re.MULTILINE):
        if expect_failure:
            return ("Success", GREEN, "Expected failure, got failure")
        else:
            return ("Failure", RED, "Expected success, got failure")

    ok_regex = r"^note: .*: done in"
    if re.search(ok_regex, test, re.MULTILINE):
        if not expect_failure:
            return ("Success", GREEN, "Expected success, got success")
        else:
            return ("Failure", RED, "Expected failure, got success")

    if "internal error, uncaught exception" in test:
        return ("Raised exception", RED, None)

    if "unknown option" in test:
        return ("Unknown CLI option", RED, None)

    return (f"Unknown", MAGENTA, None)


def categorise_kani(test: str, *, expect_failure: bool) -> LogCategorisation:
    if "CBMC timed out" in test or "Forced timeout" in test:
        return ("Time out", ORANGE, None)

    if (
        "A Rust construct that is not currently supported by Kani was found to be reachable"
        in test
    ):
        return ("Unsupported - Tool", PURPLE, None)

    if "VERIFICATION:- SUCCESSFUL" in test:
        if not expect_failure:
            return ("Success", GREEN, "Expected success, got success")
        else:
            return ("Failure", RED, "Expected failure, got success")

    if "VERIFICATION:- FAILED" in test:
        if expect_failure:
            return ("Success", GREEN, "Expected failure, got failure")
        else:
            return ("Failure", RED, "Expected success, got failure")

    if "exited with status exit status" in test or "fatal runtime error" in test:
        return ("Crashed", PURPLE, None)

    if "No proof harnesses" in test:
        return ("No entry points found", RED, None)

    return (f"Unknown", MAGENTA, None)


def categorise_miri(test: str, *, expect_failure: bool) -> LogCategorisation:
    if "Forced timeout" in test:
        return ("Time out", ORANGE, None)

    if (
        "use of unresolved module or unlinked crate `kani`" in test
        or "can't find crate for `kani`" in test
    ):
        return ("Unsupported - Tool", PURPLE, None)

    if (
        "functions used as tests can not have any arguments" in test
        or "error: Miri can only run programs that have a main function." in test
    ):
        return ("No entry points found", RED, None)

    if "test result: ok." in test or "CODE: 0" in test:
        if not expect_failure:
            return ("Success", GREEN, "Expected success, got success")
        else:
            return ("Failure", RED, "Expected failure, got success")

    # we're quite fine-grained here to not misattribute compilation errors
    error_signs = [
        "error: Undefined Behavior",
        "error: memory leaked",
        "error: abnormal termination",
        "error: unsupported operation",
        "error: multiple definitions of symbol",
        "error: post-monomorphization error",
        "error[E0308]: mismatched types",
        "accessing memory based on pointer with alignment",
        "symbol definition that clashes with a built-in shim",
        "panicked at",
        "test result: FAILED.",
    ]
    if any(sign in test for sign in error_signs):
        if expect_failure:
            return ("Success", GREEN, "Expected failure, got failure")
        else:
            return ("Failure", RED, "Expected success, got failure")

    if "error" in test:
        if expect_failure:
            return ("Success", GREEN, "Expected failure, got failure")
        else:
            return ("Failure", RED, "Expected success, got failure")

    # if (
    #     "error[E0599]: no method named" in test
    #     or "error[E0432]: unresolved import" in test
    #     or "error[E0433]: failed to resolve" in test
    #     or "error[E0423]: expected function" in test
    #     or "error: cannot find macro" in test
    #     or "error: This macro cannot be used" in test
    #     or "error: format argument must be" in test
    # ):
    #     return ("Compilation error", ORANGE, None)

    return (f"Unknown", MAGENTA, None)


def analyse(file: str) -> LogInfo:
    file_filters = [arg[3:] for arg in sys.argv if arg.startswith("-f=")]

    stats: LogInfo = {}

    def log(test: str, cause: str, color: str, reason: Optional[str] = None):
        key = (cause, color)
        if key not in stats:
            stats[key] = set()
        if reason:
            reason = reason.replace("\\n", "\n")
        stats[key].add((test, reason))

    try:
        content = open(file, "r").read()
    except FileNotFoundError:
        exit(f"File not found: {file}")
    tests = content.split("[TEST] Running ")[1:]
    print(f"• Found {len(tests)} tests in {file}")
    tests[0] = tests[0].replace("Running ", "")
    for test in tests:
        file_path = re.search(r"(.+) - .*\n", test)
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
        if file_name.startswith("kani/"):
            file_name = file_name[len("kani/") :]

        # test run through kani
        if "Kani Rust Verifier" in test:
            categories = categorise_kani(test, expect_failure=expect_failure)
        else:
            categories = categorise_rusteria(test, expect_failure=expect_failure)

        if isinstance(categories, list):
            for cause, color, reason in categories:
                log(file_name, cause, color, reason)
        else:
            cause, color, reason = categories
            log(file_name, cause, color, reason)

    return stats


def merge(logs: Iterable[LogInfo]) -> LogInfo:
    ret: LogInfo = {}
    for log in logs:
        for key, tests in log.items():
            if key not in ret:
                ret[key] = set()
            ret[key] = ret[key].union(tests)
    return ret


def filtered(log: LogInfo) -> LogInfo:
    cause_filters = [arg[3:] for arg in sys.argv if arg.startswith("-F=")]
    return {
        (cause, color): tests
        for (cause, color), tests in log.items()
        if (len(cause_filters) == 0 or any(filter in cause for filter in cause_filters))
    }


# List equivalent of LogInfo:
# [( cause, color, test_num, {(test, specific reason?)} )]
LogInfoList = list[tuple[str, str, int, set[tuple[str, Optional[str]]]]]


# parses a LogInfo into a list TestItems, applying the required filtering and sorting
def as_items(log: LogInfo) -> LogInfoList:
    alpha_sort = "--az" in sys.argv
    rev_sort = "--rev" in sys.argv

    items = [
        (cause, color, len(set(test[0] for test in tests)), tests)
        for (cause, color), tests in log.items()
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
    items: LogInfoList = as_items(filtered(stats))

    verbosity = sum(1 for flag in sys.argv if flag == "-v")

    print(f"{BOLD}Summary:{RESET}")
    for cause, color, num, tests in items:
        pprint(f"{BOLD}{num:3d}{RESET} {color}{cause}{RESET}")
        if verbosity >= 1:
            dot = f"{rainbow()}•{RESET}"
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
        inc_rainbow()

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

    cause_filters = [arg[3:] for arg in sys.argv if arg.startswith("-F=")]
    if len(cause_filters) > 0:

        def filter_diff(c: str | tuple[TestInfo, TestInfo]):
            if isinstance(c, str):
                return True
            before, after = c
            return any(
                filter in cause
                for filter in cause_filters
                for (cause, _, _) in before.union(after)
            )

        diffs = {test: diff for test, diff in diffs.items() if filter_diff(diff)}

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
        pprint(f"{color}{cause}{RESET}: {msg}")
        inc_rainbow()
    if verbosity < 1:
        return

    def sort_items(item: tuple[str, str | tuple[TestInfo, TestInfo]]):
        _, info = item
        if isinstance(info, str):
            return info
        only_before, only_after = info
        if len(only_after) == 0:
            return list(only_before)[0][0]
        return list(only_after)[0][0]

    diffs_items = list(diffs.items())
    diffs_items.sort(key=sort_items)
    print()
    print(f"{BOLD}Diffs:{RESET} ({len(diffs)})")
    for test, diff in diffs_items:
        inc_rainbow()
        if isinstance(diff, str):
            pprint(f"{test}{RESET} {diff}")
        else:

            def mk_str(outcomes):
                return ", ".join(
                    f"{color}{cause}{RESET}"
                    for cause, color in set(
                        ((cause, color) for cause, color, _ in outcomes)
                    )
                )

            only_before, only_after = diff
            pprint(f"{test}{RESET} {mk_str(only_before)} → {mk_str(only_after)}")
            if verbosity < 2:
                continue
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
