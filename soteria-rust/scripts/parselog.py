#!/usr/bin/env python3

from common import *
import sys
import re
from typing import Iterable, Optional, Protocol, assert_never


def file_str(file_name: str, tool: ToolName):
    issue = KNOWN_ISSUES.get(file_name, None)
    if issue and tool == "Rusteria":
        return f"{GRAY}{file_name} {YELLOW}✦{RESET} {BOLD}{issue}{RESET} {GRAY}{tool}{RESET}"
    return f"{file_name} {GRAY}{tool}{RESET}"


# { (result, color) -> {(tool, test, specific reason?)} }
LogInfo = dict[Outcome, set[tuple[ToolName, str, Optional[str]]]]
LogCategorisation_ = tuple[Outcome, Optional[str]]
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
                Outcome.COMPILATION_ERR(f"missing {crate}::{fn}")
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
            return [Outcome.COMPILATION_ERR(error) for error in compile_errors]

        if "cannot find macro" in test:
            err = re.search(r"(cannot find macro.*)\n", test)
            if err is not None:
                return Outcome.COMPILATION_ERR(f"{err}")

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
            return [Outcome.TOOL(reason) for reason in sub_errors]
        return Outcome.TOOL(None)

    if "Fatal: No entry points found" in test:
        return Outcome.NO_ENTRY_POINTS()

    if "Execution timed out" in test or "Forced timeout" in test:
        return Outcome.TIME_OUT()

    if "resolve_constant (Generated_Expressions.COpaque" in test:
        return Outcome.TOOL("Constant resolving")

    # check engine errors first; one error overrides any success
    fatal_regex = r"^warning: .*s\): (.*)"
    fatals = re.search(fatal_regex, test, re.MULTILINE)
    if fatals is not None:
        err = fatals.group(1)
        if err.startswith("unsupported feature"):
            return Outcome.UNSUPPORTED(err[len("unsupported feature, ") :])

        if err.startswith("exception, "):
            err = err[len("exception, ") :]

        if err.startswith('Failure("'):
            err = err[len('Failure("') : -2]

        return Outcome.CRASH(err)

    err_regex = r"^error: (.+): found issues in"
    if re.search(err_regex, test, re.MULTILINE):
        if expect_failure:
            return Outcome.PASS("Expected failure, got failure")
        else:
            return Outcome.FAIL("Expected success, got failure")

    ok_regex = r"^note: .*: done in"
    if re.search(ok_regex, test, re.MULTILINE):
        if not expect_failure:
            return Outcome.PASS("Expected success, got success")
        else:
            return Outcome.FAIL("Expected failure, got success")

    if "internal error, uncaught exception" in test:
        return Outcome.CRASH()

    if "unknown option" in test:
        return Outcome.CRASH()

    return Outcome.UNKNOWN()


def categorise_kani(test: str, *, expect_failure: bool) -> LogCategorisation:
    if "CBMC timed out" in test or "Forced timeout" in test:
        return Outcome.TIME_OUT()

    if (
        "A Rust construct that is not currently supported by Kani was found to be reachable"
        in test
    ):
        return Outcome.UNSUPPORTED()

    if "VERIFICATION:- SUCCESSFUL" in test:
        if not expect_failure:
            return Outcome.PASS("Expected success, got success")
        else:
            return Outcome.FAIL("Expected failure, got success")

    if "VERIFICATION:- FAILED" in test:
        if expect_failure:
            return Outcome.PASS("Expected failure, got failure")
        else:
            return Outcome.FAIL("Expected success, got failure")

    if "exited with status exit status" in test or "fatal runtime error" in test:
        return Outcome.CRASH()

    if "No proof harnesses" in test:
        return Outcome.NO_ENTRY_POINTS()

    return Outcome.UNKNOWN()


def categorise_miri(test: str, *, expect_failure: bool) -> LogCategorisation:
    if "Forced timeout" in test:
        return Outcome.TIME_OUT()

    if (
        "use of unresolved module or unlinked crate `kani`" in test
        or "can't find crate for `kani`" in test
    ):
        return Outcome.UNSUPPORTED()

    if (
        "functions used as tests can not have any arguments" in test
        or "error: Miri can only run programs that have a main function." in test
    ):
        return Outcome.NO_ENTRY_POINTS()

    if "test result: ok." in test or "CODE: 0" in test:
        if not expect_failure:
            return Outcome.PASS("Expected success, got success")
        else:
            return Outcome.FAIL("Expected failure, got success")

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
            return Outcome.PASS("Expected failure, got failure")
        else:
            return Outcome.FAIL("Expected success, got failure")

    if "error" in test:
        if expect_failure:
            return Outcome.PASS("Expected failure, got failure")
        else:
            return Outcome.FAIL("Expected success, got failure")

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

    return Outcome.UNKNOWN()


def analyse(file: str) -> LogInfo:
    file_filters = [arg[3:] for arg in sys.argv if arg.startswith("-f=")]

    stats: LogInfo = {}
    tool: ToolName = "Rusteria"

    def log(tool: ToolName, test: str, outcome: Outcome, reason: Optional[str] = None):
        if outcome not in stats:
            stats[outcome] = set()
        if reason:
            reason = reason.replace("\\n", "\n")
        stats[outcome].add((tool, test, reason))

    try:
        content = open(file, "r").read()
    except FileNotFoundError:
        exit(f"File not found: {file}")
    tests = content.split("[TEST] Running ")[1:]
    print(f"• Found {len(tests)} tests in {file}")
    for test in tests:
        # get file name
        file_path = re.search(r"(.+) - .*\n", test)
        if not file_path:
            exit(f"No file found in {test}")
        file_path = file_path.group(1)
        if len(file_filters) and not any(
            [filter in file_path for filter in file_filters]
        ):
            continue

        # detect failure mode
        expect_failure = False
        if "kani" in file_path:
            try:
                with open(file_path, "r") as f:
                    content = f.read()
                    # this only holds for kani!
                    expect_failure = "kani-verify-fail" in content
            except Exception:
                ...
        elif "miri" in file_path:
            expect_failure = "/fail/" in file_path or "/panic/" in file_path

        tests_idx = file_path.split("/").index("tests") + 1
        file_name = "/".join(file_path.split("/")[tests_idx:])
        if file_name.startswith("kani/"):
            file_name = file_name[len("kani/") :]

        # categorise appropriately
        if tool == "Kani":
            categories = categorise_kani(test, expect_failure=expect_failure)
        elif tool == "Rusteria":
            categories = categorise_rusteria(test, expect_failure=expect_failure)
        elif tool == "Miri":
            categories = categorise_miri(test, expect_failure=expect_failure)
        else:
            assert_never(tool)

        if isinstance(categories, list):
            for outcome, reason in categories:
                log(tool, file_name, outcome, reason)
        else:
            outcome, reason = categories
            log(tool, file_name, outcome, reason)

        # detect tool change
        tool_mode = re.search(r"Running benchmark \w+ with (\w+)", test)
        if tool_mode:
            tool = cast(ToolName, tool_mode.group(1))
        elif "Kani Rust Verifier" in test:
            tool = "Kani"

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
    tool_filters = [arg[3:] for arg in sys.argv if arg.startswith("-T=")]
    return {
        outcome: filtered_tests
        for outcome, tests in log.items()
        if (
            len(cause_filters) == 0
            or any(filter in outcome.txt for filter in cause_filters)
        )
        if len(
            filtered_tests := set(
                test
                for test in tests
                if (
                    len(tool_filters) == 0
                    or any(filter in test[0] for filter in tool_filters)
                )
            )
        )
        > 0
    }


# List equivalent of LogInfo:
# [( cause, color, test_num, {(test, specific reason?)} )]
LogInfoList = list[tuple[Outcome, int, set[tuple[ToolName, str, Optional[str]]]]]


# parses a LogInfo into a list LogInfoList, applying the required filtering and sorting
def as_items(log: LogInfo) -> LogInfoList:
    alpha_sort = "--az" in sys.argv
    rev_sort = "--rev" in sys.argv

    items = [
        (outcome, len(set((test[0], test[1]) for test in tests)), tests)
        for outcome, tests in log.items()
    ]

    if alpha_sort:
        items.sort(key=lambda x: x[0].txt)
    else:
        items.sort(key=lambda x: -x[1])

    if rev_sort:
        items.reverse()

    return items


# Reverse of LogInfo: mapping of test to outcome
# { test -> {(tool, outcome, color, specific reason?)} }
TestOutcomeMap = dict[str, set[tuple[ToolName, Outcome, Optional[str]]]]


def as_test_outcome_map(log: LogInfo) -> TestOutcomeMap:
    ret: TestOutcomeMap = {}

    for outcome, tests in log.items():
        for tool, test, reason in tests:
            if test not in ret:
                ret[test] = set()
            ret[test].add((tool, outcome, reason))

    return ret


def main(files: list[str]):
    stats_all: list[LogInfo] = [analyse(file) for file in files]
    stats: LogInfo = merge(stats_all)
    items: LogInfoList = as_items(filtered(stats))

    verbosity = sum(1 for flag in sys.argv if flag == "-v")

    print(f"{BOLD}Summary:{RESET}")
    for outcome, num, tests in items:
        pprint(f"{BOLD}{num:3d}{RESET} {outcome}", inc=False)
        if verbosity >= 1:
            dot = f"{rainbow()}•{RESET}"
            if all(test[1] is None for test in tests):
                # print tests one by one
                ts: list[tuple[str, ToolName]] = [(file, t) for t, file, _ in tests]
                ts.sort()
                tests_str = f"\n  {dot} ".join([file_str(f, t) for f, t in ts])
                print(f"  {dot} {tests_str}")
            else:
                # aggregate by reason
                reasons_d: dict[str, list[tuple[str, ToolName]]] = {}
                for tool, file, reason in tests:
                    if reason is None:
                        reason = "Unknown reason"
                    reasons_d[reason] = reasons_d.get(reason, []) + [(file, tool)]
                reasons = reasons_d.items()
                if "--az" in sys.argv:
                    reasons = sorted(reasons, key=lambda x: x[0])
                else:
                    reasons = sorted(reasons, key=lambda x: -len(x[1]))
                if "--rev" in sys.argv:
                    reasons.reverse()
                for reason, ts in reasons:
                    print(f"  {dot} {reason} ({len(ts)})")
                    if verbosity >= 2:
                        ts.sort()
                        print(
                            "      " + "\n      ".join([file_str(f, t) for f, t in ts])
                        )
        inc_rainbow()

    print(
        f"{BOLD}Total:{RESET} {len(set(t[0] for tests in stats.values() for t in tests))}"
    )


# { (reason, color, specific reason?) }
TestInfo = set[tuple[ToolName, Outcome, Optional[str]]]
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
                filter in outcome.txt
                for filter in cause_filters
                for (_, outcome, _) in before.union(after)
            )

        diffs = {test: diff for test, diff in diffs.items() if filter_diff(diff)}

    all_outcomes = list(set(log1.keys()).union(log2.keys()))
    all_outcomes.sort()
    verbosity = sum(1 for flag in sys.argv if flag == "-v")

    minus = f"{RED}-{RESET}"
    plus = f"{GREEN}+{RESET}"
    print(f"{BOLD}Summary:{RESET}")
    for outcome in all_outcomes:
        len_before = len(log1.get(outcome, []))
        len_after = len(log2.get(outcome, []))
        if len_before == len_after:
            msg = f"{GRAY}{len_before}{RESET}"
        else:
            msg = f"{len_before} -> {len_after}"
        pprint(f"{outcome}: {msg}")
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
            pprint(f"{test}{RESET} {diff}", inc=False)
        else:

            def mk_str(outcomes):
                return ", ".join(
                    f"{color}{cause}{RESET}"
                    for cause, color in set(
                        ((cause, color) for cause, color, _ in outcomes)
                    )
                )

            only_before, only_after = diff
            pprint(
                f"{test}{RESET} {mk_str(only_before)} → {mk_str(only_after)}", inc=False
            )
            if verbosity < 2:
                continue
            for _, outcome, reason in only_before:
                if reason is not None:
                    print(f"  {minus} {outcome} ({reason})")
                else:
                    print(f"  {minus} {outcome}")
            for _, outcome, reason in only_after:
                if reason is not None:
                    print(f"  {plus} {outcome} ({reason})")
                else:
                    print(f"  {plus} {outcome}")


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
    if files == []:
        print("Usage: parselog.py <logfile> [...logfiles] [...--flags]")
        sys.exit(1)
    if "--diff" in sys.argv:
        if len(files) != 2:
            print("--diff requires two files")
            sys.exit(1)
        diff(files[0], files[1])
    else:
        main(files)
