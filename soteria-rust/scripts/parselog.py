#!/usr/bin/env python3

import sys
import re
from typing import Optional

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
    "kani/Intrinsics/Compiler/variant_count.rs": "Kani doesn't handle variant_count yet -- we do!",
    "kani/Uninit/access-padding-enum-diverging-variants.rs": "Kani can't handle variants with different paddings",
    "pass/integer-ops.rs": "Miri allows negative bit shifts, we don't (like Kani)",
    "pass/disable-alignment-check.rs": "We don't provide a way to disable alignment checks",
}


def file_str(file_name: str):
    issue = known_issue.get(file_name, None)
    if issue:
        return f"{GRAY}{file_name} {YELLOW}✦{RESET} {BOLD}{issue}{RESET}"
    return file_name


def main(files: list[str]):
    cause_filters = [arg[3:] for arg in sys.argv if arg.startswith("-F=")]
    file_filters = [arg[3:] for arg in sys.argv if arg.startswith("-f=")]

    # for each reason, the list of tests
    stats: dict[tuple[str, str], set[tuple[str, Optional[str]]]] = {}

    def log(test: str, cause: str, color: str, reason: Optional[str] = None):
        key = (cause, color)
        if key not in stats:
            stats[key] = set()
        stats[key].add((test, reason))

    for file in files:
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

                hax_panics = re.findall(
                    r"thread \'rustc\' panicked at .+/frontend/(exporter/src/.+):", test
                )
                sub_errors = sub_errors + [f"Hax panicked: {err}" for err in hax_panics]

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

    i = 0
    items = [
        (cause, color, len(set(test[0] for test in tests)), tests)
        for (cause, color), tests in stats.items()
        if (len(cause_filters) == 0 or any(filter in cause for filter in cause_filters))
    ]

    if "--az" in sys.argv:
        items.sort(key=lambda x: x[0])
    else:
        items.sort(key=lambda x: -x[2])

    if "--rev" in sys.argv:
        items.reverse()

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
    main(files)
