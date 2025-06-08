import sys
import os
import subprocess
import re
from pathlib import Path
from typing import TypedDict, Optional

PURPLE = "\033[0;35m"
RED = "\033[0;31m"
ORANGE = "\033[38;5;208m"
YELLOW = "\033[38;5;220m"
GREEN = "\033[0;32m"
CYAN = "\033[0;36m"
BLUE = "\033[0;34m"
MAGENTA = "\033[0;95m"
GRAY = "\033[0;90m"
BOLD = "\033[1m"
RESET = "\033[0m"

# if piping output, remove colors:
NO_COLOR = not sys.stdout.isatty()
if NO_COLOR:
    PURPLE = RED = ORANGE = YELLOW = GREEN = CYAN = BLUE = GRAY = BOLD = RESET = ""


rainbow_ = 0


def inc_rainbow():
    global rainbow_
    rainbow_ += 1


def rainbow():
    if NO_COLOR:
        return ""
    return [
        "\033[38;5;197m",
        "\033[38;5;208m",
        "\033[38;5;220m",
        "\033[38;5;70m",
        "\033[38;5;74m",
        "\033[38;5;33m",
        "\033[38;5;127m",
    ][rainbow_ % 7]


def pprint(*args, inc: bool = False, **kwargs):
    if NO_COLOR:
        print("| ", end="")
    else:
        clr = rainbow()
        print(f"{clr}|{RESET} ", end="")
    print(*args, **kwargs)
    if inc:
        inc_rainbow()


# displays a list of rows -- each cell is text and an optional color
def pptable(rows: list[list[tuple[str, Optional[str]]]]):
    cols = len(rows[0])
    col_len = [max(len(row[i][0]) for row in rows) for i in range(cols)]
    pad = "  "
    for row in rows:
        pprint(
            pad.join(
                (clr or "") + cell + RESET + " " * (col_len[i] - len(cell))
                for i, (cell, clr) in enumerate(row)
            ),
            inc=True,
        )


pass_ = lambda x: ("Success", GREEN, x)
fail_ = lambda x: ("Failure", RED, x)
unkn_ = lambda x: ("Unknown", YELLOW, x)

SKIPPED_TESTS: dict[str, tuple[str, str, str]] = {
    # Kani
    "ArithOperators/rem_float_fixme.rs": fail_("Complicated float expression"),
    "ConstEval/limit.rs": unkn_("Takes ages to compile (const eval loop of 131072)"),
    "FloatingPoint/main.rs": pass_("Slow floating operation operations"),
    "BitwiseShiftOperators/shift_neg_vals.rs": unkn_(
        "Wrapping operations without loop unrolling branch too much"
    ),
    "Intrinsics/Count/ctpop.rs": pass_("The test requires 2^N branches"),
    "Intrinsics/Math/Rounding/Ceil/ceilf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Ceil/ceilf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Floor/floorf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Floor/floorf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Ceil/floorf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Ceil/floorf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/RInt/rintf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/RInt/rintf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Round/roundf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Round/roundf64.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Trunc/truncf32.rs": pass_("Slow floating point rounding"),
    "Intrinsics/Math/Rounding/Trunc/truncf64.rs": pass_("Slow floating point rounding"),
    # Miri
    "pass/issues/issue-17877.rs": unkn_("Makes an array of size 16384, too slow"),
    "pass/tag-align-dyn-u64.rs": unkn_("Slow due to symbolic checks on the pointer"),
}

KNOWN_ISSUES = {
    # Kani
    "ArithOperators/unsafe_add_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "ArithOperators/unsafe_mul_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "ArithOperators/unsafe_sub_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "Intrinsics/Compiler/variant_count.rs": "Kani doesn't handle variant_count yet -- we do!",
    "Intrinsics/ConstEval/pref_align_of.rs": "Requires support for custom target architectures",
    "LayoutRandomization/should_fail.rs": "We don't handle layout randomization yet",
    "Uninit/access-padding-enum-diverging-variants.rs": "Kani can't handle variants with different paddings",
    "Uninit/access-padding-enum-multiple-variants.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "Uninit/access-padding-enum-single-field.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "Uninit/access-padding-enum-single-variant.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "ValidValues/write_bytes.rs": "Kani checks for validity on write, whereas Miri does on read; we copy Miri.",
    # Miri
    "fail/intrinsics/typed-swap-invalid-scalar.rs": "Uses weird CFGs, technically we pass it",
    "fail/erroneous_const.rs": "We lazily load constants, so the panic never triggers",
    "pass/integer-ops.rs": "Miri allows negative bit shifts, we don't (like Kani)",
    "pass/disable-alignment-check.rs": "We don't provide a way to disable alignment checks",
}

PWD = Path(os.path.dirname(os.path.abspath(__file__)))


def build_rusteria():
    env = (
        subprocess.check_output("opam exec -- dune exec -- env", shell=True)
        .decode()
        .split("\n")
    )
    for line in env:
        if not "=" in line:
            continue
        name, value = line.split("=", 1)
        os.environ[name] = value
    os.environ["RUSTERIA_PLUGINS"] = str(PWD / ".." / "plugins")
    try:
        subprocess.check_call("dune build", shell=True)
    except subprocess.CalledProcessError:
        print(f"{RED}Rusteria couldn't build")
        exit(1)


def determine_failure_expect(filepath: str) -> bool:
    if "kani" in filepath:
        try:
            with open(filepath, "r") as f:
                content = f.read()
                return "kani-verify-fail" in content
        except:
            ...
    elif "miri" in filepath:
        return "/tests/fail/" in filepath or "/tests/panic/" in filepath
    return False


class Flags(TypedDict):
    cmd_flags: list[str]
    filters: list[str]
    exclusions: list[str]
    iterations: Optional[int]
    tag: Optional[str]


def parse_flags(argv: list[str]) -> Flags:
    i = 0
    flags: Flags = {
        "cmd_flags": [],
        "filters": [],
        "exclusions": [],
        "iterations": None,
        "tag": None,
    }
    while i < len(argv):
        arg = argv[i]
        if arg == "--":
            flags["cmd_flags"] += argv[i + 1 :]
            break
        elif arg == "-f":
            flags["filters"].append(argv[i + 1])
            i += 1
        elif arg == "-e":
            flags["exclusions"].append(argv[i + 1])
            i += 1
        elif arg == "-i":
            flags["iterations"] = int(argv[i + 1])
            i += 1
        elif arg == "--tag":
            flags["tag"] = argv[i + 1]
            i += 1
        else:
            print(f"{RED}Unknown flag: {arg}")
            exit(1)
        i += 1

    return flags
