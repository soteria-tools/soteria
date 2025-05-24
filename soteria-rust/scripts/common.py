import sys
import os
import subprocess
from pathlib import Path
from typing import TypedDict

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


KNOWN_ISSUES = {
    # Kani
    "ArithOperators/unsafe_add_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "ArithOperators/unsafe_mul_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "ArithOperators/unsafe_sub_fail.rs": "The main function takes a parameter?? Kani crashes too",
    "Intrinsics/Compiler/variant_count.rs": "Kani doesn't handle variant_count yet -- we do!",
    "LayoutRandomization/should_fail.rs": "We don't handle layout randomization yet",
    "Uninit/access-padding-enum-diverging-variants.rs": "Kani can't handle variants with different paddings",
    "Uninit/access-padding-enum-multiple-variants.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "Uninit/access-padding-enum-single-field.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "Uninit/access-padding-enum-single-variant.rs": "Kani assumes discriminants are i32, but Charon gives isize",
    "ValidValues/write_bytes.rs": "Kani checks for validity on write, whereas Miri does on read; we copy Miri.",
    # Miri
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
    res = subprocess.check_call("dune build", shell=True)
    if res != 0:
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


def parse_flags() -> Flags:
    i = 2  # ignore idx 0 (executable name) and idx 1 (command)
    flags: Flags = {
        "cmd_flags": [],
        "filters": [],
        "exclusions": [],
    }
    while i < len(sys.argv):
        arg = sys.argv[i]
        if arg == "--":
            flags["cmd_flags"] += sys.argv[i + 1 :]
            break
        elif arg == "-f":
            flags["filters"].append(sys.argv[i + 1])
            i += 1
        elif arg == "-e":
            flags["exclusions"].append(sys.argv[i + 1])
            i += 1
        else:
            print(f"{RED}Unknown flag: {arg}")
            exit(1)
        i += 1

    return flags
