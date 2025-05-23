import sys
import os
import subprocess

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


def pprint(*args):
    if NO_COLOR:
        print("| ", end="")
    else:
        clr = rainbow()
        print(f"{clr}|{RESET} ", end="")
    print(*args)


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

# location of this file
PWD = os.path.dirname(os.path.abspath(__file__))


def build_rusteria():
    # 1. execute "opam exec -- dune exec -- env"
    # 2. export that output to the environment
    # 3. run "dune build", ensure succeeds
    env = (
        subprocess.check_output("opam exec -- dune exec -- env", shell=True)
        .decode()
        .split("\n")
    )
    for line in env:
        name, value = line.split("=", 1)
        os.environ[name] = value
    cmd = "dune build"
    res = subprocess.check_call(cmd, shell=True)
    if res != 0:
        print(f"{RED}Rusteria couldn't build")
        exit(1)
