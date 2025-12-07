import enum
import os
import subprocess
import sys
from pathlib import Path
from typing import Literal, Optional, TypeVar, cast

# ------ Pretty printing ------

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


def pprint(*args, inc: bool = True, end="\n", **kwargs):
    if NO_COLOR:
        print("| ", end="")
    else:
        clr = rainbow()
        print(f"{clr}|{RESET} ", end="")
    print(*args, **kwargs, end=end + RESET)
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
        )


# ------ Shared types and definitions ------

PWD = Path(os.path.dirname(os.path.abspath(__file__)))

ToolName = Literal["Rusteria", "Kani", "Miri"]
TOOL_NAMES: list[ToolName] = ["Rusteria", "Kani", "Miri"]
SuiteName = Literal["kani", "miri", "custom"]
SUITE_NAMES: list[SuiteName] = ["miri", "kani", "custom"]

# ------ Outcomes ------


# We have detailed outcomes, and simplified ones for the benchmark, with a mapping
class Outcome(enum.Enum):
    def __new__(cls, *args, **kwds):
        value = len(cls.__members__) + 1
        obj = object.__new__(cls)
        obj._value_ = value
        return obj

    def __init__(self, txt, clr):
        self.txt = txt
        self.clr = clr

    def __str__(self):
        return f"{self.clr}{self.txt}{RESET}"

    def __lt__(self, other):
        if not isinstance(other, Outcome):
            return NotImplemented
        return self.txt < other.txt

    def __le__(self, other):
        if not isinstance(other, Outcome):
            return NotImplemented
        return self.txt <= other.txt

    def __call__(self, reason: Optional[str] = None) -> tuple["Outcome", Optional[str]]:
        return (self, reason or self.txt)

    def is_pass(self) -> bool:
        return self == Outcome.PASS

    def is_fail(self) -> bool:
        return self == Outcome.FAIL

    def is_expected(self) -> bool:
        return self in (Outcome.PASS, Outcome.FAIL)

    def is_timeout(self) -> bool:
        return self == Outcome.TIME_OUT

    def is_tool(self) -> bool:
        return self == Outcome.TOOL

    def is_simple(self) -> bool:
        return self in (Outcome.PASS, Outcome.FAIL, Outcome.TIME_OUT, Outcome.CRASH)

    def simplify(self) -> "Outcome":
        return {
            Outcome.PASS: Outcome.PASS,
            Outcome.FAIL: Outcome.FAIL,
            Outcome.TIME_OUT: Outcome.TIME_OUT,
        }.get(self, Outcome.CRASH)

    txt: str
    clr: str

    PASS = "pass", GREEN
    FAIL = "fail", RED
    MISSING_DEP = "missing dependency", ORANGE
    COMPILATION_ERR = "compilation error", ORANGE
    TOOL = "tool", PURPLE
    NO_ENTRY_POINTS = "no entry points", ORANGE
    TIME_OUT = "timeout", YELLOW
    UNSUPPORTED = "unsupported", ORANGE
    CRASH = "crash", ORANGE
    UNKNOWN = "unknown", MAGENTA


T = TypeVar("T")


def dict_get_suffix(d: dict[str, T], key: str) -> Optional[T]:
    for k in d:
        if key.endswith(k):
            return d[k]
    return None


def build_rusteria():
    charon_path = PWD / ".." / ".." / ".." / "charon"
    miri_sysroot = (
        subprocess.check_output(
            f"cd {charon_path} && cargo miri setup --print-sysroot", shell=True
        )
        .decode()
        .strip()
    )
    os.environ["RUST_SYSROOT"] = miri_sysroot

    env = (
        subprocess.check_output(
            "opam exec -- dune exec -- env 2> /dev/null", shell=True
        )
        .decode()
        .split("\n")
    )
    for line in env:
        if "=" not in line:
            continue
        name, value = line.split("=", 1)
        os.environ[name] = value

    # find line starting with "host: "
    targets = (
        subprocess.check_output("$(obol toolchain-path)/bin/cargo -vV", shell=True)
        .decode()
        .split("\n")
    )
    for line in targets:
        if line.startswith("host: "):
            os.environ["TARGET"] = line[6:]
            break

    os.environ["RUSTERIA_PLUGINS"] = str((PWD / ".." / "plugins").resolve())
    try:
        subprocess.check_call("dune build > /dev/null 2> /dev/null", shell=True)
        subprocess.check_call("soteria-rust build-plugins > /dev/null", shell=True)
    except subprocess.CalledProcessError:
        print(f"{RED}Rusteria couldn't build")
        exit(1)


def determine_failure_expect(filepath: str) -> bool:
    if "kani" in filepath:
        try:
            with open(filepath, "r") as f:
                content = f.read()
                return "kani-verify-fail" in content
        except Exception:
            ...
    elif "miri" in filepath:
        return "/tests/fail/" in filepath or "/tests/panic/" in filepath
    return False


def subprocess_run(
    *popenargs, input=None, capture_output=False, timeout=None, check=False, **kwargs
):
    """
    Patched version of subprocess.run, that uses a SIGTERM rather than a SIGKILL
    to terminate on timeout.
    """
    from subprocess import (
        PIPE,
        CalledProcessError,
        CompletedProcess,
        Popen,
        TimeoutExpired,
    )

    if input is not None:
        if kwargs.get("stdin") is not None:
            raise ValueError("stdin and input arguments may not both be used.")
        kwargs["stdin"] = PIPE

    if capture_output:
        if kwargs.get("stdout") is not None or kwargs.get("stderr") is not None:
            raise ValueError(
                "stdout and stderr arguments may not be used " "with capture_output."
            )
        kwargs["stdout"] = PIPE
        kwargs["stderr"] = PIPE

    with Popen(*popenargs, **kwargs) as process:
        try:
            stdout, stderr = process.communicate(input, timeout=timeout)
        except TimeoutExpired as _:
            process.terminate()
            # NOTE!!! HERE WE WINDOWS SHOULD BE HANDLED DIFFERENTLY
            # POSIX _communicate already populated the output so
            # far into the TimeoutExpired exception.
            process.wait()
            raise
        except:  # Including KeyboardInterrupt, communicate handled that.
            process.terminate()
            # We don't call process.wait() as .__exit__ does that for us.
            raise
        retcode = cast(int, process.poll())
        if check and retcode:
            raise CalledProcessError(
                retcode, process.args, output=stdout, stderr=stderr
            )
    return CompletedProcess(process.args, retcode, stdout, stderr)
