import enum
import os
import subprocess
import sys
from pathlib import Path
# `cast` is re-exported for the scripts that rely on `from common import *`.
from typing import Literal, Optional, TypeVar, cast  # noqa: F401

# Make the shared utilities importable regardless of where this script is run
# from: walk up to the repository root and add its `scripts/` directory to the
# path, then re-export everything (colours, message levels, rainbow, pprint,
# pptable, subprocess_run, ...) so `from common import *` keeps providing them.
_dir = Path(__file__).resolve().parent
while not (_dir / "scripts" / "soteria_utils.py").exists():
    if _dir.parent == _dir:
        raise RuntimeError("could not locate scripts/soteria_utils.py")
    _dir = _dir.parent
sys.path.insert(0, str(_dir / "scripts"))

from soteria_utils import *  # noqa: E402,F401,F403

# ------ Shared types and definitions ------

PWD = Path(os.path.dirname(os.path.abspath(__file__)))

ToolName = Literal["Soteria", "Kani", "Miri"]
TOOL_NAMES: list[ToolName] = ["Soteria", "Kani", "Miri"]
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

    def is_unsupported(self) -> bool:
        return self == Outcome.UNSUPPORTED

    def is_expected(self) -> bool:
        return self in (Outcome.PASS, Outcome.FAIL)

    def is_timeout(self) -> bool:
        return self == Outcome.TIME_OUT

    def is_tool(self) -> bool:
        return self in (Outcome.TOOL, Outcome.VANISHED)

    def is_simple(self) -> bool:
        return self in (
            Outcome.PASS,
            Outcome.FAIL,
            Outcome.TIME_OUT,
            Outcome.UNSUPPORTED,
        )

    def simplify(self) -> "Outcome":
        return {
            Outcome.PASS: Outcome.PASS,
            Outcome.FAIL: Outcome.FAIL,
            Outcome.TIME_OUT: Outcome.TIME_OUT,
        }.get(self, Outcome.UNSUPPORTED)

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
    VANISHED = "vanished", PURPLE
    UNKNOWN = "unknown", MAGENTA


T = TypeVar("T")


def dict_get_suffix(d: dict[str, T], key: str) -> Optional[T]:
    for k in d:
        if key.endswith(k):
            return d[k]
    return None


def get_toolchain() -> str:
    toolchain_path = (
        subprocess.check_output("obol toolchain-path", shell=True).decode().strip()
    )
    return "-".join(os.path.basename(toolchain_path).split("-")[0:4])


def toolchain_bin(cmd: str) -> str:
    return (
        subprocess.check_output(f"$(obol toolchain-path)/bin/{cmd}", shell=True)
        .decode()
        .strip()
    )


_target: Optional[str] = None


def get_target() -> str:
    """The host triple, e.g. `aarch64-apple-darwin`."""
    global _target
    if _target is None:
        _target = os.environ.get("TARGET") or next(
            line.removeprefix("host: ")
            for line in toolchain_bin("cargo -vV").split("\n")
            if line.startswith("host: ")
        )
    return _target


_targets: Optional[list[str]] = None


def get_targets() -> list[str]:
    """The triples we can compile for, the host first, then those that behave
    like it, so a test that must run elsewhere fails for as few incidental
    reasons as possible. The sysroot has one directory per target it ships a
    standard library for; the other targets are of no use to us."""
    global _targets
    if _targets is None:

        def shape(target: str) -> list[str]:
            behaviours = ("target_pointer_width", "target_endian")
            cfg = toolchain_bin(f"rustc --print cfg --target={target}").split()
            return sorted(c for c in cfg if c.startswith(behaviours))

        sysroot = os.environ.get("RUST_SYSROOT") or toolchain_bin(
            "rustc --print sysroot"
        )
        rustlib = Path(sysroot) / "lib" / "rustlib"
        available = (d.name for d in rustlib.iterdir() if (d / "lib").is_dir())
        host = get_target()
        host_shape = shape(host)
        _targets = [host] + sorted(
            (t for t in available if t != host),
            key=lambda t: (shape(t) != host_shape, t),
        )
    return _targets


def get_sysroot(toolchain: str) -> str:
    if not toolchain.startswith("+"):
        toolchain = "+" + toolchain
    return subprocess.run(
        ["cargo", toolchain, "miri", "setup", "--print-sysroot"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()


def build_soteria():
    if not os.environ.get("RUST_SYSROOT"):
        toolchain = get_toolchain()
        sysroot = get_sysroot(toolchain)
        os.environ["RUST_SYSROOT"] = sysroot

    os.environ["TARGET"] = get_target()

    try:
        subprocess.check_call(
            "soteria-rust build-plugins", shell=True, stdout=subprocess.DEVNULL
        )
    except subprocess.CalledProcessError:
        print(f"{RED}Soteria couldn't build")
        exit(1)
