"""Central utilities shared by every Python script in the Soteria repository.

This module is intentionally dependency-free (standard library only) so that the
scripts importing it keep running without any setup. It concentrates the bits
that used to be copy-pasted across scripts:

  * the colour palette and TTY/`NO_COLOR` detection
  * the message levels (info / success / warning / error / step / fail)
  * the rainbow helpers and the `pprint` / `pptable` pretty printers
  * a terminal `Progress` bar that cooperates with the message levels
  * small filesystem / subprocess helpers (`get_env_path_or`, `ask_and_remove`,
    `subprocess_run`)

Scripts living in `scripts/` can simply `from soteria_utils import *`. Scripts in
sub-projects (`soteria-rust/scripts/`, `soteria-c/scripts/`) add the repository's
`scripts/` directory to `sys.path` first; see `common.py` and `utils.py`.
"""

import os
import subprocess
import sys
from pathlib import Path
from typing import NoReturn, Optional

# Public API exported by `from soteria_utils import *` (keeps stdlib imports such
# as `os`, `sys`, `Path` out of consumers' namespaces).
__all__ = [
    # colours
    "PURPLE",
    "RED",
    "ORANGE",
    "YELLOW",
    "GREEN",
    "CYAN",
    "BLUE",
    "MAGENTA",
    "GRAY",
    "BOLD",
    "DIM",
    "RESET",
    "NO_COLOR",
    # rainbow
    "rainbow",
    "inc_rainbow",
    # message-level formatters and printers
    "fmt_info",
    "fmt_success",
    "fmt_warning",
    "fmt_error",
    "fmt_step",
    "color_print",
    "info",
    "success",
    "warning",
    "error",
    "step",
    "fail",
    "ok",
    "warn",
    # progress bar
    "Progress",
    # rainbow pretty printers
    "pprint",
    "pptable",
    # filesystem / subprocess helpers
    "get_env_path_or",
    "ask_and_remove",
    "subprocess_run",
]

# ---------------------------------------------------------------------------
# Colours
# ---------------------------------------------------------------------------

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
DIM = "\033[2m"
RESET = "\033[0m"

# Disable colours when the output is not a terminal, or when NO_COLOR is set
# (https://no-color.org/).
NO_COLOR = not sys.stdout.isatty() or bool(os.environ.get("NO_COLOR"))
if NO_COLOR:
    PURPLE = RED = ORANGE = YELLOW = GREEN = CYAN = BLUE = MAGENTA = GRAY = BOLD = (
        DIM
    ) = RESET = ""


# ---------------------------------------------------------------------------
# Rainbow helpers
# ---------------------------------------------------------------------------

_rainbow = 0

_RAINBOW_COLORS = [
    "\033[38;5;197m",
    "\033[38;5;208m",
    "\033[38;5;220m",
    "\033[38;5;70m",
    "\033[38;5;74m",
    "\033[38;5;33m",
    "\033[38;5;127m",
]


def inc_rainbow() -> None:
    global _rainbow
    _rainbow += 1


def rainbow() -> str:
    if NO_COLOR:
        return ""
    return _RAINBOW_COLORS[_rainbow % len(_RAINBOW_COLORS)]


# ---------------------------------------------------------------------------
# Message levels
# ---------------------------------------------------------------------------

# A `Progress` bar currently being rendered, if any. Messages emitted while a bar
# is active are interleaved above it (the bar line is cleared and redrawn).
_active_progress: "Optional[Progress]" = None


def _emit(text: str, *, err: bool = False) -> None:
    """Print `text`, cooperating with an active progress bar."""
    if _active_progress is not None:
        _active_progress.log(text)
    else:
        print(text, file=sys.stderr if err else sys.stdout)


def fmt_info(message: str) -> str:
    return f"{CYAN}ℹ {message}{RESET}"


def fmt_success(message: str) -> str:
    return f"{GREEN}✓ {message}{RESET}"


def fmt_warning(message: str) -> str:
    return f"{YELLOW}⚠ {message}{RESET}"


def fmt_error(message: str) -> str:
    return f"{RED}✗ {message}{RESET}"


def fmt_step(message: str) -> str:
    return f"{BOLD}{BLUE}▶{RESET} {BOLD}{message}{RESET}"


def color_print(message: str, color: str = "") -> None:
    """Print a message with an optional colour, cooperating with a progress bar."""
    _emit(f"{color}{message}{RESET}")


def info(message: str) -> None:
    """Informational message (cyan ℹ)."""
    _emit(fmt_info(message))


def success(message: str) -> None:
    """Success message (green ✓)."""
    _emit(fmt_success(message))


def warning(message: str) -> None:
    """Warning message (yellow ⚠), on stderr."""
    _emit(fmt_warning(message), err=True)


def error(message: str) -> None:
    """Error message (red ✗), on stderr."""
    _emit(fmt_error(message), err=True)


def step(message: str) -> None:
    """Step header (bold blue ▶)."""
    _emit(fmt_step(message))


def fail(message: str) -> NoReturn:
    """Print an error message and exit with status 1."""
    error(message)
    sys.exit(1)


# Aliases kept for the call sites that used these names.
ok = success
warn = warning


# ---------------------------------------------------------------------------
# Progress bar
# ---------------------------------------------------------------------------


class Progress:
    """A minimal terminal progress bar. No-op when stdout is not a TTY.

    Usable as a context manager; while active, the message-level helpers
    (`info`, `success`, ...) print above the bar and redraw it.
    """

    BAR_WIDTH = 30

    def __init__(self, label: str, total: int) -> None:
        self.label = label
        self.total = total
        self.n = 0
        self.enabled = sys.stdout.isatty() and total > 0

    def __enter__(self) -> "Progress":
        global _active_progress
        _active_progress = self
        return self

    def __exit__(self, *_exc: object) -> bool:
        global _active_progress
        self.finish()
        _active_progress = None
        return False

    def _render(self) -> None:
        if not self.enabled:
            return
        filled = int(self.BAR_WIDTH * self.n / self.total)
        bar = "█" * filled + "░" * (self.BAR_WIDTH - filled)
        pct = int(100 * self.n / self.total)
        print(
            f"\r{BOLD}{self.label}{RESET} [{bar}] {self.n}/{self.total} ({pct}%)",
            end="",
            flush=True,
        )

    def advance(self, n: int = 1) -> None:
        """Advance the bar by n steps and redraw it."""
        self.n += n
        self._render()

    def log(self, message: str) -> None:
        """Print a message above the bar, then redraw the bar."""
        if self.enabled:
            print("\r\033[K", end="")  # clear the current bar line
        print(message)
        self._render()

    def finish(self) -> None:
        """Complete the bar and move to a fresh line."""
        if self.enabled:
            self.n = self.total
            self._render()
            print()


# ---------------------------------------------------------------------------
# Rainbow pretty printers
# ---------------------------------------------------------------------------


def pprint(*args, inc: bool = True, end: str = "\n", **kwargs) -> None:
    """Print a line prefixed with a rainbow bar, advancing the rainbow by default."""
    if NO_COLOR:
        print("| ", end="")
    else:
        clr = rainbow()
        print(f"{clr}|{RESET} ", end="")
    print(*args, **kwargs, end=end + RESET)
    if inc:
        inc_rainbow()


def pptable(rows: "list[list[tuple[str, Optional[str]]]]") -> None:
    """Display a table; each cell is a (text, optional colour) pair."""
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


# ---------------------------------------------------------------------------
# Filesystem helpers
# ---------------------------------------------------------------------------


def get_env_path_or(key: str, fallback: Path) -> Path:
    """Resolve an environment variable to a path, or fall back to `fallback`."""
    e = os.environ.get(key)
    if e:
        return Path(e).resolve()
    return fallback.resolve()


def ask_and_remove(path: Path) -> None:
    """Prompt for confirmation, then `rm -rf` the path if it exists."""
    if path.exists():
        response = input(f"Are you sure you want to remove '{path}'? (y/N): ")
        if response.lower() in ["y", "yes"]:
            subprocess.run(["rm", "-rf", str(path)])


# ---------------------------------------------------------------------------
# Subprocess helpers
# ---------------------------------------------------------------------------


def subprocess_run(
    *popenargs, input=None, capture_output=False, timeout=None, check=False, **kwargs
):
    """Patched `subprocess.run` that terminates with SIGTERM (not SIGKILL) on
    timeout, so the child gets a chance to clean up."""
    from subprocess import (
        PIPE,
        CalledProcessError,
        CompletedProcess,
        Popen,
        TimeoutExpired,
    )
    from typing import cast

    if input is not None:
        if kwargs.get("stdin") is not None:
            raise ValueError("stdin and input arguments may not both be used.")
        kwargs["stdin"] = PIPE

    if capture_output:
        if kwargs.get("stdout") is not None or kwargs.get("stderr") is not None:
            raise ValueError(
                "stdout and stderr arguments may not be used with capture_output."
            )
        kwargs["stdout"] = PIPE
        kwargs["stderr"] = PIPE

    with Popen(*popenargs, **kwargs) as process:
        try:
            stdout, stderr = process.communicate(input, timeout=timeout)
        except TimeoutExpired:
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
