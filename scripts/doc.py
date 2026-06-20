#!/usr/bin/env python3
"""Build the project documentation and present odoc's diagnostics readably.

This replaces the body of ``make doc``: it runs ``dune build @doc``, applies the
local odoc theme, then pretty-prints odoc's (otherwise very verbose) output —
one diagnostic per block, blocks separated by a blank line, coloured by kind,
with un-actionable noise filtered out.

By default the build is incremental (fast), but dune caches odoc's warnings and
stays silent on a no-op rebuild — so warnings in files you didn't just touch may
not show. Pass ``--nocache`` to force a from-scratch rebuild (in a throwaway
build directory, leaving the main ``_build/`` untouched) that always reports
every warning.

Usage:
    python3 scripts/doc.py
    python3 scripts/doc.py --open           # build, then open in browser
    python3 scripts/doc.py --nocache        # always report all warnings
    python3 scripts/doc.py --raw            # the unprocessed odoc logs
    python3 scripts/doc.py --no-simplify    # keep un-actionable noise
    dune build @doc 2>&1 | python3 scripts/doc.py --stdin   # parse only

The dune command can be overridden with the ``DUNE`` environment variable
(e.g. ``DUNE="opam exec -- dune"``), which is how the Makefile invokes it.
"""

from __future__ import annotations

import argparse
import itertools
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import threading
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
INDEX_HTML = REPO_ROOT / "_build" / "default" / "_doc" / "_html" / "index.html"

# ---------------------------------------------------------------------------
# FILTERING — diagnostics we cannot act on, and therefore hide.
#
# This section is intentionally self-contained so it is easy to tweak: each
# entry of ``FILTER_PATTERNS`` is a regex tested against a diagnostic's full
# text; a single match hides that diagnostic. To reveal a category again, just
# comment out its pattern. To hide a new category, add one.
# ---------------------------------------------------------------------------

# OCaml standard-library values/modules/exceptions. odoc has no documentation
# for the stdlib in our doc set, so references to them can never resolve — this
# is a limitation of odoc, not a mistake in our docs.
_STDLIB_NAMES = [
    # predefined exceptions
    "Invalid_argument",
    "Failure",
    "Not_found",
    "Out_of_memory",
    "Stack_overflow",
    "Sys_error",
    "End_of_file",
    "Division_by_zero",
    "Exit",
    "Match_failure",
    "Assert_failure",
    "Undefined_recursive_module",
    # predefined modules occasionally referenced
    "Buffer",
    "Stdlib",
]

FILTER_PATTERNS = [
    # (1) Unresolvable references to the OCaml standard library (see above).
    re.compile(r"""Couldn't find "(?:%s)\"""" % "|".join(_STDLIB_NAMES)),
    # (2) "Hidden constructors/fields" warnings. These come from types whose
    #     constructors are deliberately not exported (e.g. the hash-consed
    #     Svalue nodes, or PatriciaTree's internal `.view` types). Working as
    #     intended — there is nothing to fix.
    re.compile(r"Hidden (?:constructors|fields) in type"),
    # (3) odoc's own linking plumbing failing to find a page `.odoc` file. This
    #     is build-infrastructure noise, not a documentation problem.
    re.compile(r"odoc: FILE\.odoc argument"),
]


def is_filtered(text: str) -> bool:
    """Whether a diagnostic block should be hidden as un-actionable."""
    return any(pattern.search(text) for pattern in FILTER_PATTERNS)


# ---------------------------------------------------------------------------
# Parsing — split odoc's flat output into diagnostic blocks.
#
# odoc/dune emit each diagnostic as a `File "...":` line followed by one or
# more message lines, with no blank line in between, e.g.:
#
#     File "soteria/lib/foo.ml", line 89, characters 6-52:
#     Warning: Failed to resolve reference unresolvedroot(Bar) Couldn't find "Bar"
#
# so we start a fresh block at every `File "..."` line.
# ---------------------------------------------------------------------------

_FILE_RE = re.compile(r'^File "(?P<path>[^"]+)"(?:, line (?P<line>\d+))?')
# A real source location embedded in a message (used when the block's own
# `File` line points at an internal `.odoc` artefact rather than a source file).
_SRC_IN_MSG_RE = re.compile(r'at File "(?P<path>[^"]+)", line (?P<line>\d+)')


class Block:
    def __init__(self, header: str):
        self.header = header  # the `File "..."` line
        self.body: list[str] = []  # subsequent message lines

    @property
    def text(self) -> str:
        return "\n".join([self.header, *self.body])

    def location(self) -> str | None:
        """Best human-facing `path:line` for this diagnostic, if any."""
        m = _FILE_RE.match(self.header)
        path = m.group("path") if m else None
        line = m.group("line") if m else None
        # Prefer a real source file mentioned in the body over an `.odoc`
        # artefact path in the header.
        if path is None or path.endswith(".odoc"):
            for bl in self.body:
                sm = _SRC_IN_MSG_RE.search(bl)
                if sm:
                    path, line = sm.group("path"), sm.group("line")
                    break
        if path is None:
            return None
        # odoc reports paths relative to its deep build cwd (e.g.
        # "../../../../soteria/lib/foo.ml"); strip the leading "../" so the path
        # is relative to the repo root and stays click-to-open in a terminal.
        path = re.sub(r"^(?:\.\.?/)+", "", path)
        return f"{path}:{line}" if line else path


def parse_blocks(output: str) -> tuple[list[str], list[Block]]:
    """Return (preamble_lines, blocks). Preamble is anything before the first
    `File` line — usually empty, but can hold a bare dune error."""
    preamble: list[str] = []
    blocks: list[Block] = []
    current: Block | None = None
    for raw in output.splitlines():
        if _FILE_RE.match(raw):
            current = Block(raw)
            blocks.append(current)
        elif current is None:
            preamble.append(raw)
        else:
            current.body.append(raw)
    return preamble, blocks


# ---------------------------------------------------------------------------
# Classification & colouring.
# ---------------------------------------------------------------------------


def use_colour() -> bool:
    return sys.stdout.isatty() and os.environ.get("NO_COLOR") is None


class C:
    RESET = "\033[0m"
    DIM = "\033[2m"
    BOLD = "\033[1m"
    RED = "\033[31m"
    BRED = "\033[1;31m"
    YELLOW = "\033[33m"
    MAGENTA = "\033[35m"
    CYAN = "\033[36m"


# kind -> (label, colour)
_KINDS = {
    "error": ("error", C.BRED),
    "unresolved": ("unresolved reference", C.YELLOW),
    "ambiguous": ("ambiguous reference", C.MAGENTA),
    "hidden": ("hidden type", C.DIM),
    "odoc": ("odoc", C.DIM),
    "other": ("warning", C.CYAN),
}


def classify(text: str) -> str:
    if re.search(r"(^|\n)Error:", text):
        return "error"
    if "Failed to resolve reference" in text:
        return "unresolved"
    if "is ambiguous" in text:
        return "ambiguous"
    if "Hidden constructors" in text or "Hidden fields" in text:
        return "hidden"
    if "odoc:" in text or "Usage: odoc link" in text:
        return "odoc"
    return "other"


def _paint(s: str, colour: str, on: bool) -> str:
    return f"{colour}{s}{C.RESET}" if on else s


def render(block: Block, colour: bool) -> str:
    kind = classify(block.text)
    label, col = _KINDS[kind]
    loc = block.location()
    # Headline: the diagnostic kind, plus its location.
    head = _paint(label, col, colour)
    if loc:
        head += "  " + _paint(loc, C.DIM, colour)
    # Body: the message lines, with the noisy `Warning:`/`File ...:` framing
    # stripped, indented under the headline.
    msg_lines: list[str] = []
    for bl in [block.header, *block.body]:
        bl = bl.rstrip()
        if not bl or _FILE_RE.match(bl):
            continue
        bl = re.sub(r"^Warning:\s*", "", bl)
        bl = re.sub(r"^Error:\s*", "", bl)
        # Drop the "While resolving the expansion of include at File ..." noise:
        # the useful part is the actual warning that follows it.
        if bl.startswith("While resolving the expansion of include"):
            continue
        msg_lines.append(bl)
    body = "\n".join("    " + ln for ln in msg_lines)
    return head + ("\n" + body if body else "")


# ---------------------------------------------------------------------------
# Driver.
# ---------------------------------------------------------------------------


def dune_cmd() -> list[str]:
    return shlex.split(os.environ.get("DUNE", "dune"))


class Spinner:
    """A tiny braille spinner shown on stderr while a slow step runs. Disables
    itself when stderr is not a terminal (pipes, CI), so logs stay clean."""

    FRAMES = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

    def __init__(self, message: str):
        self.message = message
        self.enabled = sys.stderr.isatty() and os.environ.get("NO_COLOR") is None
        self._stop = threading.Event()
        self._thread: threading.Thread | None = None

    def _spin(self) -> None:
        for frame in itertools.cycle(self.FRAMES):
            if self._stop.is_set():
                break
            sys.stderr.write(f"\r{frame} {self.message}")
            sys.stderr.flush()
            self._stop.wait(0.08)

    def __enter__(self) -> "Spinner":
        if self.enabled:
            self._thread = threading.Thread(target=self._spin, daemon=True)
            self._thread.start()
        return self

    def __exit__(self, *exc) -> None:
        self._stop.set()
        if self._thread is not None:
            self._thread.join()
            sys.stderr.write("\r\033[K")  # erase the spinner line
            sys.stderr.flush()


def build_docs() -> tuple[int, str]:
    """A normal, incremental ``dune build @doc`` in the main ``_build/``.

    Fast, but dune caches action results (including their stderr), so a no-op
    rebuild stays silent and hides warnings in files that didn't just change.
    Use {!build_docs_forced} (``--nocache``) to always see every warning."""
    cmd = [*dune_cmd(), "build", "@doc"]
    proc = subprocess.run(
        cmd,
        cwd=REPO_ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    return proc.returncode, proc.stdout


def build_docs_forced() -> tuple[int, str]:
    """A from-scratch documentation build, so odoc re-runs and re-emits *all*
    its warnings (not just the cached ones for recently-changed files).

    dune won't re-run odoc unless its inputs change, and neither ``--force`` nor
    deleting the odoc artefacts defeats that — the robust trigger is a cold
    build, which we do in a throwaway build directory. Building there (rather
    than running ``dune clean``) leaves the user's ``_build/`` — and hence their
    normal, incremental dev build — completely untouched.

    The throwaway dir sits next to ``_build/`` (same filesystem, and dot-dirs
    are ignored by dune) so that publishing the freshly built docs back is a
    cheap rename rather than a 70 MB+ copy."""
    force_dir = Path(tempfile.mkdtemp(prefix=".doc-force-build-", dir=REPO_ROOT))
    try:
        cmd = [*dune_cmd(), "build", "@doc", "--build-dir", str(force_dir)]
        proc = subprocess.run(
            cmd,
            cwd=REPO_ROOT,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
        publish_docs(force_dir / "default" / "_doc")
        return proc.returncode, proc.stdout
    finally:
        shutil.rmtree(force_dir, ignore_errors=True)


def publish_docs(built_doc: Path) -> None:
    """Move the freshly built `_doc` tree into the conventional location under
    `_build/`, so the generated HTML stays browsable where users expect it."""
    if not built_doc.exists():
        return
    dst = REPO_ROOT / "_build" / "default" / "_doc"
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.rmtree(dst, ignore_errors=True)
    shutil.move(str(built_doc), str(dst))


def open_index() -> None:
    """Open the generated documentation in the default browser/handler."""
    if not INDEX_HTML.exists():
        return
    opener = "open" if sys.platform == "darwin" else "xdg-open"
    try:
        subprocess.run([opener, str(INDEX_HTML)], check=False)
    except FileNotFoundError:
        pass  # no opener available (e.g. headless CI); the path is still printed


def apply_theme() -> None:
    css = REPO_ROOT / "_build/default/_doc/_html/odoc.support/odoc.css"
    theme = REPO_ROOT / "doc/odoc-theme/odoc.css"
    if css.exists() and theme.exists():
        css.chmod(0o644)
        shutil.copyfile(theme, css)


def report(output: str, colour: bool, simplify: bool = True) -> None:
    preamble, blocks = parse_blocks(output)

    if any(ln.strip() for ln in preamble):
        print("\n".join(preamble).rstrip())
        print()

    shown = 0
    hidden = 0
    for block in blocks:
        if simplify and is_filtered(block.text):
            hidden += 1
            continue
        print(render(block, colour))
        print()
        shown += 1

    if simplify:
        summary = f"{shown} diagnostic(s) shown, {hidden} hidden (un-actionable)."
    else:
        summary = f"{shown} diagnostic(s) shown (no filtering)."
    print(_paint(summary, C.BOLD, colour))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--raw",
        action="store_true",
        help="print odoc's raw, unprocessed logs instead of the tidy report",
    )
    parser.add_argument(
        "--no-simplify",
        action="store_true",
        help="keep un-actionable warnings (disables the filtering step)",
    )
    parser.add_argument(
        "--nocache",
        action="store_true",
        help="force a from-scratch rebuild so every odoc warning is reported "
        "(dune otherwise caches them and stays silent on a no-op build)",
    )
    parser.add_argument(
        "--open",
        dest="open_after",
        action="store_true",
        help="open the generated documentation in the browser afterwards",
    )
    parser.add_argument(
        "--stdin",
        action="store_true",
        help="read odoc logs from stdin instead of running dune (no build)",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    colour = use_colour()

    if args.stdin:
        output, code = sys.stdin.read(), 0
    elif args.nocache:
        with Spinner("Force-rebuilding documentation (dune build @doc)…"):
            code, output = build_docs_forced()
        apply_theme()
    else:
        with Spinner("Building documentation (dune build @doc)…"):
            code, output = build_docs()
        apply_theme()

    if args.raw:
        sys.stdout.write(output)
    else:
        report(output, colour, simplify=not args.no_simplify)
        if not args.stdin and INDEX_HTML.exists():
            print(_paint("Documentation:", C.BOLD, colour) + " " + str(INDEX_HTML))

    if not args.stdin and args.open_after:
        open_index()

    if code != 0:
        print(
            _paint(f"\ndune build @doc exited with code {code}.", C.BRED, colour),
            file=sys.stderr,
        )
    return code


if __name__ == "__main__":
    raise SystemExit(main())
