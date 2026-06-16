#!/usr/bin/env python3
"""Run Soteria benchmarks described in a benchmarks.json file.

The output is a JSON array in the format expected by
benchmark-action/github-action-benchmark with `tool: customSmallerIsBetter`:

    [ { "name": "...", "unit": "s", "value": <mean>, "range": "± <stddev>" }, ... ]

Four kinds of benchmarks are supported, configured under the matching key in
benchmarks.json (every entry accepts `args` and `no_hyperfine`, see below):

  - "rust_files":   a single .rs file, compiled once then run with --no-compile
  - "rust_crates":  a crate root, compiled once then run with --no-compile
  - "c_wpst":      a C file run in wpst mode (`soteria-c exec`)
  - "c_biab":   a C project run in bi-abduction, with `mode` set to either
                    "gen-summaries" or "capture-db"

With --conformance, an additional Rust *conformance* benchmark runs the Kani
and Miri test suites through soteria-rust (via soteria-rust/scripts/test.py) and
emits, per suite, the passed/failed/unsupported/timed-out test counts and the
total time. It is not configured in benchmarks.json; CI must clone the suites
and set KANI_SUITE_PATH / MIRI_SUITE_PATH first.

Per-entry fields:
  - "path"          (required) file or project root, relative to the repo root
  - "name"          (optional) label used in the report; defaults to the path
  - "args"          (optional) list of extra arguments passed to the tool
  - "no_hyperfine"  (optional) if true, time a single run instead of using
                    hyperfine (use for benchmarks too long to run repeatedly)
  - "mode"          (c_biab only) "gen-summaries" or "capture-db"
  - "compile_commands" (c_biab/capture-db only) path to compile_commands.json
                    relative to the project root; defaults to
                    "build/compile_commands.json", generated with cmake if absent
  - "cmake_args"    (c_biab/capture-db only) extra args for the cmake
                    invocation used to generate the compilation database
"""

import argparse
import csv
import importlib.util
import json
import os
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Callable, Optional

from soteria_utils import GRAY, RESET

SCRIPTS_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPTS_DIR.parent

SOTERIA_C = shutil.which("soteria-c") or "soteria-c"
SOTERIA_RUST = shutil.which("soteria-rust") or "soteria-rust"

HYPERFINE_WARMUP = 1
HYPERFINE_RUNS = 10


def log(msg: str) -> None:
    print(f"{GRAY}[benchmarks]{RESET} {msg}", flush=True)


def run(cmd: list[str], cwd: Optional[Path] = None, check: bool = False) -> int:
    """Run a command, streaming output. Returns the exit code."""
    log(f"$ {shlex.join(cmd)}")
    result = subprocess.run(cmd, cwd=cwd)
    if check and result.returncode != 0:
        raise SystemExit(
            f"command failed with exit code {result.returncode}: {shlex.join(cmd)}"
        )
    return result.returncode


def measure(cmd: list[str], no_hyperfine: bool, cwd: Optional[Path] = None) -> dict:
    """Measure `cmd` and return {"value": seconds, "range": "± stddev" | None}.

    Soteria tools exit non-zero when they find a bug, which is expected here, so
    failures are tolerated (hyperfine `-i`, ignored return code otherwise).
    """
    if no_hyperfine:
        log(f"timing single run: {shlex.join(cmd)}")
        start = time.perf_counter()
        run(cmd, cwd=cwd)
        elapsed = time.perf_counter() - start
        return {"value": elapsed, "range": None}

    if shutil.which("hyperfine") is None:
        raise SystemExit(
            'hyperfine not found in PATH; install it or set "no_hyperfine": true'
        )

    with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as tmp:
        export_path = Path(tmp.name)
    hf = [
        "hyperfine",
        "--warmup",
        str(HYPERFINE_WARMUP),
        "--runs",
        str(HYPERFINE_RUNS),
        "-i",
        "--export-json",
        str(export_path),
        "--",
        shlex.join(cmd),
    ]
    run(hf, cwd=cwd, check=True)
    data = json.loads(export_path.read_text())["results"][0]
    export_path.unlink(missing_ok=True)
    return {"value": data["mean"], "range": f"± {data['stddev']:.4f}"}


def resolve(path: str) -> Path:
    p = Path(path)
    return p if p.is_absolute() else (REPO_ROOT / p)


def bench_rust(entry: dict, kind: str) -> dict:
    soteria_rust = os.environ.get("SOTERIA_RUST", SOTERIA_RUST)
    target = resolve(entry["path"])
    args = entry.get("args", [])
    base = [soteria_rust, "exec", str(target), *args]

    # Compile once (untimed); analysis may exit non-zero on a found bug.
    log(f"pre-compiling {kind}: {target}")
    run(base)

    cmd = [soteria_rust, "exec", str(target), "--no-compile", *args]
    stats = measure(cmd, entry.get("no_hyperfine", False))
    return make_result(entry, kind, stats)


def bench_c_file(entry: dict) -> dict:
    soteria_c = os.environ.get("SOTERIA_C", SOTERIA_C)
    target = resolve(entry["path"])
    args = entry.get("args", [])
    cmd = [soteria_c, "exec", str(target), *args]
    stats = measure(cmd, entry.get("no_hyperfine", False))
    return make_result(entry, "c", stats)


def bench_c_project(entry: dict) -> dict:
    soteria_c = os.environ.get("SOTERIA_C", SOTERIA_C)
    root = resolve(entry["path"])
    args = entry.get("args", [])
    mode = entry.get("mode")
    if mode not in ("gen-summaries", "capture-db"):
        raise SystemExit(
            f'c_projects entry {entry["path"]!r} needs "mode": '
            '"gen-summaries" or "capture-db"'
        )

    if mode == "gen-summaries":
        c_files = sorted(str(p) for p in root.rglob("*.c"))
        if not c_files:
            raise SystemExit(f"no .c files found under {root}")
        cmd = [soteria_c, "gen-summaries", *c_files, *args]
    else:
        rel_db = entry.get("compile_commands", "build/compile_commands.json")
        db = (root / rel_db).resolve()
        if not db.exists():
            log(f"compile_commands.json missing, generating: {db}")
            run(
                [
                    "cmake",
                    "-S",
                    str(root),
                    "-B",
                    str(db.parent),
                    "-DCMAKE_EXPORT_COMPILE_COMMANDS=1",
                    *entry.get("cmake_args", []),
                ],
                check=True,
            )
        if not db.exists():
            raise SystemExit(f"failed to produce compilation database at {db}")
        cmd = [soteria_c, "capture-db", str(db), *args]

    stats = measure(cmd, entry.get("no_hyperfine", False))
    return make_result(entry, f"c-{mode}", stats)


def make_result(entry: dict, kind: str, stats: dict) -> dict:
    name = entry.get("name") or entry["path"]
    result = {"name": f"{kind}: {name}", "unit": "s", "value": stats["value"]}
    if stats["range"] is not None:
        result["range"] = stats["range"]
    log(f"-> {result['name']}: {result['value']:.4f}s {stats['range'] or ''}")
    return result


# --- Rust conformance benchmark ------------------------------------------
#
# Unlike the performance benchmarks above, this is a *conformance* benchmark:
# it runs soteria-rust over the Kani and Miri test suites (which CI clones
# beforehand) and tracks how many tests pass/fail/are unsupported/time out,
# plus the total time spent. The running and aggregation live in
# soteria-rust/scripts/test.py (`soteria_conformance`); here we only shape its
# per-suite summary into benches. Bench names are prefixed "conformance-<suite>:"
# so the dashboard can group them into their own section.

CONFORMANCE_SUITES = ("kani", "miri")

# A small tail of slow tests dominates each suite's runtime (see benchmark.csv:
# kani's median is ~0.1s but a handful of tests run for 5-23s). Cap each test so
# the benchmark finishes in a few minutes rather than ~10; the cost is a few
# extra "timed out" outcomes, which is fine since timeouts are tracked anyway.
CONFORMANCE_TIMEOUT_S = 3

# soteria_conformance() summary key -> dashboard label, for the count benches.
CONFORMANCE_OUTCOMES = {
    "passed": "passed",
    "failed": "failed",
    "unsupported": "unsupported",
    "timed_out": "timed out",
}


def load_soteria_test():
    """Import soteria-rust/scripts/test.py as a module.

    test.py and its siblings (cliopts, common, config, ...) import each other by
    bare name, so their directory must be on sys.path. We load test.py under a
    distinct module name to avoid clashing with the stdlib `test` package.
    """
    rust_scripts = REPO_ROOT / "soteria-rust" / "scripts"
    if str(rust_scripts) not in sys.path:
        sys.path.insert(0, str(rust_scripts))
    spec = importlib.util.spec_from_file_location(
        "soteria_rust_test", rust_scripts / "test.py"
    )
    if spec is None or spec.loader is None:
        raise SystemExit(f"could not load {rust_scripts / 'test.py'}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_conformance_outcomes(src_csv: Path, dest_csv: Path) -> None:
    """Write a sorted `suite,file,outcome` CSV from test.py's benchmark.csv.

    Dropping the per-test times and sorting the rows makes this file diff
    cleanly: committed to gh-pages, its git history gains a commit only when a
    test's outcome actually changes, so before/after regressions are obvious.
    """
    with src_csv.open(newline="") as f:
        rows = list(csv.reader(f))
    # benchmark.csv header: Suite,File,Soteria,(s),Kani,(s),Miri,(s)
    # We keep the suite, file, and Soteria outcome (the only tool run here).
    body = sorted((r[0], r[1], r[2]) for r in rows[1:] if len(r) >= 3)
    with dest_csv.open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["suite", "file", "outcome"])
        writer.writerows(body)


def bench_conformance(csv_dest: Optional[Path] = None) -> list[dict]:
    """Run the Kani/Miri conformance suites and return one list of benches.

    Requires KANI_SUITE_PATH and MIRI_SUITE_PATH to point at the cloned suites
    (test.py reads them); CI sets these up. test.py writes its CSV/logs to
    OUTPUT_DIR, which we point at a temp dir to keep the repo clean. When
    `csv_dest` is given, the per-test outcomes are also written there (for
    committing as a diffable regression record).
    """
    if "OUTPUT_DIR" not in os.environ:
        os.environ["OUTPUT_DIR"] = tempfile.mkdtemp(prefix="soteria-conformance-")
    summary = load_soteria_test().soteria_conformance(CONFORMANCE_TIMEOUT_S)

    if csv_dest is not None:
        src = Path(os.environ["OUTPUT_DIR"]) / "benchmark.csv"
        if src.exists():
            write_conformance_outcomes(src, csv_dest)
            log(f"wrote conformance outcomes to {csv_dest}")
        else:
            log(f"WARNING: no conformance CSV at {src}; skipping {csv_dest}")

    results: list[dict] = []
    for suite in CONFORMANCE_SUITES:
        suite_summary = summary.get(suite, {})
        for key, label in CONFORMANCE_OUTCOMES.items():
            results.append(
                {
                    "name": f"conformance-{suite}: {label}",
                    "unit": "tests",
                    "value": suite_summary.get(key, 0),
                }
            )
        results.append(
            {
                "name": f"conformance-{suite}: total time",
                "unit": "s",
                "value": round(suite_summary.get("total_time", 0.0), 4),
            }
        )
        log(
            f"-> conformance-{suite}: "
            + ", ".join(
                f"{lbl} {suite_summary.get(k, 0)}"
                for k, lbl in CONFORMANCE_OUTCOMES.items()
            )
            + f", total time {suite_summary.get('total_time', 0.0):.2f}s"
        )
    return results


# Maps each benchmarks.json section to the function that runs one of its
# entries; iteration order is the order benchmarks run in.
SECTIONS: dict[str, Callable[[dict], dict]] = {
    "rust_files": lambda e: bench_rust(e, "rust-file"),
    "rust_crates": lambda e: bench_rust(e, "rust-crate"),
    "c_wpst": bench_c_file,
    "c_biab": bench_c_project,
}


def main() -> None:
    parser = argparse.ArgumentParser(description="Run Soteria benchmarks.")
    parser.add_argument(
        "--config",
        type=Path,
        default=SCRIPTS_DIR / "benchmarks.json",
        help="Path to benchmarks.json (default: scripts/benchmarks.json)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=REPO_ROOT / "benchmark-results.json",
        help="Where to write the github-action-benchmark JSON",
    )
    parser.add_argument(
        "--keep-going",
        action="store_true",
        help="Continue with remaining benchmarks if one fails",
    )
    parser.add_argument(
        "--conformance",
        action="store_true",
        help=(
            "Also run the Rust conformance benchmark over the Kani/Miri suites "
            "(requires KANI_SUITE_PATH and MIRI_SUITE_PATH; see test.py)"
        ),
    )
    parser.add_argument(
        "--conformance-csv",
        type=Path,
        default=None,
        help=(
            "Write a sorted suite,file,outcome CSV of the conformance run here, "
            "for committing as a diffable record of per-test outcome changes"
        ),
    )
    parsed = parser.parse_args()

    config = json.loads(parsed.config.read_text())
    results: list[dict] = []

    plan = [
        (section, entry, fn)
        for section, fn in SECTIONS.items()
        for entry in config.get(section, [])
    ]

    if not plan and not parsed.conformance:
        raise SystemExit(f"no benchmarks configured in {parsed.config}")

    failures = 0
    for kind, entry, fn in plan:
        log(f"=== {kind}: {entry.get('name') or entry['path']} ===")
        try:
            results.append(fn(entry))
        except Exception as exc:  # noqa: BLE001 - we want to report and continue
            failures += 1
            log(f"FAILED ({kind} {entry.get('path')!r}): {exc}")
            if not parsed.keep_going:
                raise

    if parsed.conformance:
        log("=== conformance: Kani/Miri suites ===")
        try:
            results.extend(bench_conformance(parsed.conformance_csv))
        except Exception as exc:  # noqa: BLE001 - report and continue
            failures += 1
            log(f"FAILED (conformance): {exc}")
            if not parsed.keep_going:
                raise

    parsed.output.write_text(json.dumps(results, indent=2))
    log(f"wrote {len(results)} result(s) to {parsed.output}")
    if failures and not parsed.keep_going:
        sys.exit(1)


if __name__ == "__main__":
    main()
