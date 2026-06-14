#!/usr/bin/env python3
"""Diff two conformance-outcomes.csv files and emit a markdown list of the
per-test outcome changes, for the PR benchmark comment.

Both inputs are the sorted `suite,file,outcome` CSVs produced by
run_benchmarks.py (--conformance-csv). The "before" is main's published copy
(from gh-pages); the "after" is this PR's run. Each changed test is shown as
`before -> after` with an emoji conveying whether the change is good:

  🔴 regression  (was passing, now isn't)
  🟢 fixed       (now passing, wasn't)
  🟡 changed     (still not passing, but a different outcome)
  🆕 added       (test only present after — e.g. a suite bump)
  🗑️ removed     (test only present before)
"""
from __future__ import annotations

import argparse
import csv
from collections import Counter
from pathlib import Path
from typing import Optional

# Cap the listed rows so a large change set (e.g. a suite-pin bump) can't bloat
# the PR comment; the count line still reports the true total.
MAX_ROWS = 50

# kind -> (emoji, sort priority, summary label). Lower priority sorts first, so
# regressions lead.
KINDS = {
    "regression": ("🔴", 0, "🔴 regressed"),
    "changed": ("🟡", 1, "🟡 changed"),
    "fixed": ("🟢", 2, "🟢 fixed"),
    "added": ("🆕", 3, "🆕 added"),
    "removed": ("🗑️", 4, "🗑️ removed"),
}


def classify(before: Optional[str], after: Optional[str]) -> Optional[str]:
    if before == after:
        return None
    if before is None:
        return "added"
    if after is None:
        return "removed"
    if before == "pass":
        return "regression"
    if after == "pass":
        return "fixed"
    return "changed"


def load(path: Optional[Path]) -> dict[tuple[str, str], str]:
    out: dict[tuple[str, str], str] = {}
    if not path or not path.exists() or path.stat().st_size == 0:
        return out
    with path.open(newline="") as f:
        reader = csv.reader(f)
        next(reader, None)  # header: suite,file,outcome
        for row in reader:
            if len(row) >= 3:
                out[(row[0], row[1])] = row[2]
    return out


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--before", type=Path, help="main's conformance-outcomes.csv")
    ap.add_argument("--after", type=Path, required=True, help="this run's CSV")
    ap.add_argument("--output", type=Path, required=True)
    args = ap.parse_args()

    before = load(args.before)
    after = load(args.after)

    lines = ["### Conformance changes", ""]

    if not before:
        lines.append("_No main baseline to compare conformance outcomes against._")
        args.output.write_text("\n".join(lines) + "\n")
        return

    changes = []
    for key in set(before) | set(after):
        kind = classify(before.get(key), after.get(key))
        if kind is not None:
            changes.append((key[0], key[1], before.get(key), after.get(key), kind))

    if not changes:
        lines.append("✅ _No conformance test outcomes changed._")
        args.output.write_text("\n".join(lines) + "\n")
        return

    counts = Counter(c[4] for c in changes)
    summary = ", ".join(
        f"{counts[kind]} {KINDS[kind][2]}" for kind in KINDS if counts.get(kind)
    )
    lines.append(f"{len(changes)} test(s) changed: {summary}.")
    lines.append("")

    changes.sort(key=lambda c: (KINDS[c[4]][1], c[0], c[1]))
    for suite, file, b, a, kind in changes[:MAX_ROWS]:
        emoji = KINDS[kind][0]
        if kind == "added":
            transition = f"_added_ → {a}"
        elif kind == "removed":
            transition = f"{b} → _removed_"
        else:
            transition = f"{b} → {a}"
        lines.append(f"- {emoji} `{suite}/{file}`: {transition}")
    if len(changes) > MAX_ROWS:
        lines.append(f"- … and {len(changes) - MAX_ROWS} more")

    args.output.write_text("\n".join(lines) + "\n")


if __name__ == "__main__":
    main()
