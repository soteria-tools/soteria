#!/usr/bin/env python3
"""Build a markdown comparison table between a PR's benchmark results and the
most recent run on main.

Reads:
  --current      JSON list produced by run_benchmarks.py: [{name, unit, value, range?, ...}]
  --main-data    main's `data.js` (from gh-pages). May be missing or empty.
  --output       markdown table written here.

The table is robust:
  * if `--main-data` is missing/empty/unparseable, every benchmark renders as "new"
  * if a current benchmark is absent from main, it renders as "new" (no crash)
  * benchmarks present on main but absent in the current run are not listed
"""
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path
from typing import Optional

DATA_JS_RE = re.compile(r"^window\.BENCHMARK_DATA\s*=\s*(\{.*\})\s*;\s*$", re.DOTALL)

# Canonical workflow name written by benchmarks.yml. Older history may live
# under a different key — fall back to whichever entry is present.
DEFAULT_ENTRY = "Soteria benchmarks"

# Bold the percentage when its absolute value exceeds this threshold, to make
# meaningful regressions/improvements jump out from sub-noise variation.
NOTABLE_PCT = 10.0


def parse_baseline(path: Path) -> dict[str, float]:
    """Return {name: value} from the last run on main, or {} if unavailable."""
    if not path.exists() or path.stat().st_size == 0:
        return {}
    m = DATA_JS_RE.match(path.read_text())
    if not m:
        return {}
    try:
        data = json.loads(m.group(1))
    except json.JSONDecodeError:
        return {}
    entries = data.get("entries", {}) or {}
    runs = entries.get(DEFAULT_ENTRY) or next(iter(entries.values()), [])
    if not runs:
        return {}
    last = runs[-1]
    return {
        b["name"]: b["value"]
        for b in last.get("benches", [])
        if "name" in b and "value" in b
    }


def fmt_time(v: float) -> str:
    return f"{v * 1000:.1f} ms" if v < 1 else f"{v:.3f} s"


def fmt_delta(old: Optional[float], new: float) -> str:
    if old is None:
        return "**new**"
    if old == 0:
        return "n/a"
    pct = (new - old) / old * 100
    # Color reflects semantics, not direction: negative pct = faster = good.
    # The signed percentage itself carries the direction.
    marker = "🔴" if pct > 0 else ("🟢" if pct < 0 else "⚪")
    value = f"{pct:+.1f}%"
    if abs(pct) > NOTABLE_PCT:
        value = f"**{value}**"
    return f"{marker} {value}"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--current", type=Path, required=True)
    ap.add_argument("--main-data", type=Path, required=True)
    ap.add_argument("--output", type=Path, required=True)
    args = ap.parse_args()

    current = json.loads(args.current.read_text())
    baseline = parse_baseline(args.main_data)

    if not current:
        args.output.write_text("_No benchmark results produced._\n")
        return

    header = "_No main baseline available — every row is reported as new._\n\n" if not baseline else ""

    lines: list[str] = [
        header,
        "| Benchmark | main | this PR | Δ |",
        "| --- | ---: | ---: | ---: |",
    ]
    for entry in current:
        name = entry["name"]
        new_val = entry["value"]
        old_val = baseline.get(name)
        lines.append(
            "| {} | {} | {} | {} |".format(
                name,
                fmt_time(old_val) if old_val is not None else "—",
                fmt_time(new_val),
                fmt_delta(old_val, new_val),
            )
        )

    args.output.write_text("\n".join(lines).strip() + "\n")
    print(f"[compare] wrote {args.output} ({len(current)} benchmark(s))")


if __name__ == "__main__":
    main()
