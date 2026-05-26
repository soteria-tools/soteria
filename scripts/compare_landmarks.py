#!/usr/bin/env python3
"""Compare two OCaml Landmarks JSON profiles and report what got faster/slower.

Usage:
    python3 compare_landmarks.py before.json after.json [--top N] [--min-ms MS]

The JSON files are produced by running with OCAML_LANDMARKS=format=json.
The program output (non-JSON lines) before the JSON object is skipped automatically.
"""

import json
import sys
import argparse
from collections import defaultdict


def load_profile(path: str) -> dict:
    """Load landmarks JSON, skipping any non-JSON header lines."""
    with open(path) as f:
        content = f.read()
    # Find the start of the JSON object
    idx = content.find("{")
    if idx == -1:
        raise ValueError(f"No JSON object found in {path}")
    return json.loads(content[idx:])


def compute_exclusive_times(nodes: list[dict]) -> dict[int, float]:
    """Return exclusive time per node id (time minus sum of direct children's time)."""
    id_to_node = {n["id"]: n for n in nodes}
    exclusive = {}
    for node in nodes:
        children_time = sum(
            id_to_node[cid]["time"] for cid in node["children"] if cid in id_to_node
        )
        exclusive[node["id"]] = max(0.0, node["time"] - children_time)
    return exclusive


def aggregate_by_name(nodes: list[dict], exclusive: dict[int, float]) -> dict:
    """Aggregate nodes by name, summing time and calls across all call-tree instances.

    landmark_id is a hash that changes on recompilation, so it can't be used to
    match the same function across two different builds. name (fully qualified) is stable.
    """
    agg = defaultdict(lambda: {"location": "", "calls": 0, "excl_ns": 0.0})
    for node in nodes:
        if not node["landmark_id"]:
            continue
        name = node["name"]
        entry = agg[name]
        entry["location"] = node["location"]
        entry["calls"] += node["calls"]
        entry["excl_ns"] += exclusive[node["id"]]
    return dict(agg)


def profile_stats(path: str) -> tuple[dict, float]:
    """Return (aggregated stats by name, total_time_ns)."""
    data = load_profile(path)
    nodes = data["nodes"]
    exclusive = compute_exclusive_times(nodes)
    agg = aggregate_by_name(nodes, exclusive)
    root = next(n for n in nodes if n["kind"] == "root")
    return agg, root["time"]



def format_ms(ns: float) -> str:
    return f"{ns / 1e6:>10.1f} ms"


def format_pct(value: float, total: float) -> str:
    if total == 0:
        return "    -   "
    return f"{100 * value / total:>6.1f}%"


def format_delta(delta_ns: float) -> str:
    sign = "+" if delta_ns >= 0 else "-"
    return f"{sign}{abs(delta_ns) / 1e6:.1f} ms"


def format_rel(before: float, after: float) -> str:
    if before == 0:
        return "     new"
    pct = 100 * (after - before) / before
    sign = "+" if pct >= 0 else ""
    return f"{sign}{pct:.1f}%"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("before", help="JSON profile from the baseline run")
    parser.add_argument("after", help="JSON profile from the modified run")
    parser.add_argument("--top", type=int, default=40,
                        help="Number of functions to show (default: 40)")
    parser.add_argument("--min-ms", type=float, default=1.0,
                        help="Minimum exclusive time in ms to include (default: 1.0)")
    args = parser.parse_args()

    before_stats, before_total = profile_stats(args.before)
    after_stats, after_total = profile_stats(args.after)

    all_names = set(before_stats) | set(after_stats)

    rows = []
    for name in all_names:
        b = before_stats.get(name)
        a = after_stats.get(name)
        location = (a or b)["location"]
        b_ns = b["excl_ns"] if b else 0.0
        a_ns = a["excl_ns"] if a else 0.0
        b_calls = b["calls"] if b else 0
        a_calls = a["calls"] if a else 0
        delta_ns = a_ns - b_ns
        max_ns = max(b_ns, a_ns)
        if max_ns < args.min_ms * 1e6:
            continue
        rows.append({
            "name": name,
            "location": location,
            "b_ns": b_ns,
            "a_ns": a_ns,
            "b_calls": b_calls,
            "a_calls": a_calls,
            "delta_ns": delta_ns,
            "max_ns": max_ns,
        })

    # Sort by abs(delta) * max_time so that large changes on tiny functions
    # don't crowd out small changes on heavy functions.
    rows.sort(key=lambda r: -abs(r["delta_ns"]) * r["max_ns"])
    rows = rows[: args.top]

    before_s = before_total / 1e9
    after_s = after_total / 1e9
    speedup = before_s / after_s if after_s else 0
    print(f"Total wall time:  before={before_s:.2f}s  after={after_s:.2f}s  "
          f"delta={after_s - before_s:+.2f}s  speedup={speedup:.2f}x")
    print()

    col_name = 48
    header = (
        f"{'Function':<{col_name}}  "
        f"{'Before':>10}  {'After':>10}  {'Delta':>12}  {'Rel':>8}  "
        f"{'Calls B':>8}  {'Calls A':>8}"
    )
    print(header)
    print("-" * len(header))

    use_color = sys.stdout.isatty()

    for r in rows:
        display = r["name"]
        if len(display) > col_name:
            display = display[: col_name - 1] + "…"
        rel = format_rel(r["b_ns"], r["a_ns"])
        if use_color and r["delta_ns"] < -1e6:
            color, reset = "\033[32m", "\033[0m"
        elif use_color and r["delta_ns"] > 1e6:
            color, reset = "\033[31m", "\033[0m"
        else:
            color, reset = "", ""
        line = (
            f"{display:<{col_name}}  "
            f"{format_ms(r['b_ns'])}  {format_ms(r['a_ns'])}  "
            f"{format_delta(r['delta_ns']):>12}  {rel:>8}  "
            f"{r['b_calls']:>8}  {r['a_calls']:>8}"
        )
        print(f"{color}{line}{reset}")


if __name__ == "__main__":
    main()
