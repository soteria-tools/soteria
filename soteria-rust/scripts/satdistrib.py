#!/usr/bin/env python3.9
"""
plot_check_sat_times.py

Parse an SMT log file for lines that start with "(check-sat)" and plot the
distribution of times taken (in milliseconds). Lines look like:

  (check-sat) ; -> unsat (115ms)
  (check-sat) ; -> sat

If the time is omitted (under 1ms), we count that as 0 ms.

Usage:
  python plot_check_sat_times.py /path/to/logfile.log
  python plot_check_sat_times.py /path/to/logfile.log --bins 50 --save out.png
  python plot_check_sat_times.py /path/to/logfile.log --logx
"""

from __future__ import annotations
import argparse
import re
import sys
import math
from statistics import mean, median, pstdev
import matplotlib.pyplot as plt

CHECK_SAT_RE = re.compile(
    r"^\s*\(check-sat\).*->\s*(sat|unsat)(?:\s*\(\s*([\d.]+)ms\s*\))?", re.IGNORECASE
)


def parse_times_from_file(path: str):
    """
    Return list of times in milliseconds (float). Lines with no time are counted as 0.0.
    """
    times = []
    with open(path, "r", encoding="utf-8", errors="replace") as f:
        for lineno, raw in enumerate(f, start=1):
            m = CHECK_SAT_RE.match(raw)
            if not m:
                continue
            time_str = m.group(2)
            if time_str is None:
                # time omitted => <1ms => count as 0ms
                times.append(0.0)
            else:
                try:
                    t = float(time_str)
                    times.append(t)
                except ValueError:
                    # fallback: treat unparsable as 0 and continue
                    print(
                        f"Warning: couldn't parse time on line {lineno}: {raw.strip()}",
                        file=sys.stderr,
                    )
                    times.append(0.0)
    return times


def print_stats(times):
    n = len(times)
    if n == 0:
        print("No (check-sat) lines found.")
        return
    zeros = sum(1 for t in times if t == 0.0)
    nonzero = n - zeros
    mn = min(times)
    mx = max(times)
    avg = mean(times)
    med = median(times)
    try:
        std = pstdev(times)
    except Exception:
        std = float("nan")

    print(f"Parsed {n} (check-sat) entries")
    print(f"  zeros (<1ms)  : {zeros} ({zeros/n*100:.1f}%)")
    print(f"  non-zero ms   : {nonzero}")
    print(f"  min           : {mn:.3f} ms")
    print(f"  median        : {med:.3f} ms")
    print(f"  mean          : {avg:.3f} ms")
    print(f"  max           : {mx:.3f} ms")
    print(f"  std (pop)     : {std:.3f} ms")


def plot_distribution(times, bins="auto", logx=False, save_path=None, show=True):
    if len(times) == 0:
        raise ValueError("No data to plot.")

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.hist(times, bins=bins)
    ax.set_xlabel("Time (ms)")
    ax.set_ylabel("Count")
    ax.set_title("(check-sat) time distribution")

    # annotate basic stats on plot
    avg = mean(times)
    med = median(times)
    ax.axvline(avg, linestyle="dashed", linewidth=1)
    ax.axvline(med, linestyle="dotted", linewidth=1)
    stats_txt = f"n={len(times)}\nmean={avg:.2f}ms\nmedian={med:.2f}ms\nzeros={(sum(1 for t in times if t==0))*1}"
    ax.text(
        0.98,
        0.95,
        stats_txt,
        transform=ax.transAxes,
        horizontalalignment="right",
        verticalalignment="top",
        fontsize=9,
        bbox=dict(facecolor="white", alpha=0.7, edgecolor="none"),
    )

    if logx:
        # avoid applying log scale if there are non-positive values other than zeros.
        # Since zeros are expected, we shift all values by +1 for log scale display.
        shifted = [t + 1.0 for t in times]  # shift so 0ms becomes 1ms on log scale
        ax.clear()
        ax.hist(shifted, bins=bins)
        ax.set_xscale("log")
        ax.set_xlabel("Time (ms) -- log scale (shifted by +1 for zeros)")
        ax.set_ylabel("Count")
        ax.set_title("(check-sat) time distribution (log x-axis)")
        ax.axvline(mean(shifted), linestyle="dashed", linewidth=1)
        ax.axvline(median(shifted), linestyle="dotted", linewidth=1)

    plt.tight_layout()

    if save_path:
        fig.savefig(save_path, dpi=150)
        print(f"Saved plot to: {save_path}")

    if show:
        plt.show()
    else:
        plt.close(fig)


def main():
    ap = argparse.ArgumentParser(
        description="Plot distribution of (check-sat) times from an SMT log."
    )
    ap.add_argument("logfile", help="Path to SMT log file")
    ap.add_argument(
        "--bins",
        type=str,
        default="auto",
        help="Bins for histogram (int, or 'auto', or comma-separated bin edges)",
    )
    ap.add_argument(
        "--logx",
        action="store_true",
        help="Plot histogram with log x-axis (shifts times by +1 to keep zeros)",
    )
    ap.add_argument(
        "--save",
        metavar="FILE",
        help="Save plot to FILE (e.g. out.png). If omitted, plot is only shown.",
    )
    ap.add_argument(
        "--no-show",
        action="store_true",
        help="Do not show interactive plot (useful when only saving)",
    )
    args = ap.parse_args()

    # parse bins argument
    bins_arg = args.bins
    if bins_arg.lower() == "auto":
        bins = "auto"
    else:
        # try to interpret as integer bins or list
        if "," in bins_arg:
            try:
                edges = [float(x) for x in bins_arg.split(",")]
                bins = edges
            except Exception:
                print(
                    "Could not parse comma-separated bins; falling back to 'auto'.",
                    file=sys.stderr,
                )
                bins = "auto"
        else:
            try:
                bins = int(bins_arg)
            except Exception:
                bins = "auto"

    times = parse_times_from_file(args.logfile)
    if not times:
        print("No (check-sat) entries found in file.", file=sys.stderr)
        sys.exit(1)

    print_stats(times)

    try:
        plot_distribution(
            times, bins=bins, logx=args.logx, save_path=args.save, show=not args.no_show
        )
    except Exception as e:
        print("Error while plotting:", e, file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()
