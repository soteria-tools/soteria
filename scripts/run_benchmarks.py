#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2025 Soteria Tools Ltd.
# SPDX-License-Identifier: Apache-2.0
"""Run benchmarks using hyperfine and output JSON results.

This script runs a list of benchmark commands using hyperfine for reliable
timing measurements, and outputs the results as JSON.

Usage:
    python3 run_benchmarks.py <commit_sha> <timestamp> [output_file]

If output_file is not specified, results are printed to stdout.
"""

import json
import subprocess
import sys
import tempfile
from typing import TypedDict


# =============================================================================
# BENCHMARK CONFIGURATION
# Add your benchmark scripts/commands here. Each benchmark should have:
#   - name: A short identifier for the benchmark
#   - command: The shell command to run
# =============================================================================
BENCHMARKS: list[dict[str, str]] = [
    # Example benchmarks (uncomment and modify as needed):
    # {"name": "build-ocaml", "command": "dune build"},
    # {"name": "run-tests", "command": "dune test"},
    # {"name": "analyze-sample", "command": "dune exec -- soteria-c analyze sample.c"},
]
# =============================================================================


class BenchmarkResult(TypedDict):
    """Result of a single benchmark run."""

    name: str
    command: str
    mean: float
    stddev: float
    min: float
    max: float
    median: float
    runs: int


class BenchmarkOutput(TypedDict):
    """Complete benchmark output structure."""

    commit: str
    timestamp: str
    benchmarks: list[BenchmarkResult]


def run_single_benchmark(name: str, command: str) -> BenchmarkResult:
    """Run a single benchmark using hyperfine and return the results."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
        json_output_path = f.name

    try:
        # Run hyperfine with warmup and multiple runs for reliability
        hyperfine_cmd = [
            "opam",
            "exec",
            "--",
            "dune",
            "exec",
            "--",
            "hyperfine",
            "--warmup",
            "1",
            "--runs",
            "3",
            "--export-json",
            json_output_path,
            command,
        ]

        proc = subprocess.run(
            hyperfine_cmd,
            capture_output=True,
            text=True,
            check=False,
        )

        if proc.returncode != 0:
            print(f"Warning: hyperfine failed for '{name}':", file=sys.stderr)
            print(proc.stderr, file=sys.stderr)
            # Return a result with zeros to indicate failure
            return BenchmarkResult(
                name=name,
                command=command,
                mean=0.0,
                stddev=0.0,
                min=0.0,
                max=0.0,
                median=0.0,
                runs=0,
            )

        # Parse hyperfine's JSON output
        with open(json_output_path) as f:
            data = json.load(f)

        result = data["results"][0]
        return BenchmarkResult(
            name=name,
            command=command,
            mean=result["mean"],
            stddev=result["stddev"],
            min=result["min"],
            max=result["max"],
            median=result["median"],
            runs=len(result.get("times", [])) or 3,
        )

    finally:
        # Clean up temp file
        import os

        try:
            os.unlink(json_output_path)
        except OSError:
            pass


def run_all_benchmarks() -> list[BenchmarkResult]:
    """Run all configured benchmarks and return results."""
    results = []

    if not BENCHMARKS:
        print(
            "Warning: No benchmarks configured. Add benchmarks to BENCHMARKS list.",
            file=sys.stderr,
        )
        return results

    for bench in BENCHMARKS:
        print(f"Running benchmark: {bench['name']}", file=sys.stderr)
        result = run_single_benchmark(bench["name"], bench["command"])
        results.append(result)
        print(
            f"  Mean: {result['mean']:.3f}s (+/- {result['stddev']:.3f}s)",
            file=sys.stderr,
        )

    return results


def main() -> int:
    """Main entry point."""
    if len(sys.argv) < 3:
        print(
            "Usage: python3 run_benchmarks.py <commit_sha> <timestamp> [output_file]",
            file=sys.stderr,
        )
        return 1

    commit_sha = sys.argv[1]
    timestamp = sys.argv[2]
    output_file = sys.argv[3] if len(sys.argv) > 3 else None

    # Run all benchmarks
    results = run_all_benchmarks()

    # Build output structure
    output: BenchmarkOutput = {
        "commit": commit_sha,
        "timestamp": timestamp,
        "benchmarks": results,
    }

    # Output as JSON
    json_str = json.dumps(output, indent=2)

    if output_file:
        with open(output_file, "w") as f:
            f.write(json_str)
            f.write("\n")
        print(f"Results written to {output_file}", file=sys.stderr)
    else:
        print(json_str)

    return 0


if __name__ == "__main__":
    sys.exit(main())
