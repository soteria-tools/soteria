#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2025 Soteria Tools Ltd.
# SPDX-License-Identifier: Apache-2.0
"""Generate an HTML benchmark results page with Chart.js charts.

This script reads benchmark results and generates an interactive HTML page
with charts showing performance trends over time.

Usage:
    python3 generate_benchmark_page.py \\
        --new-results results.json \\
        --history main-history.json \\
        --output main.html \\
        --history-output main-history.json \\
        --mode main|pr \\
        [--pr-number 123] \\
        [--max-history 50] \\
        [--repo-url https://github.com/owner/repo]
"""

import argparse
import html
import json
import os
import sys
from typing import Any

# Maximum number of historical entries to keep
DEFAULT_MAX_HISTORY = 50


def load_json_file(path: str) -> Any:
    """Load JSON from a file, returning None if file doesn't exist."""
    if not path or not os.path.exists(path):
        return None
    try:
        with open(path) as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        print(f"Warning: Could not load {path}: {e}", file=sys.stderr)
        return None


def save_json_file(path: str, data: Any) -> None:
    """Save data as JSON to a file."""
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def merge_history(
    history: list[dict[str, Any]] | None,
    new_result: dict[str, Any],
    max_history: int,
) -> list[dict[str, Any]]:
    """Merge new results into history, keeping only the most recent entries."""
    if history is None:
        history = []

    # Avoid duplicates based on commit SHA
    existing_commits = {entry.get("commit") for entry in history}
    if new_result.get("commit") not in existing_commits:
        history.append(new_result)

    # Sort by timestamp (most recent last)
    history.sort(key=lambda x: x.get("timestamp", ""))

    # Trim to max history
    if len(history) > max_history:
        history = history[-max_history:]

    return history


def generate_html(
    history: list[dict[str, Any]],
    mode: str,
    pr_number: int | None = None,
    repo_url: str = "",
    pr_commit: str | None = None,
) -> str:
    """Generate the HTML page with Chart.js charts."""
    # Collect all unique benchmark names
    benchmark_names: set[str] = set()
    for entry in history:
        for bench in entry.get("benchmarks", []):
            benchmark_names.add(bench.get("name", "unknown"))

    benchmark_names_sorted = sorted(benchmark_names)

    # Prepare data for charts
    # labels = commit SHAs (short form)
    labels = [entry.get("commit", "")[:7] for entry in history]
    full_commits = [entry.get("commit", "") for entry in history]
    timestamps = [entry.get("timestamp", "") for entry in history]

    # For each benchmark, collect mean times
    datasets: list[dict[str, Any]] = []
    colors = [
        "#3b82f6",  # blue
        "#10b981",  # green
        "#f59e0b",  # amber
        "#ef4444",  # red
        "#8b5cf6",  # purple
        "#ec4899",  # pink
        "#06b6d4",  # cyan
        "#84cc16",  # lime
    ]

    for i, bench_name in enumerate(benchmark_names_sorted):
        data = []
        for entry in history:
            # Find this benchmark in the entry
            bench_result = next(
                (b for b in entry.get("benchmarks", []) if b.get("name") == bench_name),
                None,
            )
            if bench_result:
                data.append(bench_result.get("mean", None))
            else:
                data.append(None)

        color = colors[i % len(colors)]
        datasets.append(
            {
                "label": bench_name,
                "data": data,
                "borderColor": color,
                "backgroundColor": color + "20",
                "fill": False,
                "tension": 0.1,
            }
        )

    # Determine title
    if mode == "pr":
        title = f"Benchmark Results - PR #{pr_number}"
        subtitle = "Comparison against main branch history"
    else:
        title = "Benchmark Results - main branch"
        subtitle = f"Last {len(history)} commits"

    # Build the table rows
    table_rows = []
    for entry in reversed(history):  # Most recent first
        commit = entry.get("commit", "")
        timestamp = entry.get("timestamp", "")
        is_pr_commit = pr_commit and commit == pr_commit

        for bench in entry.get("benchmarks", []):
            row_class = "pr-row" if is_pr_commit else ""
            commit_short = commit[:7]
            if repo_url:
                commit_link = f'<a href="{repo_url}/commit/{commit}">{commit_short}</a>'
            else:
                commit_link = commit_short

            pr_badge = '<span class="pr-badge">PR</span>' if is_pr_commit else ""

            table_rows.append(
                f"""<tr class="{row_class}">
                <td>{commit_link} {pr_badge}</td>
                <td>{html.escape(timestamp)}</td>
                <td>{html.escape(bench.get("name", ""))}</td>
                <td>{bench.get("mean", 0):.4f}s</td>
                <td>{bench.get("stddev", 0):.4f}s</td>
                <td>{bench.get("min", 0):.4f}s</td>
                <td>{bench.get("max", 0):.4f}s</td>
            </tr>"""
            )

    table_rows_html = "\n".join(table_rows)

    # Generate the complete HTML
    html_content = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{html.escape(title)}</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        :root {{
            --bg-primary: #0d1117;
            --bg-secondary: #161b22;
            --bg-tertiary: #21262d;
            --text-primary: #e6edf3;
            --text-secondary: #8b949e;
            --border-color: #30363d;
            --accent-color: #3b82f6;
            --pr-highlight: #1f6feb33;
        }}

        * {{
            box-sizing: border-box;
        }}

        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            background-color: var(--bg-primary);
            color: var(--text-primary);
            margin: 0;
            padding: 20px;
            line-height: 1.5;
        }}

        .container {{
            max-width: 1200px;
            margin: 0 auto;
        }}

        h1 {{
            margin: 0 0 8px 0;
            font-size: 24px;
            font-weight: 600;
        }}

        .subtitle {{
            color: var(--text-secondary);
            margin-bottom: 24px;
        }}

        .chart-container {{
            background-color: var(--bg-secondary);
            border: 1px solid var(--border-color);
            border-radius: 8px;
            padding: 20px;
            margin-bottom: 24px;
        }}

        .chart-wrapper {{
            position: relative;
            height: 400px;
        }}

        table {{
            width: 100%;
            border-collapse: collapse;
            background-color: var(--bg-secondary);
            border: 1px solid var(--border-color);
            border-radius: 8px;
            overflow: hidden;
        }}

        th, td {{
            padding: 12px 16px;
            text-align: left;
            border-bottom: 1px solid var(--border-color);
        }}

        th {{
            background-color: var(--bg-tertiary);
            font-weight: 600;
            color: var(--text-secondary);
            font-size: 12px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }}

        tr:last-child td {{
            border-bottom: none;
        }}

        tr:hover {{
            background-color: var(--bg-tertiary);
        }}

        .pr-row {{
            background-color: var(--pr-highlight);
        }}

        .pr-row:hover {{
            background-color: #1f6feb44;
        }}

        .pr-badge {{
            display: inline-block;
            background-color: var(--accent-color);
            color: white;
            font-size: 10px;
            font-weight: 600;
            padding: 2px 6px;
            border-radius: 4px;
            margin-left: 8px;
        }}

        a {{
            color: var(--accent-color);
            text-decoration: none;
        }}

        a:hover {{
            text-decoration: underline;
        }}

        .empty-state {{
            text-align: center;
            padding: 60px 20px;
            color: var(--text-secondary);
        }}

        .timestamp {{
            color: var(--text-secondary);
            font-size: 12px;
            margin-top: 16px;
        }}

        @media (max-width: 768px) {{
            body {{
                padding: 12px;
            }}

            th, td {{
                padding: 8px 12px;
                font-size: 14px;
            }}

            .chart-wrapper {{
                height: 300px;
            }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>{html.escape(title)}</h1>
        <p class="subtitle">{html.escape(subtitle)}</p>

        {"" if not benchmark_names_sorted else f'''
        <div class="chart-container">
            <div class="chart-wrapper">
                <canvas id="benchmarkChart"></canvas>
            </div>
        </div>
        '''}

        {"<div class='empty-state'><p>No benchmark data available yet.</p></div>" if not table_rows else f'''
        <table>
            <thead>
                <tr>
                    <th>Commit</th>
                    <th>Timestamp</th>
                    <th>Benchmark</th>
                    <th>Mean</th>
                    <th>Std Dev</th>
                    <th>Min</th>
                    <th>Max</th>
                </tr>
            </thead>
            <tbody>
                {table_rows_html}
            </tbody>
        </table>
        '''}

        <p class="timestamp">Generated at: {timestamps[-1] if timestamps else "N/A"}</p>
    </div>

    {"" if not benchmark_names_sorted else f'''
    <script>
        const ctx = document.getElementById("benchmarkChart").getContext("2d");
        const labels = {json.dumps(labels)};
        const fullCommits = {json.dumps(full_commits)};
        const timestamps = {json.dumps(timestamps)};
        const datasets = {json.dumps(datasets)};

        new Chart(ctx, {{
            type: "line",
            data: {{
                labels: labels,
                datasets: datasets
            }},
            options: {{
                responsive: true,
                maintainAspectRatio: false,
                interaction: {{
                    intersect: false,
                    mode: "index"
                }},
                plugins: {{
                    legend: {{
                        position: "top",
                        labels: {{
                            color: "#e6edf3",
                            usePointStyle: true,
                            padding: 20
                        }}
                    }},
                    tooltip: {{
                        backgroundColor: "#21262d",
                        titleColor: "#e6edf3",
                        bodyColor: "#e6edf3",
                        borderColor: "#30363d",
                        borderWidth: 1,
                        padding: 12,
                        callbacks: {{
                            title: function(context) {{
                                const idx = context[0].dataIndex;
                                return "Commit: " + fullCommits[idx];
                            }},
                            afterTitle: function(context) {{
                                const idx = context[0].dataIndex;
                                return timestamps[idx];
                            }},
                            label: function(context) {{
                                return context.dataset.label + ": " + context.parsed.y.toFixed(4) + "s";
                            }}
                        }}
                    }}
                }},
                scales: {{
                    x: {{
                        grid: {{
                            color: "#30363d"
                        }},
                        ticks: {{
                            color: "#8b949e"
                        }}
                    }},
                    y: {{
                        grid: {{
                            color: "#30363d"
                        }},
                        ticks: {{
                            color: "#8b949e",
                            callback: function(value) {{
                                return value.toFixed(2) + "s";
                            }}
                        }},
                        title: {{
                            display: true,
                            text: "Execution Time (seconds)",
                            color: "#8b949e"
                        }}
                    }}
                }}
            }}
        }});
    </script>
    '''}
</body>
</html>
"""
    return html_content


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Generate benchmark results HTML page"
    )
    parser.add_argument(
        "--new-results",
        required=True,
        help="Path to the new benchmark results JSON file",
    )
    parser.add_argument(
        "--history",
        default="",
        help="Path to the existing history JSON file (optional)",
    )
    parser.add_argument(
        "--output",
        required=True,
        help="Path for the output HTML file",
    )
    parser.add_argument(
        "--history-output",
        default="",
        help="Path to save the updated history JSON (optional, main mode only)",
    )
    parser.add_argument(
        "--mode",
        choices=["main", "pr"],
        required=True,
        help="Mode: 'main' for main branch, 'pr' for pull request",
    )
    parser.add_argument(
        "--pr-number",
        type=int,
        default=None,
        help="PR number (required for PR mode)",
    )
    parser.add_argument(
        "--max-history",
        type=int,
        default=DEFAULT_MAX_HISTORY,
        help=f"Maximum number of historical entries to keep (default: {DEFAULT_MAX_HISTORY})",
    )
    parser.add_argument(
        "--repo-url",
        default="",
        help="Repository URL for commit links (e.g., https://github.com/owner/repo)",
    )

    args = parser.parse_args()

    # Validate PR mode requirements
    if args.mode == "pr" and args.pr_number is None:
        print("Error: --pr-number is required for PR mode", file=sys.stderr)
        return 1

    # Load new results
    new_results = load_json_file(args.new_results)
    if new_results is None:
        print(f"Error: Could not load new results from {args.new_results}", file=sys.stderr)
        return 1

    # Load existing history
    history = load_json_file(args.history)
    if history is None:
        history = []

    pr_commit = None

    if args.mode == "main":
        # Merge new results into history
        history = merge_history(history, new_results, args.max_history)

        # Save updated history
        if args.history_output:
            save_json_file(args.history_output, history)
            print(f"History saved to {args.history_output}", file=sys.stderr)
    else:
        # PR mode: append PR results to history for display, but don't save
        pr_commit = new_results.get("commit")
        history = list(history)  # Copy to avoid modifying original
        # Add PR result at the end
        existing_commits = {entry.get("commit") for entry in history}
        if pr_commit not in existing_commits:
            history.append(new_results)

    # Generate HTML
    html_content = generate_html(
        history=history,
        mode=args.mode,
        pr_number=args.pr_number,
        repo_url=args.repo_url,
        pr_commit=pr_commit,
    )

    # Write output
    os.makedirs(os.path.dirname(args.output) or ".", exist_ok=True)
    with open(args.output, "w") as f:
        f.write(html_content)

    print(f"HTML page generated: {args.output}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
