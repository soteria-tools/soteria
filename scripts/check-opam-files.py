#!/usr/bin/env python3
"""Check that all *.opam files have not changed since the last commit."""

import subprocess
import sys
from pathlib import Path


def main():
    """Check if any .opam files have changed since the last commit."""
    try:
        # Get the list of changed files since the last commit
        result = subprocess.run(
            ["git", "diff", "--name-only", "HEAD"],
            capture_output=True,
            text=True,
            check=True,
        )

        changed_opam_files = [
            r.strip()
            for r in result.stdout.strip().split("\n")
            if r.strip().endswith(".opam")
        ]

        if changed_opam_files:
            print(
                "ERROR: The following .opam files have changed since the last commit:"
            )
            for file in changed_opam_files:
                print(f"  - {file}")
            sys.exit(1)
        else:
            print("✓ No .opam files have changed since the last commit.")
            sys.exit(0)

    except subprocess.CalledProcessError as e:
        print(f"Error running git command: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
