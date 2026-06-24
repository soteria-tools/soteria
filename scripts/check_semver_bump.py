#!/usr/bin/env python3
"""Validate a Soteria release version bump.

Usage: check_semver_bump.py <previous> <new>

<previous> is the version of the latest existing release (without a leading
"v"), or the empty string if there is no prior release. <new> is the version
about to be released.

While Soteria is in its 0.x series the only legal bumps from 0.N.M are:
  - 0.(N+1).0   (breaking changes)
  - 0.N.(M+1)   (non-breaking changes)

The first ever release must be exactly 0.1.0. Exits non-zero with an
explanation if the bump is not allowed.
"""

import re
import sys


def parse(label: str, value: str) -> tuple[int, int, int]:
    m = re.fullmatch(r"(\d+)\.(\d+)\.(\d+)", value)
    if not m:
        sys.exit(f"error: {label} version '{value}' is not of the form X.Y.Z")
    return tuple(int(x) for x in m.groups())  # type: ignore[return-value]


def main() -> None:
    if len(sys.argv) != 3:
        sys.exit("usage: check_semver_bump.py <previous> <new>")
    previous, new = sys.argv[1], sys.argv[2]

    n_major, n_minor, n_patch = parse("new", new)
    if n_major != 0:
        sys.exit("error: major version must remain 0 during the 0.x series")

    if not previous:
        if (n_major, n_minor, n_patch) != (0, 1, 0):
            sys.exit(f"error: first release must be 0.1.0, got {new}")
        print(f"ok: first release {new}")
        return

    p_major, p_minor, p_patch = parse("previous", previous)
    breaking = (n_minor, n_patch) == (p_minor + 1, 0)
    non_breaking = (n_minor, n_patch) == (p_minor, p_patch + 1)
    if not (breaking or non_breaking):
        sys.exit(
            f"error: illegal bump {previous} -> {new}; "
            f"allowed: 0.{p_minor + 1}.0 (breaking) or "
            f"0.{p_minor}.{p_patch + 1} (non-breaking)"
        )
    print(f"ok: {previous} -> {new} ({'breaking' if breaking else 'non-breaking'})")


if __name__ == "__main__":
    main()
