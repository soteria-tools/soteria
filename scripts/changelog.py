#!/usr/bin/env python3
"""Small helper for CHANGELOG.md.

Subcommands:
  check                  Validate the changelog format (used in CI).
  check-frozen <file>    Fail if any already-released section changed relative
                         to the baseline <file> (the CHANGELOG.md from the base
                         branch). Adding a new released section is allowed.
  section <version>      Print the body of the section for <version> (used when
                         generating GitHub release notes).

The changelog follows a trimmed-down "Keep a Changelog" layout:

  # Changelog

  ## [Unreleased]
  - ...

  ## [0.1.0] - 2026-06-23
  - ...

Every released section header must be `## [X.Y.Z] - YYYY-MM-DD`. The
`## [Unreleased]` section must exist and come first. New entries are added by
hand under `[Unreleased]`; on release that header is renamed to the version.
"""

import re
import sys
from pathlib import Path

CHANGELOG = Path(__file__).resolve().parent.parent / "CHANGELOG.md"

VERSION_HEADER = re.compile(r"^## \[(\d+\.\d+\.\d+)\] - \d{4}-\d{2}-\d{2}\s*$")
UNRELEASED_HEADER = re.compile(r"^## \[Unreleased\]\s*$")
ANY_SECTION = re.compile(r"^## ")


def read_lines() -> list[str]:
    if not CHANGELOG.exists():
        sys.exit(f"error: {CHANGELOG} not found")
    return CHANGELOG.read_text().splitlines()


def released_sections(text: str) -> dict[str, str]:
    """Map each released version to its section text (header included).

    The `[Unreleased]` section is intentionally excluded: it is the only one
    pull requests are allowed to change.
    """
    lines = text.splitlines()
    sections: dict[str, str] = {}
    current: str | None = None
    body: list[str] = []

    def flush() -> None:
        if current is not None:
            sections[current] = "\n".join(body).strip("\n")

    for line in lines:
        if ANY_SECTION.match(line):
            flush()
            m = VERSION_HEADER.match(line)
            current = m.group(1) if m else None
            body = [line]
        elif current is not None:
            body.append(line)
    flush()
    return sections


def cmd_check() -> int:
    lines = read_lines()
    errors: list[str] = []

    non_empty = [l for l in lines if l.strip()]
    if not non_empty or non_empty[0].strip() != "# Changelog":
        errors.append("file must start with a '# Changelog' title")

    section_headers = [
        (i, l) for i, l in enumerate(lines) if ANY_SECTION.match(l)
    ]
    if not section_headers:
        errors.append("no '## ' sections found")

    seen_versions: set[str] = set()
    unreleased_index = None
    first_section_index = section_headers[0][0] if section_headers else None
    for i, line in section_headers:
        if UNRELEASED_HEADER.match(line):
            unreleased_index = i
            continue
        m = VERSION_HEADER.match(line)
        if not m:
            errors.append(
                f"line {i + 1}: malformed section header: {line!r}; "
                "expected '## [Unreleased]' or '## [X.Y.Z] - YYYY-MM-DD'"
            )
            continue
        if m.group(1) in seen_versions:
            errors.append(f"line {i + 1}: duplicate version {m.group(1)}")
        seen_versions.add(m.group(1))

    if unreleased_index is None:
        errors.append("missing '## [Unreleased]' section")
    elif unreleased_index != first_section_index:
        errors.append("'## [Unreleased]' must be the first section")

    if errors:
        for e in errors:
            print(f"CHANGELOG.md: {e}", file=sys.stderr)
        return 1
    print("CHANGELOG.md is well-formed.")
    return 0


def cmd_check_frozen(baseline_path: str) -> int:
    base = Path(baseline_path)
    if not base.exists() or not base.read_text().strip():
        print("no baseline changelog; skipping frozen-section check")
        return 0

    old = released_sections(base.read_text())
    new = released_sections(CHANGELOG.read_text())

    errors: list[str] = []
    for version, text in old.items():
        if version not in new:
            errors.append(f"released section [{version}] was removed")
        elif new[version] != text:
            errors.append(
                f"released section [{version}] was modified; released "
                "changelog entries are immutable (only [Unreleased] may change)"
            )

    if errors:
        for e in errors:
            print(f"CHANGELOG.md: {e}", file=sys.stderr)
        return 1
    print("CHANGELOG.md: released sections are unchanged.")
    return 0


def cmd_section(version: str) -> int:
    lines = read_lines()
    start = None
    for i, line in enumerate(lines):
        m = VERSION_HEADER.match(line)
        if m and m.group(1) == version:
            start = i
            break
    if start is None:
        sys.exit(
            f"error: no '## [{version}] - YYYY-MM-DD' section found in CHANGELOG.md"
        )
    body: list[str] = []
    for line in lines[start + 1 :]:
        if ANY_SECTION.match(line):
            break
        body.append(line)
    print("\n".join(body).strip())
    return 0


def main() -> int:
    if len(sys.argv) < 2:
        sys.exit("usage: changelog.py {check | section <version>}")
    cmd = sys.argv[1]
    if cmd == "check":
        return cmd_check()
    if cmd == "check-frozen":
        if len(sys.argv) != 3:
            sys.exit("usage: changelog.py check-frozen <baseline-file>")
        return cmd_check_frozen(sys.argv[2])
    if cmd == "section":
        if len(sys.argv) != 3:
            sys.exit("usage: changelog.py section <version>")
        return cmd_section(sys.argv[2])
    sys.exit(f"unknown subcommand: {cmd}")


if __name__ == "__main__":
    sys.exit(main())
