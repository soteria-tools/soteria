#!/usr/bin/env python3
"""
Version synchronization script for Soteria.

This script manages version strings across the codebase to ensure consistency.
Versions are defined in scripts/versions.json and propagated to various files.

Usage:
    scripts/versionsync.py list                - List all configured versions
    scripts/versionsync.py check               - Check if all versions are in sync
    scripts/versionsync.py update              - Update all version occurrences
    scripts/versionsync.py set NAME VERSION    - Set a version and update all occurrences
"""

import json
import re
import sys
from pathlib import Path

# Files to scan for versionsync tags
FILES_TO_SCAN = [
    ".github/workflows/ci.yml",
    "soteria-rust.opam.template",
    "dune-project",
    ".ocamlformat",
    "Makefile",
]

# Version types and their patterns for matching the value on the next line
# Each pattern should have a single capturing group for the version value
VERSION_PATTERNS: dict[str, list[tuple[str, str]]] = {
    # Commit hashes (40 hex chars)
    "CHARON_COMMIT_HASH": [
        (r"^(\s*CHARON_COMMIT_HASH:\s*)([a-f0-9]{40})(.*)$", r"\g<1>{version}\g<3>"),
        (r'^(.*#)([a-f0-9]{40})(.*"\].*)$', r"\g<1>{version}\g<3>"),  # opam pin-depends
    ],
    "OBOL_COMMIT_HASH": [
        (r"^(\s*OBOL_COMMIT_HASH:\s*)([a-f0-9]{40})(.*)$", r"\g<1>{version}\g<3>"),
    ],
    # Repository paths
    "CHARON_REPO": [
        (r"^(\s*CHARON_REPO:\s*)([^\s]+)(.*)$", r"\g<1>{version}\g<3>"),
        (r'^(.*github\.com/)([^#]+)(#[a-f0-9]{40}.*"\].*)$', r"\g<1>{version}\g<3>"),  # opam pin-depends
    ],
    # Semver versions
    "OCAML_VERSION": [
        (r"^(\s*OCAML_VERSION[=:]\s*)([0-9]+\.[0-9]+\.[0-9]+)(.*)$", r"\g<1>{version}\g<3>"),
        (r"^(\s*\(>=\s*)([0-9]+\.[0-9]+\.[0-9]+)(\)\).*)$", r"\g<1>{version}\g<3>"),  # dune-project
        (r"^(ocaml-version\s*=\s*)([0-9]+\.[0-9]+)(.*)$", r"\g<1>{version_short}\g<3>"),  # .ocamlformat uses major.minor
    ],
    "OCAMLFORMAT_VERSION": [
        (r"^(version=)([0-9]+\.[0-9]+\.[0-9]+)(.*)$", r"\g<1>{version}\g<3>"),
        (r"^(\s*OCAMLFORMAT_VERSION=)([0-9]+\.[0-9]+\.[0-9]+)(.*)$", r"\g<1>{version}\g<3>"),
        (r"^(\s*\(=\s*)([0-9]+\.[0-9]+\.[0-9]+)(\)\)\).*)$", r"\g<1>{version}\g<3>"),  # dune-project
    ],
}


def load_versions(path: Path) -> dict[str, str]:
    """Load versions from JSON file."""
    with open(path, "r") as f:
        data = json.load(f)
    # Filter out internal keys like _comment
    return {k: v for k, v in data.items() if not k.startswith("_")}


def save_versions(path: Path, versions: dict[str, str]) -> None:
    """Save versions to JSON file."""
    data = {
        "_comment": "Central version configuration for Soteria. Run `scripts/versionsync.py check` to verify versions are in sync, or `scripts/versionsync.py update` to update versions everywhere.",
        **versions,
    }
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def get_version_value(version: str, uses_short: bool) -> str:
    """Get the version value, handling special cases like short version."""
    if uses_short:
        parts = version.split(".")
        return ".".join(parts[:2]) if len(parts) >= 2 else version
    return version


def find_and_process_tags(
    root: Path, versions: dict[str, str], update: bool = False
) -> tuple[bool, list[str]]:
    """
    Find all versionsync tags and check/update the following line.
    Returns (all_match, list of issues/updates).
    """
    all_match = True
    messages: list[str] = []

    for file_rel in FILES_TO_SCAN:
        file_path = root / file_rel
        if not file_path.exists():
            messages.append(f"WARNING: {file_rel} not found")
            continue

        lines = file_path.read_text().splitlines(keepends=True)
        modified = False
        i = 0

        while i < len(lines):
            line = lines[i]
            # Look for versionsync tag
            tag_match = re.search(r"\[versionsync:\s*(\w+)\]", line)
            if tag_match:
                version_name = tag_match.group(1)
                if version_name not in versions:
                    messages.append(
                        f"WARNING: {file_rel}:{i+1} - Unknown version {version_name}"
                    )
                    i += 1
                    continue

                expected_version = versions[version_name]
                patterns = VERSION_PATTERNS.get(version_name, [])

                if not patterns:
                    messages.append(
                        f"WARNING: {file_rel}:{i+1} - No patterns defined for {version_name}"
                    )
                    i += 1
                    continue

                # Check the next line(s) for the version
                found_match = False
                for j in range(i + 1, min(i + 6, len(lines))):  # Check next 5 lines
                    next_line = lines[j]
                    for pattern, replacement_template in patterns:
                        match = re.match(pattern, next_line.rstrip("\n\r"))
                        if match:
                            found_match = True
                            uses_short = "{version_short}" in replacement_template
                            version_val = get_version_value(expected_version, uses_short)

                            if uses_short:
                                expected_replacement = replacement_template.format(
                                    version_short=version_val
                                )
                            else:
                                expected_replacement = replacement_template.format(
                                    version=version_val
                                )

                            new_line = re.sub(pattern, expected_replacement, next_line.rstrip("\n\r"))
                            # Preserve original line ending
                            line_ending = next_line[len(next_line.rstrip("\n\r")):]
                            new_line_with_ending = new_line + line_ending

                            if next_line != new_line_with_ending:
                                if update:
                                    lines[j] = new_line_with_ending
                                    modified = True
                                    messages.append(
                                        f"UPDATED: {file_rel}:{j+1} - {version_name}"
                                    )
                                else:
                                    all_match = False
                                    current_match = match.group(2) if match.lastindex and match.lastindex >= 2 else "?"
                                    messages.append(
                                        f"MISMATCH: {file_rel}:{j+1} - {version_name}: "
                                        f"found '{current_match}', expected '{version_val}'"
                                    )
                            break
                    if found_match:
                        break

                if not found_match:
                    messages.append(
                        f"WARNING: {file_rel}:{i+1} - Could not find version pattern after tag for {version_name}"
                    )
                    all_match = False

            i += 1

        if modified:
            file_path.write_text("".join(lines))

    return all_match, messages


def cmd_list(versions: dict[str, str]) -> int:
    """List all configured versions."""
    print("Configured versions:")
    for name, value in sorted(versions.items()):
        print(f"  {name}: {value}")
    return 0


def cmd_check(root: Path, versions: dict[str, str]) -> int:
    """Check if all versions are in sync."""
    all_match, messages = find_and_process_tags(root, versions, update=False)
    for msg in messages:
        print(msg)
    if all_match:
        print("All versions are in sync.")
        return 0
    return 1


def cmd_update(root: Path, versions: dict[str, str]) -> int:
    """Update all version occurrences."""
    _, messages = find_and_process_tags(root, versions, update=True)
    for msg in messages:
        print(msg)
    updates = [m for m in messages if m.startswith("UPDATED:")]
    if updates:
        print(f"\nUpdated {len(updates)} occurrence(s).")
    else:
        print("All versions were already in sync.")
    return 0


def cmd_set(versions_path: Path, root: Path, versions: dict[str, str], name: str, value: str) -> int:
    """Set a version and update all occurrences."""
    if name not in VERSION_PATTERNS:
        print(f"Error: Unknown version name '{name}'")
        print(f"Available versions: {', '.join(sorted(VERSION_PATTERNS.keys()))}")
        return 1

    versions[name] = value
    save_versions(versions_path, versions)
    print(f"Set {name} = {value}")

    return cmd_update(root, versions)


def main() -> int:
    if len(sys.argv) < 2:
        print(__doc__)
        return 1

    command = sys.argv[1]

    # Find repository root
    script_dir = Path(__file__).parent
    root = script_dir.parent
    versions_path = script_dir / "versions.json"

    if not versions_path.exists():
        print(f"Error: {versions_path} not found")
        return 1

    versions = load_versions(versions_path)

    if command == "list":
        return cmd_list(versions)
    elif command == "check":
        return cmd_check(root, versions)
    elif command == "update":
        return cmd_update(root, versions)
    elif command == "set":
        if len(sys.argv) != 4:
            print("Usage: versionsync.py set NAME VERSION")
            return 1
        return cmd_set(versions_path, root, versions, sys.argv[2], sys.argv[3])
    else:
        print(f"Unknown command: {command}")
        print(__doc__)
        return 1


if __name__ == "__main__":
    sys.exit(main())
