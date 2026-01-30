#!/usr/bin/env python3
"""
Version synchronization script for Soteria.

This script manages version strings across the codebase to ensure consistency.
Versions are defined in scripts/versions.json and embedded in tags throughout
the codebase in the format: [versionsync: NAME=value]

The script performs three-way checking:
1. Config value (versions.json) - the source of truth
2. Tag value - embedded in comments like [versionsync: OCAML_VERSION=5.4.0]
3. File value - the actual value on the line(s) following the tag

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

# How many lines after the tag to search for the value
LINES_TO_SEARCH = 5

# Regex to match versionsync tags: [versionsync: NAME=value]
TAG_PATTERN = re.compile(r"\[versionsync:\s*(\w+)=([^\]]+)\]")


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


def find_tags_in_file(content: str) -> list[tuple[int, int, str, str]]:
    """
    Find all versionsync tags in file content.
    Returns list of (line_number, column, name, value) tuples.
    """
    tags = []
    for i, line in enumerate(content.splitlines()):
        for match in TAG_PATTERN.finditer(line):
            tags.append((i, match.start(), match.group(1), match.group(2)))
    return tags


def check_file(
    file_path: Path, file_rel: str, versions: dict[str, str]
) -> tuple[bool, list[str]]:
    """
    Check a single file for version consistency.
    Returns (all_ok, list of messages).
    """
    content = file_path.read_text()
    lines = content.splitlines()
    messages = []
    all_ok = True

    tags = find_tags_in_file(content)

    for line_num, _, name, tag_value in tags:
        # Check 1: Does the tag value match the config?
        if name not in versions:
            messages.append(
                f"WARNING: {file_rel}:{line_num + 1} - Unknown version '{name}' in tag"
            )
            all_ok = False
            continue

        config_value = versions[name]
        if tag_value != config_value:
            messages.append(
                f"MISMATCH: {file_rel}:{line_num + 1} - {name}\n"
                f"  Tag has:    '{tag_value}'\n"
                f"  Config has: '{config_value}'"
            )
            all_ok = False
            continue

        # Check 2: Does the tag value appear in the following lines?
        found_in_file = False
        for j in range(line_num + 1, min(line_num + 1 + LINES_TO_SEARCH, len(lines))):
            if tag_value in lines[j]:
                found_in_file = True
                break

        if not found_in_file:
            messages.append(
                f"MISMATCH: {file_rel}:{line_num + 1} - {name}\n"
                f"  Tag says: '{tag_value}'\n"
                f"  But this value was not found in the next {LINES_TO_SEARCH} lines"
            )
            all_ok = False

    return all_ok, messages


def update_file(
    file_path: Path, file_rel: str, versions: dict[str, str]
) -> tuple[bool, list[str]]:
    """
    Update a single file to match the config versions.
    Returns (was_modified, list of messages).
    """
    content = file_path.read_text()
    lines = content.splitlines(keepends=True)
    messages = []
    modified = False

    # We need to process from bottom to top to preserve line numbers
    # Or we can do multiple passes. Let's do a simpler approach:
    # rebuild the content with replacements.

    tags = find_tags_in_file(content)

    for line_num, col, name, old_value in tags:
        if name not in versions:
            messages.append(
                f"WARNING: {file_rel}:{line_num + 1} - Unknown version '{name}', skipping"
            )
            continue

        new_value = versions[name]
        if old_value == new_value:
            continue  # Already in sync

        # Update the tag itself
        old_tag = f"[versionsync: {name}={old_value}]"
        new_tag = f"[versionsync: {name}={new_value}]"

        # Update the following lines (replace old_value with new_value)
        # We need to be careful to only replace in the right places

        # Find the tag line and update it
        tag_line = lines[line_num]
        if old_tag in tag_line:
            lines[line_num] = tag_line.replace(old_tag, new_tag, 1)
            modified = True

        # Update the value in following lines
        for j in range(
            line_num + 1, min(line_num + 1 + LINES_TO_SEARCH, len(lines))
        ):
            if old_value in lines[j]:
                lines[j] = lines[j].replace(old_value, new_value, 1)
                modified = True
                messages.append(f"UPDATED: {file_rel}:{j + 1} - {name}")
                break  # Only update the first occurrence

    if modified:
        file_path.write_text("".join(lines))

    return modified, messages


def cmd_list(versions: dict[str, str]) -> int:
    """List all configured versions."""
    print("Configured versions (from versions.json):")
    for name, value in sorted(versions.items()):
        print(f"  {name}: {value}")
    return 0


def cmd_check(root: Path, versions: dict[str, str]) -> int:
    """Check if all versions are in sync."""
    all_ok = True
    all_messages = []

    for file_rel in FILES_TO_SCAN:
        file_path = root / file_rel
        if not file_path.exists():
            all_messages.append(f"WARNING: {file_rel} not found")
            continue

        ok, messages = check_file(file_path, file_rel, versions)
        all_ok = all_ok and ok
        all_messages.extend(messages)

    for msg in all_messages:
        print(msg)

    if all_ok:
        print("All versions are in sync.")
        return 0
    return 1


def cmd_update(root: Path, versions: dict[str, str]) -> int:
    """Update all version occurrences."""
    all_messages = []
    total_updates = 0

    for file_rel in FILES_TO_SCAN:
        file_path = root / file_rel
        if not file_path.exists():
            all_messages.append(f"WARNING: {file_rel} not found")
            continue

        modified, messages = update_file(file_path, file_rel, versions)
        all_messages.extend(messages)
        if modified:
            total_updates += len([m for m in messages if m.startswith("UPDATED:")])

    for msg in all_messages:
        print(msg)

    if total_updates > 0:
        print(f"\nUpdated {total_updates} occurrence(s).")
    else:
        print("All versions were already in sync.")
    return 0


def cmd_set(
    versions_path: Path, root: Path, versions: dict[str, str], name: str, value: str
) -> int:
    """Set a version and update all occurrences."""
    if name.startswith("_"):
        print(f"Error: Invalid version name '{name}'")
        return 1

    old_value = versions.get(name)
    versions[name] = value
    save_versions(versions_path, versions)

    if old_value:
        print(f"Changed {name}: '{old_value}' -> '{value}'")
    else:
        print(f"Added {name}: '{value}'")

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
