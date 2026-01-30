#!/usr/bin/env python3
"""
Version synchronization script for Soteria.

This script manages version strings across the codebase to ensure consistency.
Versions are defined in scripts/versions.json and propagated to various files.

Usage:
    scripts/versionsync.py check   - Check if all versions are in sync
    scripts/versionsync.py update  - Update all version occurrences
"""

import json
import re
import sys
from pathlib import Path


def load_versions(path: Path) -> dict[str, str]:
    """Load versions from JSON file."""
    with open(path, "r") as f:
        data = json.load(f)
    # Filter out internal keys like _comment
    return {k: v for k, v in data.items() if not k.startswith("_")}


# Each entry is: (file_path, version_key, pattern, replacement_template, uses_short_version)
# pattern: regex with capturing groups
# replacement_template: string with {version} placeholder
VERSION_LOCATIONS: list[tuple[str, str, str, str, bool]] = [
    # ci.yml - OCAML_VERSION
    (
        ".github/workflows/ci.yml",
        "OCAML_VERSION",
        r"(# \[versionsync: OCAML_VERSION\]\n  OCAML_VERSION: )[^\n]+",
        r"\g<1>{version}",
        False,
    ),
    # ci.yml - CHARON_REPO
    (
        ".github/workflows/ci.yml",
        "CHARON_REPO",
        r"(# \[versionsync: CHARON_REPO\]\n  CHARON_REPO: )[^\n]+",
        r"\g<1>{version}",
        False,
    ),
    # ci.yml - CHARON_COMMIT_HASH
    (
        ".github/workflows/ci.yml",
        "CHARON_COMMIT_HASH",
        r"(# \[versionsync: CHARON_COMMIT_HASH\]\n  CHARON_COMMIT_HASH: )[^\n]+",
        r"\g<1>{version}",
        False,
    ),
    # soteria-rust.opam.template - name_matcher_parser line (CHARON_REPO and CHARON_COMMIT_HASH)
    (
        "soteria-rust.opam.template",
        "CHARON_REPO",
        r'(\["name_matcher_parser\.~dev" "git\+https://github\.com/)([^#]+)(#[a-f0-9]+"\])',
        r"\g<1>{version}\g<3>",
        False,
    ),
    (
        "soteria-rust.opam.template",
        "CHARON_COMMIT_HASH",
        r'(\["name_matcher_parser\.~dev" "git\+https://github\.com/[^#]+#)([a-f0-9]+)("\])',
        r"\g<1>{version}\g<3>",
        False,
    ),
    # soteria-rust.opam.template - charon line (CHARON_REPO and CHARON_COMMIT_HASH)
    (
        "soteria-rust.opam.template",
        "CHARON_REPO",
        r'(\["charon\.~dev" "git\+https://github\.com/)([^#]+)(#[a-f0-9]+"\])',
        r"\g<1>{version}\g<3>",
        False,
    ),
    (
        "soteria-rust.opam.template",
        "CHARON_COMMIT_HASH",
        r'(\["charon\.~dev" "git\+https://github\.com/[^#]+#)([a-f0-9]+)("\])',
        r"\g<1>{version}\g<3>",
        False,
    ),
    # dune-project - OCAML_VERSION (multiple occurrences, use replaceall logic)
    (
        "dune-project",
        "OCAML_VERSION",
        r"(; \[versionsync: OCAML_VERSION\]\n  \(ocaml\n   \(>= )([0-9.]+)(\)\))",
        r"\g<1>{version}\g<3>",
        False,
    ),
    # dune-project - OCAMLFORMAT_VERSION (multiple occurrences)
    (
        "dune-project",
        "OCAMLFORMAT_VERSION",
        r"(; \[versionsync: OCAMLFORMAT_VERSION\]\n  \(ocamlformat\n   \(and\n    :with-dev-setup\n    \(= )([0-9.]+)(\)\)\))",
        r"\g<1>{version}\g<3>",
        False,
    ),
    # .ocamlformat - OCAMLFORMAT_VERSION
    (
        ".ocamlformat",
        "OCAMLFORMAT_VERSION",
        r"(# \[versionsync: OCAMLFORMAT_VERSION\]\nversion=)([0-9.]+)",
        r"\g<1>{version}",
        False,
    ),
    # .ocamlformat - OCAML_VERSION (uses short version: major.minor)
    (
        ".ocamlformat",
        "OCAML_VERSION",
        r"(# \[versionsync: OCAML_VERSION\]\nocaml-version = )([0-9.]+)",
        r"\g<1>{version}",
        True,
    ),
    # Makefile - OCAML_VERSION
    (
        "Makefile",
        "OCAML_VERSION",
        r"(# \[versionsync: OCAML_VERSION\]\nOCAML_VERSION=)([0-9.]+)",
        r"\g<1>{version}",
        False,
    ),
    # Makefile - OCAMLFORMAT_VERSION (in switch target)
    (
        "Makefile",
        "OCAMLFORMAT_VERSION",
        r"(# \[versionsync: OCAMLFORMAT_VERSION\] install ocamlformat\n\t\$\(OPAM\) install ocaml-lsp-server odig ocamlformat\.)([0-9.]+)( -y)",
        r"\g<1>{version}\g<3>",
        False,
    ),
    # Makefile - OCAMLFORMAT_VERSION (in ocaml-deps target)
    (
        "Makefile",
        "OCAMLFORMAT_VERSION",
        r"(\$\(OPAM\) install ocamlformat\.)([0-9.]+)( # \[versionsync: OCAMLFORMAT_VERSION\])",
        r"\g<1>{version}\g<3>",
        False,
    ),
]


def get_version_value(versions: dict[str, str], key: str, uses_short: bool) -> str:
    """Get the version value, handling special cases like short version."""
    version = versions[key]
    if uses_short:
        # For ocaml-version in .ocamlformat, use only major.minor
        parts = version.split(".")
        return ".".join(parts[:2]) if len(parts) >= 2 else version
    return version


def check_versions(root: Path, versions: dict[str, str]) -> bool:
    """Check if all versions are in sync. Returns True if all match."""
    all_match = True

    for file_path, version_key, pattern, replacement_template, uses_short in VERSION_LOCATIONS:
        full_path = root / file_path
        if not full_path.exists():
            print(f"WARNING: {file_path} not found")
            continue

        content = full_path.read_text()
        version = get_version_value(versions, version_key, uses_short)
        expected_replacement = replacement_template.format(version=version)

        # Try to find the pattern
        matches = list(re.finditer(pattern, content))
        if not matches:
            print(f"MISSING: {file_path} - pattern for {version_key} not found")
            print(f"  Pattern: {pattern}")
            all_match = False
            continue

        for match in matches:
            # Check if replacement would change anything
            current = match.group(0)
            replaced = re.sub(pattern, expected_replacement, current)

            if current != replaced:
                print(f"MISMATCH: {file_path} - {version_key}")
                # Show a shortened version for readability
                current_short = current.replace('\n', '\\n')[:80]
                replaced_short = replaced.replace('\n', '\\n')[:80]
                print(f"  Found:    {current_short}...")
                print(f"  Expected: {replaced_short}...")
                all_match = False

    if all_match:
        print("All versions are in sync.")
    return all_match


def update_versions(root: Path, versions: dict[str, str]) -> None:
    """Update all version occurrences in the codebase."""
    updated_files: set[str] = set()

    for file_path, version_key, pattern, replacement_template, uses_short in VERSION_LOCATIONS:
        full_path = root / file_path
        if not full_path.exists():
            print(f"WARNING: {file_path} not found")
            continue

        content = full_path.read_text()
        version = get_version_value(versions, version_key, uses_short)
        expected_replacement = replacement_template.format(version=version)

        new_content, count = re.subn(pattern, expected_replacement, content)

        if count == 0:
            print(f"WARNING: {file_path} - pattern for {version_key} not found")
            print(f"  Pattern: {pattern}")
        elif new_content != content:
            full_path.write_text(new_content)
            updated_files.add(file_path)
            print(f"UPDATED: {file_path} - {version_key} ({count} occurrence(s))")

    if updated_files:
        print(f"\nUpdated {len(updated_files)} file(s).")
    else:
        print("All versions were already in sync.")


def main() -> int:
    if len(sys.argv) != 2 or sys.argv[1] not in ("check", "update"):
        print(__doc__)
        print("Error: Please specify 'check' or 'update'")
        return 1

    command = sys.argv[1]

    # Find repository root (where versions.json is)
    script_dir = Path(__file__).parent
    root = script_dir.parent
    versions_path = script_dir / "versions.json"

    if not versions_path.exists():
        print(f"Error: {versions_path} not found")
        return 1

    versions = load_versions(versions_path)

    if command == "check":
        return 0 if check_versions(root, versions) else 1
    else:
        update_versions(root, versions)
        return 0


if __name__ == "__main__":
    sys.exit(main())
