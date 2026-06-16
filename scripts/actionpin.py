#!/usr/bin/env python3
"""
GitHub Actions pinning script for the codebase.

This script pins every external GitHub Action to an immutable commit SHA, while
recording the tracked major version as a trailing annotation comment on the
`uses:` line:

    uses: actions/checkout@df4cb1c069e1874edd31b4311f1884172cec0e10 #[actionpin: v6]

The annotation grammar is `#[actionpin: vMAJOR]`. The action path and pinned SHA
are read from the `uses:` line itself.

Commands:
- renew-pins     Repin each action to the latest release within its current major.
                 Self-bootstrapping: an un-annotated `uses: org/repo@vN` line is
                 pinned and annotated on first run. Supports --dry-run.
- upgrade-versions Bump each action's major version if a newer major release
                 exists, then pin to the latest release in that new major.
                 Supports --dry-run.

Remote lookups use the `gh` CLI (authenticated), so both commands require `gh`
to be installed and logged in.
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

from soteria_utils import *

# Directory scanned (recursively) for workflow / action YAML files
SCAN_DIR = ".github"

# Regex to match the trailing annotation on a `uses:` line: #[actionpin: vMAJOR]
TAG_PATTERN = re.compile(r"#\s*\[actionpin:\s*(v\d+)\s*\]")

# Regex to match a `uses:` line; captures (indent, path, ref). Handles both
# "uses: x@ref" and "- uses: x@ref". The ref capture stops at whitespace, so a
# trailing annotation comment is excluded.
USES_PATTERN = re.compile(r"^(\s*)(?:-\s*)?uses:\s*([^@\s]+)@(\S+)")

# Regex to recognise a clean version tag (vX, vX.Y, vX.Y.Z), no pre-release suffix
VERSION_TAG_PATTERN = re.compile(r"^v(\d+)(?:\.(\d+))?(?:\.(\d+))?$")


def run_command(cmd: list[str]) -> str:
    """Run a command and return its stdout. Exits the program on failure."""
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != 0:
            error(f"Command failed: {' '.join(cmd)}")
            error(f"Error output: {result.stderr.strip()}")
            sys.exit(1)
        return result.stdout.strip()
    except FileNotFoundError:
        error(f"Command not found: {cmd[0]}")
        if cmd[0] == "gh":
            info("Install the GitHub CLI (https://cli.github.com/) and run `gh auth login`.")
        sys.exit(1)


def is_external(path: str) -> bool:
    """A `uses:` path is external if it is not a local (./) action reference."""
    return not path.startswith("./")


def repo_of(path: str) -> str:
    """Return the org/repo of an action path, stripping any subdir.

    e.g. actions/cache/restore -> actions/cache
    """
    return "/".join(path.split("/")[:2])


def parse_version(tag: str) -> tuple[int, int, int] | None:
    """Parse a clean version tag into a (major, minor, patch) tuple, or None."""
    match = VERSION_TAG_PATTERN.match(tag)
    if not match:
        return None
    major = int(match.group(1))
    minor = int(match.group(2)) if match.group(2) else 0
    patch = int(match.group(3)) if match.group(3) else 0
    return major, minor, patch


# Per-run cache of repo -> list of (version_tuple, tag_name)
_tags_cache: dict[str, list[tuple[tuple[int, int, int], str]]] = {}
# Per-run cache of (repo, tag) -> commit SHA
_sha_cache: dict[tuple[str, str], str] = {}


def get_version_tags(repo: str) -> list[tuple[tuple[int, int, int], str]]:
    """Return all clean version tags of a repo as (version_tuple, tag_name)."""
    if repo not in _tags_cache:
        out = run_command(
            ["gh", "api", "--paginate", f"repos/{repo}/tags", "--jq", ".[].name"]
        )
        tags = []
        for name in out.splitlines():
            name = name.strip()
            version = parse_version(name)
            if version is not None:
                tags.append((version, name))
        _tags_cache[repo] = tags
    return _tags_cache[repo]


def latest_in_major(repo: str, major: int) -> tuple[tuple[int, int, int], str] | None:
    """Return the (version_tuple, tag_name) of the latest release in a major."""
    candidates = [t for t in get_version_tags(repo) if t[0][0] == major]
    if not candidates:
        return None
    return max(candidates, key=lambda t: t[0])


def max_major(repo: str) -> int | None:
    """Return the highest major version available for a repo."""
    tags = get_version_tags(repo)
    if not tags:
        return None
    return max(t[0][0] for t in tags)


def resolve_sha(repo: str, tag: str) -> str:
    """Resolve a tag to its underlying commit SHA (dereferences annotated tags)."""
    key = (repo, tag)
    if key not in _sha_cache:
        _sha_cache[key] = run_command(
            ["gh", "api", f"repos/{repo}/commits/{tag}", "--jq", ".sha"]
        )
    return _sha_cache[key]


def find_yaml_files(root: Path) -> list[Path]:
    """Return all YAML files under the scan directory, sorted."""
    scan_root = root / SCAN_DIR
    files = sorted(scan_root.rglob("*.yml")) + sorted(scan_root.rglob("*.yaml"))
    return sorted(set(files))


def process_file(
    file_path: Path,
    file_rel: Path,
    upgrade: bool,
    dry_run: bool,
    progress: Progress,
) -> bool:
    """Renew (or upgrade) pins in a single file.

    Returns True if the file was modified (or would be, in dry-run mode).
    """
    lines = file_path.read_text().splitlines(keepends=True)
    changed = False

    for i, line in enumerate(lines):
        uses_match = USES_PATTERN.match(line)
        if not uses_match:
            continue
        _, path, ref = uses_match.groups()
        if not is_external(path):
            continue

        progress.advance()
        repo = repo_of(path)

        # Determine the current major being tracked.
        tag_match = TAG_PATTERN.search(line)
        if tag_match is not None:
            current_major = int(tag_match.group(1)[1:])
            current_sha = ref
        else:
            # No annotation yet: derive the major from the `@vN` ref (bootstrap).
            version = parse_version(ref)
            if version is None:
                warn(
                    f"{file_rel}:{i + 1} - {path}@{ref} is not annotated and its ref "
                    f"is not a version tag; skipping (pin it manually first)."
                )
                continue
            current_major = version[0]
            current_sha = ref

        # Pick the target major.
        if upgrade:
            top = max_major(repo)
            if top is None:
                warn(f"{file_rel}:{i + 1} - no version tags found for {repo}; skipping.")
                continue
            target_major = top
        else:
            target_major = current_major

        latest = latest_in_major(repo, target_major)
        if latest is None:
            warn(
                f"{file_rel}:{i + 1} - no v{target_major}.x release found for {repo}; "
                f"skipping."
            )
            continue
        (_, latest_tag) = latest
        new_sha = resolve_sha(repo, latest_tag)

        major_changed = target_major != current_major
        sha_changed = new_sha != current_sha

        if not major_changed and not sha_changed:
            continue

        action_kind = "UPGRADE" if major_changed else "RENEW"
        detail = (
            f"{path}: v{current_major} ({current_sha[:8]}) -> "
            f"v{target_major} {latest_tag} ({new_sha[:8]})"
        )

        if dry_run:
            info(f"{action_kind} {file_rel}:{i + 1} - {detail}")
            changed = True
            continue

        # Apply: rewrite the ref to the new SHA and (re)set the trailing annotation.
        base = TAG_PATTERN.sub("", line).rstrip()
        base = base.replace(f"@{ref}", f"@{new_sha}", 1)
        lines[i] = f"{base} #[actionpin: v{target_major}]\n"

        info(f"{action_kind} {file_rel}:{i + 1} - {detail}")
        changed = True

    if changed and not dry_run:
        file_path.write_text("".join(lines))

    return changed


def count_external_uses(files: list[Path]) -> int:
    """Count external `uses:` lines across the given files (progress bar total)."""
    total = 0
    for file_path in files:
        for line in file_path.read_text().splitlines():
            match = USES_PATTERN.match(line)
            if match and is_external(match.group(2)):
                total += 1
    return total


def cmd_pins(root: Path, upgrade: bool, dry_run: bool) -> int:
    """Renew or upgrade pins across all scanned files."""
    files = find_yaml_files(root)
    label = "Upgrading versions" if upgrade else "Renewing pins"

    any_change = False
    with Progress(label, count_external_uses(files)) as progress:
        for file_path in files:
            file_rel = file_path.relative_to(root)
            if process_file(file_path, file_rel, upgrade, dry_run, progress):
                any_change = True

    if not any_change:
        success("All pins are already up to date.")
    elif dry_run:
        info("Dry run: no files were modified.")
    else:
        success("Pins updated.")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="GitHub Actions SHA-pinning script for Soteria",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s renew-pins              Repin to the latest release within each major
  %(prog)s renew-pins --dry-run    Show what renewing would change
  %(prog)s upgrade-versions        Bump majors where a newer major exists
  %(prog)s upgrade-versions --dry-run  Show available major upgrades

Annotation format (trailing comment on the `uses:` line):
  uses: actions/checkout@<sha> #[actionpin: v6]
""",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    renew_parser = subparsers.add_parser(
        "renew-pins", help="Repin to the latest release within each current major"
    )
    renew_parser.add_argument(
        "--dry-run", action="store_true", help="Show changes without writing files"
    )

    upgrade_parser = subparsers.add_parser(
        "upgrade-versions", help="Bump majors when a newer major release exists"
    )
    upgrade_parser.add_argument(
        "--dry-run", action="store_true", help="Show changes without writing files"
    )

    args = parser.parse_args()

    root = Path(__file__).parent.parent

    if args.command == "renew-pins":
        return cmd_pins(root, upgrade=False, dry_run=args.dry_run)
    elif args.command == "upgrade-versions":
        return cmd_pins(root, upgrade=True, dry_run=args.dry_run)

    return 1


if __name__ == "__main__":
    sys.exit(main())
