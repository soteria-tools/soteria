#!/usr/bin/env python3
"""
Version synchronization script for the codebase.

This script manages version strings across the codebase to ensure consistency.
Versions are defined in scripts/versions.json and embedded in tags throughout
the codebase in the format: [versionsync: NAME=value]

The script performs three-way checking:
1. Config value (versions.json) - the source of truth
2. Tag value - embedded in comments like [versionsync: OCAML_VERSION=5.4.0]
3. File value - the actual value on the line(s) following the tag

Filters can be applied to transform values:
- [versionsync.slice: a..b] - slice the value from index a to b (Python-style)
  Examples: 0..3 (first 3 chars), 0..-1 (all but last char), ..-2 (all but last 2)
"""

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

# Files to scan for versionsync tags
FILES_TO_SCAN = [
    ".github/workflows/ci.yml",
    "soteria-rust.opam.template",
    "dune-project",
    ".ocamlformat",
    "Makefile",
    "README.md",
]

# How many lines after the tag to search for the value
LINES_TO_SEARCH = 5

# Regex to match versionsync tags: [versionsync: NAME=value]
TAG_PATTERN = re.compile(r"\[versionsync:\s*(\w+)=([^\]]+)\]")

# Regex to match slice filter: [versionsync.slice: a..b]
SLICE_PATTERN = re.compile(r"\[versionsync\.slice:\s*(-?\d*)\.\.(-?\d*)\]")


# ANSI color codes for pretty output
class Colors:
    RED = "\033[91m"
    GREEN = "\033[92m"
    BLUE = "\033[94m"
    YELLOW = "\033[93m"
    BOLD = "\033[1m"
    RESET = "\033[0m"


def color_print(message: str, color: str = "") -> None:
    """Print a message with optional color."""
    print(f"{color}{message}{Colors.RESET}")


def info(message: str) -> None:
    """Print an info message in blue."""
    color_print(f"ℹ {message}", Colors.BLUE)


def success(message: str) -> None:
    """Print a success message in green."""
    color_print(f"✓ {message}", Colors.GREEN)


def error(message: str) -> None:
    """Print an error message in red."""
    color_print(f"✗ {message}", Colors.RED)


def step(message: str) -> None:
    """Print a step message in bold."""
    color_print(f"▶ {message}", Colors.BOLD)


def parse_slice(slice_str: str) -> tuple[int | None, int | None]:
    """Parse a slice specification like '0..3', '..-1', '2..'."""
    match = re.match(r"(-?\d*)\.\.(-?\d*)", slice_str)
    if not match:
        return None, None
    start_str, end_str = match.groups()
    start = int(start_str) if start_str else None
    end = int(end_str) if end_str else None
    return start, end


def apply_slice(value: str, start: int | None, end: int | None) -> str:
    """Apply a slice to a string value."""
    return value[start:end]


def find_filters_on_line(line: str) -> list[tuple[str, tuple[int | None, int | None]]]:
    """Find all filters on a line. Returns list of (filter_type, params)."""
    filters = []
    for match in SLICE_PATTERN.finditer(line):
        start_str, end_str = match.groups()
        start = int(start_str) if start_str else None
        end = int(end_str) if end_str else None
        filters.append(("slice", (start, end)))
    return filters


def apply_filters(
    value: str, filters: list[tuple[str, tuple[int | None, int | None]]]
) -> str:
    """Apply all filters to a value."""
    result = value
    for filter_type, params in filters:
        if filter_type == "slice":
            start, end = params
            result = apply_slice(result, start, end)
    return result


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


def find_tags_in_file(
    content: str,
) -> list[
    tuple[int, int, str, str, str, list[tuple[str, tuple[int | None, int | None]]]]
]:
    """
    Find all versionsync tags in file content.
    Returns list of (line_number, column, name, tag_value, full_line, filters) tuples.
    """
    tags = []
    for i, line in enumerate(content.splitlines()):
        for match in TAG_PATTERN.finditer(line):
            filters = find_filters_on_line(line)
            tags.append(
                (i, match.start(), match.group(1), match.group(2), line, filters)
            )
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
    # prepare tag line boundaries so we don't search past the next tag
    tag_lines = sorted({t[0] for t in tags})

    for line_num, _, name, tag_value, _, filters in tags:
        # Check 1: Does the version exist in config?
        if name not in versions:
            messages.append(
                f"WARNING: {file_rel}:{line_num + 1} - Unknown version '{name}' in tag"
            )
            all_ok = False
            continue

        # Apply filters to get the expected value
        config_value = versions[name]
        expected_value = apply_filters(config_value, filters)

        # Check 2: Does the tag value match the expected value?
        if tag_value != expected_value:
            messages.append(
                f"MISMATCH: {file_rel}:{line_num + 1} - {name}\n"
                f"  Tag has:    '{tag_value}'\n"
                f"  Expected:   '{expected_value}' (from config '{config_value}')"
            )
            all_ok = False
            continue

        # Check 3: Does the tag value appear in the following lines?
        found_in_file = False
        # determine next tag line (if any) to avoid crossing tag boundaries
        next_tag_line = next((ln for ln in tag_lines if ln > line_num), len(lines))
        search_end = min(line_num + 1 + LINES_TO_SEARCH, next_tag_line)
        for j in range(line_num + 1, search_end):
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

    tags = find_tags_in_file(content)
    tag_lines = sorted({t[0] for t in tags})

    for line_num, col, name, old_tag_value, full_line, filters in tags:
        if name not in versions:
            messages.append(
                f"WARNING: {file_rel}:{line_num + 1} - Unknown version '{name}', skipping"
            )
            continue

        # Apply filters to get the new expected value
        config_value = versions[name]
        new_tag_value = apply_filters(config_value, filters)

        if old_tag_value == new_tag_value:
            continue  # Already in sync

        # Update the tag itself
        old_tag = f"[versionsync: {name}={old_tag_value}]"
        new_tag = f"[versionsync: {name}={new_tag_value}]"

        # Find the tag line and update it
        tag_line = lines[line_num]
        if old_tag in tag_line:
            lines[line_num] = tag_line.replace(old_tag, new_tag, 1)
            modified = True

        # Update the value in following lines, but don't cross into next tag
        next_tag_line = next((ln for ln in tag_lines if ln > line_num), len(lines))
        search_end = min(line_num + 1 + LINES_TO_SEARCH, next_tag_line)
        for j in range(line_num + 1, search_end):
            if old_tag_value in lines[j]:
                lines[j] = lines[j].replace(old_tag_value, new_tag_value, 1)
                modified = True
                messages.append(f"UPDATED: {file_rel}:{j + 1} - {name}")
                break  # Only update the first occurrence

    if modified:
        file_path.write_text("".join(lines))

    return modified, messages


def cmd_list(args: argparse.Namespace, versions: dict[str, str]) -> int:
    """List all configured versions."""
    print("Configured versions (from versions.json):")
    for name, value in sorted(versions.items()):
        print(f"  {name}: {value}")
    return 0


def cmd_check(args: argparse.Namespace, root: Path, versions: dict[str, str]) -> int:
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


def cmd_update(args: argparse.Namespace, root: Path, versions: dict[str, str]) -> int:
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
    args: argparse.Namespace, versions_path: Path, root: Path, versions: dict[str, str]
) -> int:
    """Set a version and update all occurrences."""
    name = args.name
    value = args.value

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

    return cmd_update(args, root, versions)


def run_command(cmd: list[str], cwd: Path) -> tuple[bool, str]:
    """
    Run a command and return (success, output).
    Exits the program on failure.
    """
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd,
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != 0:
            error(f"Command failed: {' '.join(cmd)}")
            error(f"Error output: {result.stderr.strip()}")
            sys.exit(1)
        return True, result.stdout.strip()
    except Exception as e:
        error(f"Failed to run command: {' '.join(cmd)}")
        error(f"Exception: {str(e)}")
        sys.exit(1)


def validate_git_repo(target_dir: Path, project: str, expected_repo: str) -> str:
    """
    Validate that the directory exists, is a git repo, and has the correct remote.
    Returns the name of the remote to use.
    Exits the program if validation fails.
    """
    # Check if directory exists
    if not target_dir.exists():
        error(f"Directory does not exist: {target_dir}")
        info(f"Please clone {project} first:")
        info(f"  git clone https://github.com/{expected_repo}.git {target_dir}")
        sys.exit(1)

    # Check if it's a git repository
    git_dir = target_dir / ".git"
    if not git_dir.exists():
        error(f"Directory is not a git repository: {target_dir}")
        info(f"Please clone {project} first:")
        info(f"  git clone https://github.com/{expected_repo}.git {target_dir}")
        sys.exit(1)

    # Get all remotes
    result = subprocess.run(
        ["git", "remote", "-v"],
        cwd=target_dir,
        capture_output=True,
        text=True,
        check=False,
    )
    
    if result.returncode != 0:
        error(f"Failed to list remotes for {project}")
        error(f"Error: {result.stderr.strip()}")
        sys.exit(1)
    
    # Parse remotes output: "remote_name\turl (fetch)"
    remotes = {}
    for line in result.stdout.strip().split("\n"):
        if not line:
            continue
        parts = line.split()
        if len(parts) >= 2:
            remote_name = parts[0]
            remote_url = parts[1]
            # Normalize remote URLs (handle both https and git@ formats)
            normalized = remote_url.replace("https://github.com/", "").replace("git@github.com:", "").replace(".git", "")
            remotes[remote_name] = normalized
    
    normalized_expected = expected_repo.replace(".git", "")
    
    # Check if any remote matches the expected repo
    for remote_name, normalized_url in remotes.items():
        if normalized_url == normalized_expected:
            info(f"Found matching remote '{remote_name}' for {expected_repo}")
            return remote_name
    
    # No matching remote found - add one
    # Use a safe remote name based on the repo (e.g., 'soteria-obol')
    org_name = expected_repo.split('/')[0]
    repo_name = expected_repo.split('/')[1]
    new_remote_name = f"{org_name}-{repo_name}".replace("_", "-")
    
    info(f"No remote found for {expected_repo}, adding remote '{new_remote_name}'")
    run_command(
        ["git", "remote", "add", new_remote_name, f"https://github.com/{expected_repo}.git"],
        target_dir
    )
    success(f"Added remote '{new_remote_name}' -> https://github.com/{expected_repo}.git")
    return new_remote_name


def pull_project(project: str, target_dir: Path, commit_hash: str, repo: str, build_cmd: str) -> None:
    """
    Pull and build a single project.
    
    Args:
        project: Project name (obol or charon)
        target_dir: Directory where the project is located
        commit_hash: Commit hash to checkout
        repo: Expected repository (e.g., 'soteria-tools/obol')
        build_cmd: Build command to run
    """
    step(f"Processing {project}...")
    
    # Validate the repository and get the remote name to use
    remote_name = validate_git_repo(target_dir, project, repo)
    success(f"Validated {project} repository at {target_dir}")
    
    # Fetch from the specific remote
    info(f"Running git fetch {remote_name}...")
    run_command(["git", "fetch", remote_name], target_dir)
    success(f"Fetched from {remote_name}")
    
    # Force checkout the target commit
    info(f"Checking out commit {commit_hash[:8]}...")
    run_command(["git", "checkout", "-f", commit_hash], target_dir)
    success(f"Checked out {commit_hash[:8]}")
    
    # Run the build command
    info(f"Running build command: {build_cmd}")
    build_parts = build_cmd.split()
    run_command(build_parts, target_dir)
    success(f"Build completed successfully")
    
    color_print(f"\n✓ {project} updated and built successfully!\n", Colors.GREEN + Colors.BOLD)


def cmd_pull(args: argparse.Namespace, root: Path, versions: dict[str, str]) -> int:
    """Pull and build obol and/or charon."""
    projects_config = {
        "obol": {
            "repo_key": "OBOL_REPO",
            "version_key": "OBOL_COMMIT_HASH",
            "default_dir": root.parent / "obol",
            "build_cmd": "make build",
        },
        "charon": {
            "repo_key": "CHARON_REPO",
            "version_key": "CHARON_COMMIT_HASH",
            "default_dir": root.parent / "charon",
            "build_cmd": "make build-charon-rust",
        },
    }
    
    # Determine which projects to pull
    projects = []
    if args.project == "all":
        projects = ["obol", "charon"]
    else:
        projects = [args.project]
    
    # Process each project
    for project in projects:
        config = projects_config[project]
        
        # Check if required keys exist in versions
        if config["repo_key"] not in versions:
            error(f"Missing {config['repo_key']} in versions.json")
            return 1
        if config["version_key"] not in versions:
            error(f"Missing {config['version_key']} in versions.json")
            return 1
        
        # Determine target directory
        if args.dir and len(projects) == 1:
            # Only use custom dir if pulling a single project
            target_dir = Path(args.dir).expanduser().resolve()
        else:
            target_dir = config["default_dir"]
        
        repo = versions[config["repo_key"]]
        commit_hash = versions[config["version_key"]]
        
        try:
            pull_project(project, target_dir, commit_hash, repo, config["build_cmd"])
        except SystemExit:
            # Error already printed
            return 1
    
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Version synchronization script for Soteria",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s list                    List all configured versions
  %(prog)s check                   Check if all versions are in sync
  %(prog)s update                  Update all version occurrences
  %(prog)s set OCAML_VERSION 5.5.0 Set a version and update everywhere
  %(prog)s pull obol               Pull and build obol from configured commit
  %(prog)s pull charon             Pull and build charon from configured commit
  %(prog)s pull all                Pull and build both obol and charon
  %(prog)s pull obol --dir ~/code/obol  Pull obol from custom directory

Filters:
  Tags can include filters to transform values:
  [versionsync: NAME=value] [versionsync.slice: 0..3]

  Slice examples:
    0..3   - first 3 characters
    0..-1  - all but last character
    ..-2   - all but last 2 characters
    2..    - from index 2 to end
""",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    # list command
    subparsers.add_parser("list", help="List all configured versions")

    # check command
    subparsers.add_parser("check", help="Check if all versions are in sync")

    # update command
    subparsers.add_parser("update", help="Update all version occurrences")

    # set command
    set_parser = subparsers.add_parser(
        "set", help="Set a version and update all occurrences"
    )
    set_parser.add_argument("name", metavar="NAME", help="Version name to set")
    set_parser.add_argument("value", metavar="VALUE", help="New version value")

    # pull command
    pull_parser = subparsers.add_parser(
        "pull", help="Pull and build obol and/or charon from configured commits"
    )
    pull_parser.add_argument(
        "project",
        choices=["obol", "charon", "all"],
        help="Project to pull (obol, charon, or all)",
    )
    pull_parser.add_argument(
        "--dir",
        help="Override default directory (only for single project pulls)",
    )

    args = parser.parse_args()

    # Find repository root
    script_dir = Path(__file__).parent
    root = script_dir.parent
    versions_path = script_dir / "versions.json"

    if not versions_path.exists():
        print(f"Error: {versions_path} not found")
        return 1

    versions = load_versions(versions_path)

    if args.command == "list":
        return cmd_list(args, versions)
    elif args.command == "check":
        return cmd_check(args, root, versions)
    elif args.command == "update":
        return cmd_update(args, root, versions)
    elif args.command == "set":
        return cmd_set(args, versions_path, root, versions)
    elif args.command == "pull":
        return cmd_pull(args, root, versions)

    return 1


if __name__ == "__main__":
    sys.exit(main())
