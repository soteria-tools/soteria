"""Utility functions for Soteria C scripts."""

import subprocess
import sys
from abc import ABC, abstractmethod
from collections import Counter
from pathlib import Path
from typing import Any

# Make the shared utilities importable regardless of where this script is run
# from: walk up to the repository root and add its `scripts/` directory to the
# path, then re-export everything (colours, message levels, ...) so
# `from utils import *` keeps providing them.
_dir = Path(__file__).resolve().parent
while not (_dir / "scripts" / "soteria_utils.py").exists():
    if _dir.parent == _dir:
        raise RuntimeError("could not locate scripts/soteria_utils.py")
    _dir = _dir.parent
sys.path.insert(0, str(_dir / "scripts"))

from soteria_utils import *  # noqa: E402,F401,F403


class PrintersMixin(ABC):
    @abstractmethod
    def print_prefix(self) -> str:
        pass

    def print_message(self, message: str):
        print(f"{self.print_prefix()}{message}")

    def print_info(self, message: str):
        self.print_message(fmt_info(message))

    def print_warning(self, message: str):
        self.print_message(fmt_warning(message))

    def print_error(self, message: str):
        self.print_message(fmt_error(message))

    def print_success(self, message: str):
        self.print_message(fmt_success(message))

    def run_command(self, cmd: str):
        self.print_message(f"{MAGENTA}Running: {cmd}{RESET}")
        subprocess.run(cmd, shell=True)


class GlobalPrinter(PrintersMixin):
    def print_prefix(self) -> str:
        return f"{BOLD}{CYAN}Global{RESET} - "


global_printer = GlobalPrinter()


def normalize_stats(data: dict[str, Any]) -> dict[str, Any]:
    """
    Normalize a stats dictionary by:
    1. Removing the 'soteria-c.give-up-reasons' key entirely
    2. Converting 'soteria.give-up-reasons' from a list to a count dictionary

    Args:
        data: Raw statistics dictionary

    Returns:
        Normalized statistics dictionary
    """
    result = {}

    for key, value in data.items():
        # Remove soteria-c.give-up-reasons entirely
        if key == "soteria-c.give-up-reasons":
            continue

        # Convert soteria.give-up-reasons from list to count dict
        if key == "soteria.give-up-reasons" and isinstance(value, list):
            result[key] = dict(Counter(value))
        else:
            result[key] = value

    return result


def merge_entry(entry1: Any, entry2: Any) -> Any:
    """
    Merge two stat entries based on their types, following the OCaml logic:
    - Int: addition
    - Float: addition
    - List (StrSeq): concatenation
    - Dict (Map): recursive merge
    - Other: create list of both values

    Args:
        entry1: First entry
        entry2: Second entry

    Returns:
        Merged entry

    Raises:
        ValueError: If entries are incompatible types
    """
    # Both are ints
    if isinstance(entry1, int) and isinstance(entry2, int):
        return entry1 + entry2

    # Both are floats
    if isinstance(entry1, float) and isinstance(entry2, float):
        return entry1 + entry2

    # Both are lists (StrSeq)
    if isinstance(entry1, list) and isinstance(entry2, list):
        return entry1 + entry2

    # Both are dicts (Map) - merge recursively
    if isinstance(entry1, dict) and isinstance(entry2, dict):
        return merge_stats(entry1, entry2)

    # Incompatible types - create a list containing both
    if isinstance(entry1, list) and not isinstance(entry2, list):
        return entry1 + [entry2]
    if not isinstance(entry1, list) and isinstance(entry2, list):
        return [entry1] + entry2

    # Neither is a list - create a new list
    return [entry1, entry2]


def merge_stats(stats1: dict[str, Any], stats2: dict[str, Any]) -> dict[str, Any]:
    """
    Merge two statistics dictionaries generically based on entry types.

    Args:
        stats1: First statistics dictionary
        stats2: Second statistics dictionary

    Returns:
        Merged statistics dictionary
    """
    result = {}

    # Get all keys from both dictionaries
    all_keys = set(stats1.keys()) | set(stats2.keys())

    for key in all_keys:
        if key in stats1 and key in stats2:
            # Both have this key - merge the entries
            result[key] = merge_entry(stats1[key], stats2[key])
        elif key in stats1:
            # Only in stats1
            result[key] = stats1[key]
        else:
            # Only in stats2
            result[key] = stats2[key]

    return result
